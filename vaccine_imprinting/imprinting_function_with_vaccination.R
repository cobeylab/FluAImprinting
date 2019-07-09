library(dplyr)
# define globals
pandemic_data = read.csv('../data/Marshfield_adjusted_exposure_probs.csv') # Weekly exposure probabilities of infection, seasonal vaccination, and pandemic vaccination
vac_birth_year = read.csv('../data/vac_coverage_by_birth_year_seasonal_2010.csv') # Seasonal vaccine coverage by birth year
naive_vac = read.csv('../data/Marshfield_naive_vac_by_birth_year.csv') # Assuming autocorrelation in vaccination, the fraction of people who were naive to vaccination given birth year and season
vac_santinabez = read.csv('../data/santibanez_vc_by_birth_year.csv') # Vaccine coverage among children from santibanez study

vac_scalings = read.csv('../data/vaccination_scalings.csv') # Fraction of people in a given birth year and season who could have been vaccinated
vac_child = vac_birth_year %>% select(Birth_year, Season, coverage)
names(vac_child) = c('Birth_year', 'Season', 'Coverage')
vac_child = rbind(vac_child, vac_santinabez)

get_scaled_attack_rates <- function(birth,
                                    attack_rate){
    
    filtered_scaling = scalings %>% filter(birth_year==birth)
    filtered_scaling = merge(filtered_scaling, intensity_and_frequency, all.x=TRUE)
    filtered_scaling$scaled_attack_rate = filtered_scaling$Intensity * attack_rate * filtered_scaling$frac_exposed
    
    # Converting the attack rate to a probability
    filtered_scaling$scaled_attack_rate = 1 - exp(-filtered_scaling$scaled_attack_rate) 
    
    filtered_scaling = filtered_scaling %>% select(season_float, scaled_attack_rate) 
    row.names(filtered_scaling) = filtered_scaling$season_float
    return(filtered_scaling)
}


get_breakpoint_week <- function(season){
    
    if (season == 2009.5){
        breakpoint = 15
    } else if (season == 2010){
        breakpoint = 47
    } else{
        breakpoint = 40
    }
    return(breakpoint)
}



imprinting <- function(birth_year,
                       current_flu_season,
                       attack_rate){
    
    
    scaled_attack_rates = get_scaled_attack_rates(birth_year, attack_rate)
    filtered_scaling = scalings %>% filter(birth_year==birth_year)
    # Get seasons to iterate based on attack rates
    seasons = birth_year:current_flu_season
    if (birth_year <= 2009 & current_flu_season > 2009){
        seasons = c(seasons, 2009.5)
    }
    seasons = sort(seasons)
    subtype_fractions = intensity_and_frequency %>% 
        filter(season_float %in% seasons) %>%
        select(season_float, H1N1_fraction, H3N2_fraction, H2N2_fraction)
    row.names(subtype_fractions) = subtype_fractions$season_float
    subtype_fractions = subtype_fractions %>% 
        arrange(season_float) %>% 
        select(H1N1_fraction, H3N2_fraction, H2N2_fraction)
    # subtype season fractions have to be shifted up one season
    subtype_fractions = subtype_fractions[-nrow(subtype_fractions), ]
    subtype_fractions = rbind(c(0,0,0), subtype_fractions)

    imprinting_probs = data.frame()
    imprinting_probs[as.character(birth_year), 'Naive'] = 1
    imprinting_probs[as.character(birth_year), 'first_vac'] = imprinting_probs[as.character(birth_year), 'first_infect'] = imprinting_probs[as.character(birth_year), 'first_pan_vac'] = 0
    current_index = 1
    prob_naive=1.0
    naive_to_vac_given_unvac=1.0
    prob_naive_to_vac = 1.0

    for (season in seasons){
        if ((season == 2009.5 | season == 2010) & (birth_year != 2010)){
            naive_to_vac_given_vac_mono = naive_vac %>% filter(vactype=='2009Pan', Birth_year==birth_year) %>% select(Naive_vac)
            naive_to_vac_given_vac_season = naive_vac %>% filter(vactype=='2010', Birth_year==birth_year) %>% select(Naive_vac)
            if (season == 2009.5){
                imprinting_probs['2009.5', 'first_vac'] = 0
            }

            imprinting_probs[as.character(seasons[current_index]), 'Naive'] = prob_naive
            # get probabilities for first infection, monovalent vaccination, and seasonal vaccination
            season_info = pandemic_data %>% filter(Birth_year == birth_year, Season == season)
            prob_infection = season_info$infection
            prob_mono = season_info$monovalent_vaccine
            prob_seasonal = season_info$seasonal_vaccine

            mono_cov = season_info$mono_coverage * naive_to_vac_given_vac_mono
            seasonal_cov = season_info$seasonal_coverage * naive_to_vac_given_vac_season
            frac_vaccinated = mono_cov + seasonal_cov - (mono_cov * seasonal_cov)

            prob_mono = (prob_mono * naive_to_vac_given_vac_mono) / prob_naive_to_vac
            prob_seasonal = (prob_seasonal * naive_to_vac_given_vac_season) / prob_naive_to_vac

            # update prob naive to vac
            prob_naive_to_vac = prob_naive_to_vac * (1 - (frac_vaccinated))

            imprinting_probs[as.character(seasons[current_index+1]), 'first_pan_vac'] = (prob_naive * prob_mono)
            imprinting_probs[as.character(seasons[current_index+1]), 'first_vac'] = (prob_naive * prob_seasonal)
            imprinting_probs[as.character(seasons[current_index+1]), 'first_infect'] = (prob_naive * prob_infection)

            prob_naive = prob_naive * (1-(prob_infection + prob_mono + prob_seasonal))

        } else{
            if (season == birth_year){
                vac = 0
                frac_exposed = 0
                naive_to_vac_given_vac = 1
            } else if(season < 2003){
                vac = 0
                frac_exposed = 0
                naive_to_vac_given_vac  = 1
            }
            else{
                frac_exposed = vac_scalings %>% filter(Birth_year==birth_year, Season==season) %>% select(Frac_exposed_to_vaccination)
                naive_to_vac_given_vac  = naive_vac %>% filter(vactype == as.character(season), Birth_year==birth_year) %>% select(Naive_vac)
                vac = vac_child %>% filter(Season == season, Birth_year==birth_year) %>% select(Coverage)
                
                if (nrow(vac) == 0){
                    vac = 0
                }
                if (nrow(naive_to_vac_given_vac) == 0){
                    naive_to_vac_given_vac  = 0
                }
            }

            # Calculate the probability of being vaccinated GIVEN that you're naive to vaccination
            prob_vaccination = (vac * frac_exposed * naive_to_vac_given_vac) / prob_naive_to_vac
            prob_naive_to_vac = prob_naive_to_vac * (1 - (vac * frac_exposed * naive_to_vac_given_vac))
            # Vaccinate all naive individuals and figure out the new fraction naive to any exposure
            imprinting_probs[as.character(seasons[current_index]), 'first_vac'] = (prob_naive * prob_vaccination)
            prob_naive = prob_naive * (1 - prob_vaccination)


            # This is the probability of being naive at the BEGINNING of a season
            imprinting_probs[as.character(seasons[current_index]), 'Naive'] = prob_naive
            # Then you infect the remaining naive people to get the fraction imprinted by infection at the beginning of the NEXT season
            prob_infection = scaled_attack_rates[as.character(season), 'scaled_attack_rate']

            if (current_index + 1 <= length(seasons)){
                imprinting_probs[as.character(seasons[current_index + 1]), 'first_pan_vac'] = 0
                imprinting_probs[as.character(seasons[current_index + 1]), 'first_infect'] = prob_naive  * prob_infection
            }
            # Final update of naive to any exposure 
            prob_naive = prob_naive * (1 - prob_infection)
        }
        current_index = current_index + 1
    }
    imprinting_probs = cbind(imprinting_probs, subtype_fractions)
    
    imprinting_probs$H1 = cumsum(imprinting_probs$first_infect * imprinting_probs$H1N1_fraction)
    imprinting_probs$H2 = cumsum(imprinting_probs$first_infect * imprinting_probs$H2N2_fraction)
    imprinting_probs$H3 = cumsum(imprinting_probs$first_infect * imprinting_probs$H3N2_fraction)
    imprinting_probs$vac = cumsum(imprinting_probs$first_vac)
    imprinting_probs$panvac = cumsum(imprinting_probs$first_pan_vac)
    imprinting_probs$season_float = rownames(imprinting_probs)
    imprinting_probs$Birth_year = birth_year

    return(imprinting_probs)
}

calc_imprinting_all <- function(attack_rate){
    output = data.frame()
    for (b in 1918:2017){
        temp = imprinting(b, max(all_seasons), attack_rate)
        rownames(temp) = temp$season_float
        final = data.frame(season_float=all_seasons)
        rownames(final) = all_seasons
        
        if (max(temp$season_float) <= min(all_seasons)){
            first_season = min(all_seasons)
        }
        else{
            first_season = max(min(temp$season_float), min(all_seasons))
        }
        
        seasons = first_season:max(all_seasons)
        if (first_season <= 2009){
            seasons = c(seasons, 2009.5)
        }
        seasons = sort(seasons)
        
        for (season in seasons){
            key = as.character(season)
            if (season %in% temp$season_float){
                imps = temp[key, c('H1', 'H2', 'H3', 'vac', 'panvac')]
            }
            else{
                imps = temp[as.character(max(temp$season_float)), c('H1', 'H2', 'H3', 'vac', 'panvac')]
            }
            final[key, 'H1'] = imps$H1
            final[key, 'H2'] = imps$H2
            final[key, 'H3'] = imps$H3
            final[key, 'vac'] = imps$vac
            final[key, 'panvac'] = imps$panvac
        }
        final$Birth_year = b
        names(final) = c('Season', 'H1', 'H2', 'H3', 'vac', 'panvac', 'Birth_year')
        output = rbind(output, final)
    }
    return(output)    
}