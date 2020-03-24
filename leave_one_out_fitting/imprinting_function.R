library(dplyr)

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


imprinting <- function(birth_year,
                       current_flu_season,
                       attack_rate){
    
    
    scaled_attack_rates = get_scaled_attack_rates(birth_year, attack_rate)
    
    # Get seasons to iterate based on attack rates
    seasons = birth_year:current_flu_season
    if (birth_year <= 2009){
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
    
    
    # Set up final dataframe
    imprinting_df = data.frame()
    imprinting_df[as.character(birth_year), 'prob_first_infection'] = 0

    # Calculate probability of first infection for each season
    current_index = 1
    prob_naive = 1.0
    for (season in seasons){
        prob_infection = scaled_attack_rates[as.character(season), 'scaled_attack_rate']
        if (current_index + 1 <= length(seasons)){
            imprinting_df[as.character(seasons[current_index + 1]), 'prob_first_infection'] = prob_naive  * prob_infection
        }
        prob_naive = prob_naive * (1-prob_infection)
        current_index = current_index + 1
    }

    # Merge in subtype information
    imprinting_df = cbind(imprinting_df, subtype_fractions)
    
    # Calculate subtype probabilities
    imprinting_df$H1 = cumsum(imprinting_df$prob_first_infection * imprinting_df$H1N1_fraction)
    imprinting_df$H2 = cumsum(imprinting_df$prob_first_infection * imprinting_df$H2N2_fraction)
    imprinting_df$H3 = cumsum(imprinting_df$prob_first_infection * imprinting_df$H3N2_fraction)
    imprinting_df$season_float = rownames(imprinting_df)
    imprinting_df$Birth_year = birth_year

    return(imprinting_df)
}

calc_imprinting_all <- function(attack_rate){
    output = data.frame()
    if (exclude_year == 2018){
        lastyear = 2016
    }
    else{
        lastyear = 2017
    }

    for (b in 1918:lastyear){
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
                imps = temp[key, c('H1', 'H2', 'H3')]
            }
            else{
                print(temp)
                imps = temp[as.character(max(temp$season_float)), c('H1', 'H2', 'H3')]
            }
            final[key, 'H1'] = imps$H1
            final[key, 'H2'] = imps$H2
            final[key, 'H3'] = imps$H3
            
        }
        final$Birth_year = b
        names(final) = c('Season', 'H1', 'H2', 'H3', 'Birth_year')
        output = rbind(output, final)
    }
    return(output)    
}

convert_to_age <- function(imprinting_df,
                           demography){
    final = data.frame()
    for (age in 0:100){
        fractions = demography %>% filter(Age==age,
                                          Season %in% all_seasons,
                                          y1 >= 1918,
                                          y2 >= 1918)
        if (nrow(fractions) > 0){
            for (i in 1:nrow(fractions)){
                season = fractions[i, 'Season']
                y1 = fractions[i, 'y1']
                y2 = fractions[i, 'y2']
                f1 = fractions[i, 'f1']
                f2 = fractions[i, 'f2']
                
                temp_imp1 = imprinting_df %>% 
                                filter(Season==season, Birth_year==y1) %>% 
                                select(H1, H2, H3)
                temp_imp2 = imprinting_df %>% 
                                filter(Season==season, Birth_year==y2) %>% 
                                select(H1, H2, H3)
                temp_imp = f1 * temp_imp1 + f2 * temp_imp2
                temp_imp$Age = age
                temp_imp$Season = season
                final = rbind(final, temp_imp)
            }
        }
    }
    return(final)
}

convert_to_squareform <- function(age_imprinting,
                                  demography){
    h1_final = h2_final = h3_final = data.frame(matrix(nrow=length(all_seasons), ncol=length(group_names)))
    row.names(h1_final) = row.names(h2_final) = row.names(h3_final) = all_seasons
    names(h1_final) = names(h2_final) = names(h3_final) = group_names
    
    for (i in 1:nrow(age_groups)){
        lb = age_groups[i, 'LB']
        ub = age_groups[i, 'UB']
        colname = paste(lb, ub, sep='-')

        for (season in all_seasons){
            filtered_demo = demography %>% filter(Age >= lb, Age <= ub, Season==season)
            filtered_imprinting = age_imprinting %>% filter(Age >= lb, Age <= ub, Season==season)
            temp = merge(filtered_demo, filtered_imprinting, by='Age')
            temp$fracs = temp$Population / sum(temp$Population)
            temp = temp$fracs * temp

            temp = colSums(temp[c('H1', 'H2', 'H3')])
            h1_final[as.character(season), colname] = temp[['H1']]
            h2_final[as.character(season), colname] = temp[['H2']]
            h3_final[as.character(season), colname] = temp[['H3']]
        }
    }
    return(list(H1=h1_final, H2=h2_final, H3=h3_final))
}