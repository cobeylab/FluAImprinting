library(tidyverse)
library(foreach)
set.seed(1)
source('linear_model_functions.R')
source('imprinting_function.R')

result_folder="../results_0-100/" # Output folder for results
# Create it if it doesn't exist
if (!dir.exists(result_folder)){
    dir.create(result_folder)
}

min_age = 0
max_age = 100
ref_group='20-29'
reference_year = 2017
attack_prob = 0.28 # attack probability for imprinting

inc_raw = read.csv('../data/standard_eligible_observed.csv')
inc_age = read.csv('../data/standard_eligible_observed_by_age.csv')
demo_raw = read.csv('../data/demography_by_birth_year.csv')
vac_raw = read.csv('../raw_data/test-negative_vac_status.csv')
enrollment_data = read.csv('../data/normalized_enrollment.csv')
approachment_data = read.csv('../raw_data/normalized_approachment.csv')
nursing_raw = read.csv('../data/nursing_home_flat.csv')
intensity_and_frequency = read.csv('../data/Intensity_and_Frequency.csv')
scalings = read.csv('../data/intensity_scalings.csv')
demo_fracs = read.csv('../data/demography_by_age_extended.csv')


seasons = all_seasons = c(2008, 2009, 2009.5, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)


# Setting age groups

age_groups <- 
			  rbind(c(0, 4),
                    c(5, 9),
                    c(10, 14),
                    c(15, 19),
                    c(20, 29),
                    c(30, 39),
                    c(40, 49),
                    c(50, 64),
                    c(65, 100))

age_groups_covar <- rbind(c(0, 4),
                          c(5, 9),
                          c(10, 14),
                          c(15, 19),
                          c(20, 29),
                          c(30, 39),
                          c(40, 49),
                          c(50, 64),
                          c(65, 100))

full_age_groups <- #age_groups
				   rbind(c(0, 4),
                         c(5, 9),
                         c(10, 14),
                         c(15, 19),
                         c(20, 29),
                         c(30, 39),
                         c(40, 49),
                         c(50, 64),
                         c(65, 100))

colnames(age_groups) = c("LB","UB")
groupNames = paste(age_groups[,'LB'], age_groups[, 'UB'], sep='-')
rownames(age_groups) = groupNames

colnames(age_groups_covar) = c("LB","UB")
rownames(age_groups_covar) = paste(age_groups_covar[,'LB'], age_groups_covar[, 'UB'], sep='-')

# Mapping age to age group
age_to_age_group = data.frame(age_group=seq(min_age, 100))
rownames(age_to_age_group) = seq(min_age, 100)
for (a in seq(min_age, 100)){
    for (age_group in rownames(age_groups)){
        if (age_groups[age_group, 'LB'] <= a & age_groups[age_group, 'UB'] >= a){
            age_to_age_group[as.character(a), 'age_group'] = age_group
            break
        }
    }
}

# Mapping age to age group
age_to_age_group_covar = data.frame(age_group=seq(min_age, 100))
rownames(age_to_age_group_covar) = seq(min_age, 100)
for (a in seq(min_age, 100)){
    for (age_group in rownames(age_groups_covar)){
        if (age_groups_covar[age_group, 'LB'] <= a & age_groups_covar[age_group, 'UB'] >= a){
            age_to_age_group_covar[as.character(a), 'age_group'] = age_group
            break
        }
    }
}

# setting birth year cohorts for cohort-specific VE models
cohorts <- reference_year - full_age_groups
colnames(cohorts) = c("UB","LB")
cohort_names = paste(cohorts[,'LB'],
                     cohorts[, 'UB'], sep='-')
rownames(cohorts) = cohort_names
birth_year_to_birth_year_group = data.frame(birth_year=seq(min(cohorts), max(cohorts)))
rownames(birth_year_to_birth_year_group) = seq(min(cohorts), max(cohorts))

for (y in seq(min(cohorts), max(cohorts))){
    for (c in rownames(cohorts)){
        if (cohorts[c, 'LB'] <= y & cohorts[c, 'UB'] >= y){
            birth_year_to_birth_year_group[as.character(y), 'birth_year_cohort'] = c
            break
        }
    }
}


# Set vaccine age groups in the case that you choose to test different age groups for VE that are different
# from the age groups for age-specific medically attended flu A infection.
vax_age_groups <- age_groups
colnames(vax_age_groups) = c("LB","UB")
vax_groupNames = paste(vax_age_groups[,'LB'], vax_age_groups[, 'UB'], sep='-')
rownames(vax_age_groups) = vax_groupNames

# Mapping age to vax age group
age_to_vax_age_group = data.frame(age_group=seq(min_age, 100))
rownames(age_to_vax_age_group) = seq(min_age, 100)
for (a in seq(min_age, 100)){
    for (age_group in rownames(vax_age_groups)){
        if (vax_age_groups[age_group, 'LB'] <= a & vax_age_groups[age_group, 'UB'] >= a){
            age_to_vax_age_group[as.character(a), 'age_group'] = age_group
            break
        }
    }
}

# Mapping season to maximum birth year
if (min_age != 0){
    season_to_max_birth_year = demo_fracs %>% 
        group_by(Season) %>% 
        filter(Age==min_age) %>% 
        mutate(max_year=y2) %>%
        select(Season, max_year)
    
} else{
    # Need to fix row labels
    season_to_max_birth_year = inc_raw %>% group_by(season) %>% summarize(max_year=max(birth_year))
    season_to_max_birth_year = as.data.frame(season_to_max_birth_year)
    for (row in rownames(season_to_max_birth_year)){
        s = season_to_max_birth_year[row, 'season']
        if (s == '2009Pan'){
            s = '2009.5'
        } else{
            s = as.character(s)
        }
        season_to_max_birth_year[row, 'Season'] = s
    }
    rownames(season_to_max_birth_year) = season_to_max_birth_year$Season
}

# Mapping season to minimum birth year
season_to_min_birth_year = demo_fracs %>% 
                                        group_by(Season) %>% 
                                        filter(Age==max_age) %>% 
                                        mutate(min_year=y1) %>%
                                        select(Season, min_year)

# Setting the birth year bounds on each season
season_bounds = merge(season_to_max_birth_year, season_to_min_birth_year)
season_bounds = season_bounds %>% filter(Season >= min(seasons),
                                         Season <= max(seasons))

season_bounds = as.data.frame(season_bounds)
rownames(season_bounds) = season_bounds$Season

for (season in rownames(season_bounds)){
    if (season_bounds[season, 'min_year'] < 1918){
        season_bounds[season, 'min_year'] = 1918
    }
}


# Template for matrices
birth_years = seq(min(season_bounds$min_year),
                  max(season_bounds$max_year))
template = data.frame(matrix(ncol=length(birth_years), nrow=length(seasons)))
colnames(template) = birth_years
rownames(template) = seasons

# Set imprinting probabilities
attack_rate = -log(1 - attack_prob)
raw_imprinting = calc_imprinting_all(attack_rate)

# Fill imprinting matrices
m_h1 = m_h2 = m_h3 = m_n = template
for (season in seasons){
    for (birth_year in seq(1918, 2017)){
        if (birth_year <= season_bounds[as.character(season), 'max_year'] &
            birth_year >= season_bounds[as.character(season), 'min_year']){
            imps = raw_imprinting %>% filter(Birth_year == birth_year, Season == season)
            m_h1[as.character(season), as.character(birth_year)] = imps$H1
            m_h2[as.character(season), as.character(birth_year)] = imps$H2
            m_h3[as.character(season), as.character(birth_year)] = imps$H3
            m_n[as.character(season), as.character(birth_year)] = 1 - (imps$H3 + imps$H1 + imps$H2)
        }
    }
}


# Fill up incidence data
# The first season of each birth year is the birth year + 1
# i.e., if you're born in 2010, then your first season is 2010-2011

inc_vac_h1 = inc_unvac_h1 = inc_vac_h3 = inc_unvac_h3 = template

for (season in seasons){
    for (Birth_year in seq(1918, 2017)){
        if (Birth_year <= season_bounds[as.character(season), 'max_year'] &
            Birth_year >= season_bounds[as.character(season), 'min_year']){
            if (season == 2009.5){
                season_test = '2009Pan'
            } else{
                season_test = as.character(season)
            }
            
            incs = inc_raw %>% filter(birth_year == Birth_year, season == season_test)
            inc_vac_h1[as.character(season), as.character(Birth_year)] = incs$I_vac_h1
            inc_vac_h3[as.character(season), as.character(Birth_year)] = incs$I_vac_h3
            inc_unvac_h1[as.character(season), as.character(Birth_year)] = incs$I_obs_h1
            inc_unvac_h3[as.character(season), as.character(Birth_year)] = incs$I_obs_h3
            
        }
    }
}
inc_data = list(h1_unvac=inc_unvac_h1, h1_vac=inc_vac_h1, h3_unvac=inc_unvac_h3, h3_vac=inc_vac_h3)

# Fill up demography data

demo_data = template

for (season in seasons){
    for (birth_year in seq(1918, 2017)){
        if (birth_year <= season_bounds[as.character(season), 'max_year'] &
            birth_year >= season_bounds[as.character(season), 'min_year']){
            season_test = season
            demo = demo_raw %>% filter(Birth_year == birth_year, Season == season_test)
            demo_data[as.character(season), as.character(birth_year)] = demo$Population
        }
    }
}
demo_data = demo_data / rowSums(demo_data, na.rm = TRUE)

# Fill up vacine coverage adjusted for differences in healthcare seeking behavior
vacc_coverage_h1 = vacc_coverage_h3 = template
for (season in seasons){
    season_vac = vac_raw %>% filter(Season==season)
    rownames(season_vac) = season_vac[['age_group']]

    for (birth_year in seq(1918, 2017)){
        if (birth_year <= season_bounds[as.character(season), 'max_year'] &
            birth_year >= season_bounds[as.character(season), 'min_year']){

            season_test = season
            demo = filter(demo_raw, Birth_year == birth_year, Season == season_test)
            a1 = demo$a1
            a2 = demo$a2
            f1 = demo$f1
            f2 = 1 - f1
            
            ag1 = age_to_age_group_covar[as.character(a1), 'age_group']
            ag2 = age_to_age_group_covar[as.character(a2), 'age_group']



            if (season == 2009.5){
                vh1 = vh3 = 0
            } else{
                V_param_1 = season_vac[ag1, 'V_factor']
                if (f1 == 1){
                    V_param_2 = 0
                }else{
                    V_param_2 = season_vac[ag2, 'V_factor']
                }
                vh1 = vh3 = V_param_1 * f1 + V_param_2 * f2
            }

            vacc_coverage_h1[as.character(season), as.character(birth_year)] = vh1
            vacc_coverage_h3[as.character(season), as.character(birth_year)] = vh3
        }
    }
}


# Fill up age-specific covars: Enrollment and approachment
age_specific_unvac = age_specific_vac = template
for (season in seasons){
    season_enrollment = enrollment_data %>% filter(Season==season)
    rownames(season_enrollment) = season_enrollment[['Age.group']]

    season_approachment = approachment_data %>% filter(season_float==season)
    rownames(season_approachment) = season_approachment[['Age.group']]
    
    for (birth_year in seq(1918, 2017)){
        if (birth_year <= season_bounds[as.character(season), 'max_year'] &
            birth_year >= season_bounds[as.character(season), 'min_year']){
                season_test = season
                demo = filter(demo_raw, Birth_year == birth_year, Season == season_test)
                a1 = demo$a1
                a2 = demo$a2
                f1 = demo$f1
                f2 = 1 - f1
                ag1 = age_to_age_group_covar[as.character(a1), 'age_group']
                ag2 = age_to_age_group_covar[as.character(a2), 'age_group']

                A_param_unvac1 = season_enrollment[ag1, 'Unvaccinated']
                A_param_vac1 = season_enrollment[ag1, 'Vaccinated']
                approachment_param1 = season_approachment[ag1, 'Relative.fraction']


    
                if (f1 == 1){
                    A_param_unvac2 = A_param_vac2 = approachment_param2 = 0
                }else{
                    A_param_unvac2 = season_enrollment[ag2, 'Unvaccinated']
                    A_param_vac2 = season_enrollment[ag2, 'Vaccinated']
                    approachment_param2 = season_approachment[ag2, 'Relative.fraction']
                }
                approachment_final = approachment_param1 * f1 + approachment_param2 * f2
                age_specific_unvac[as.character(season), as.character(birth_year)] = (A_param_unvac1 * f1 + A_param_unvac2 * f2) * approachment_final
                age_specific_vac[as.character(season), as.character(birth_year)] = (A_param_vac1 * f1 + A_param_vac2 * f2) * approachment_final
        }
    }
}


# Fill up nursing home data
# The first season of each birth year is the birth year + 1
# i.e., if you're born in 2010, then your first season is 2010-2011
nursing_home_data = template
for (season in seasons){
    for (birth_year in seq(1918, 2017)){
        if (birth_year <= season_bounds[as.character(season), 'max_year'] &
            birth_year >= season_bounds[as.character(season), 'min_year']){
            season_test = season
            demo = demo_raw %>% filter(Birth_year == birth_year, Season == season_test)
            a1 = demo$a1
            a2 = demo$a2
            f1 = demo$f1
            f2 = 1 - f1
            n1 = nursing_raw %>% filter(Season == season_test, Age==a1)
            if (f1 != 1){
                n2 = nursing_raw %>% filter(Season == season_test, Age==a2)
            } else{
                n2 = data.frame(Nursing_home_fraction=0)
            }
            nursing_home_data[as.character(season), as.character(birth_year)] = n1$Nursing_home_fraction * f1 + n2$Nursing_home_fraction * f2
        }
    }
}

p0 = demo_data