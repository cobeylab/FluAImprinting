library(tidyverse)
library(foreach)
set.seed(1)
source('../fitting_scripts/linear_model_functions.R')
source('../fitting_scripts/imprinting_function.R')

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

attack_rate = -log(1 - attack_prob)

# Set imprinting probabilities
for (trial in 0:1){
  intensity_and_frequency = read.csv(sprintf('../data/sample_ILI/ILI_sample_%s.csv', trial))
  raw_imprinting = calc_imprinting_all(attack_rate)
  raw_imprinting$Trial = trial
  write.csv(raw_imprinting, file=sprintf('../data/sample_ILI/imprinting_probs_%s.csv', trial), row.names=FALSE)
}



