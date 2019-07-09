inc_raw = read.csv(sprintf('simulated_data/standard_simulation_%s.csv', random_number_seed))


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