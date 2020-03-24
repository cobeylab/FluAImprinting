calc_cAIC <- function(log_lik, n, K) {
    aic <- -2*log_lik + 2*K + ((2*K*(K+1))/(n-K-1))
    return(aic)
}


model_lik <- function(pp, i) {

    
    ppH1_unvac = pp$ppH1_unvac
    ppH1_vac = pp$ppH1_vac
    
    ppH3_unvac = pp$ppH3_unvac
    ppH3_vac = pp$ppH3_vac
    
    incH1_unvac = inc_data$h1_unvac
    incH1_vac = inc_data$h1_vac
    
    incH3_unvac = inc_data$h3_unvac
    incH3_vac = inc_data$h3_vac

    # Find the log density for each season (row) and take the negative sum

      tot_h1 = sum(incH1_unvac[i, ], na.rm=TRUE) + sum(incH1_vac[i, ], na.rm=TRUE)
      tot_h3 = sum(incH3_unvac[i, ], na.rm=TRUE) + sum(incH3_vac[i, ], na.rm=TRUE)
      lik_h1 = lik_h3 = 0
      # If season is H1-dominated
      if (tot_h1 > tot_h3){
          # Remove columns with unnecessary birth years
          cols = !is.na(incH1_unvac[i, ])
          dmultinom(as.numeric(cbind(incH1_unvac[i, cols], incH1_vac[i, cols])),
                    size = tot_h1,
                    prob = as.numeric(cbind(ppH1_unvac[i, cols], ppH1_vac[i, cols])),
                    log = TRUE) -> lik_h1
          tot = tot_h1
      }
      # If season is H3 dominated
      else{
          # Remove columns with unnecessary birth years
          cols = !is.na(incH3_unvac[i, ])
          dmultinom(as.numeric(cbind(incH3_unvac[i, cols], incH3_vac[i, cols])),
                    size = tot_h3,
                    prob = as.numeric(cbind(ppH3_unvac[i, cols], ppH3_vac[i, cols])),
                    log = TRUE) -> lik_h3
          tot = tot_h3
      }
        
    return(list(loglik=lik_h1 + lik_h3, N=tot)) 
}

get_age_contribution = function(pars, p0, class){
    # Calculate age contribution
    age_contribution = p0 # Copy p0 as a template for the age contribution matrix
    for (season in rownames(p0)){
        for (birth_year in colnames(p0)){
            if (birth_year <= season_bounds[as.character(season), 'max_year'] &
                birth_year >= season_bounds[as.character(season), 'min_year']){
                season_test = season
                demo = filter(demo_raw, Birth_year == birth_year, Season == season_test)
                a1 = demo$a1
                a2 = demo$a2
                f1 = demo$f1
                f2 = 1 - f1
                ag1 = age_to_age_group[as.character(a1), 'age_group']
                ag2 = age_to_age_group[as.character(a2), 'age_group']
                if (ag1 == ref_group){
                    A_param_1 = 1
                } else{
                    A_param_1 = pars[paste0('A', ag1)]
                }
                
                if (f1 == 1){
                    A_param_2 = 0
                }else if (ag2 == ref_group){
                    A_param_2 = 1
                }else{
                    A_param_2 = pars[paste0('A', ag2)]
                }
                age_contribution[as.character(season), as.character(birth_year)] = A_param_1 * f1 + A_param_2 * f2
            }
        }
    }
    return(age_contribution)
}

get_vac_contribution_age = function(pars, p0, subtype){
    # Calculate age contribution
    age_contribution = p0 # Copy p0 as a template for the age contribution matrix
    for (season in rownames(p0)){
        for (birth_year in colnames(p0)){
            if (birth_year <= season_bounds[as.character(season), 'max_year'] &
                birth_year >= season_bounds[as.character(season), 'min_year']){
                season_test = season
                demo = filter(demo_raw, Birth_year == birth_year, Season == season_test)
                a1 = demo$a1
                a2 = demo$a2
                f1 = demo$f1
                f2 = 1 - f1
                ag1 = age_to_vax_age_group[as.character(a1), 'age_group']
                ag2 = age_to_vax_age_group[as.character(a2), 'age_group']

                A_param_1 = pars[paste('VE', ag1, subtype, sep='_')]
                
                if (f1 == 1){
                    A_param_2 = 0
                }else{
                    A_param_2 = pars[paste('VE', ag2, subtype, sep='_')]
                }
                age_contribution[as.character(season), as.character(birth_year)] = A_param_1 * f1 + A_param_2 * f2
            }
        }
    }
    return(age_contribution)
}


get_vac_contribution_cohort = function(pars, p0, subtype){
    # Calculate ve by cohort
    cohort_ve = p0 # Copy p0 as a template for the age contribution matrix
    for (season in rownames(p0)){
        for (birth_year in colnames(p0)){
            if (birth_year <= season_bounds[as.character(season), 'max_year'] &
                birth_year >= season_bounds[as.character(season), 'min_year']){
                cohort = birth_year_to_birth_year_group[as.character(birth_year), 'birth_year_cohort']
                ve_param_name = paste('VE', cohort, subtype, sep='_')
                Cv = pars[ve_param_name]
                cohort_ve[as.character(season), as.character(birth_year)] =  Cv
            }
        }
    }
    return(cohort_ve)
}



get_vac_contribution = function(pars, p0){
    # Calculate age contribution
    vac_contribution = p0 # Copy p0 as a template for the age contribution matrix
    for (season in rownames(p0)){
        vac_factor = pars[paste('VE', season, sep='_')]
        vac_contribution[season, ] = rep(vac_factor, ncol(vac_contribution))
    }
    return(vac_contribution)
}

get_n_points <- function(pp) {
    n_points = length(pp[!is.na(pp)]) * 2 - ncol(p0['2009.5', !is.na(p0['2009.5', ])]) # vaccinated + unvaccinated for every season - the vaccinated class for the pandemic season
    return(n_points)
}

print_expectation <- function(optim_output, model, model_name) {
    parcols = length(colnames(optim_output)) - 8
    mle = as.numeric(optim_output['L-BFGS-B', colnames(optim_output)[1:parcols]])
    names(mle) = gsub('\\.','-', colnames(optim_output)[1:parcols])
    pp <- model(pars=mle)

    # Prints the expectations from MLE 
    write.csv(pp$ppH1_unvac, sprintf('%s/%s_H1_unvac_expectations.csv', result_folder, model_name))
    write.csv(pp$ppH1_vac, sprintf('%s/%s_H1_vac_expectations.csv', result_folder, model_name))
    write.csv(pp$ppH3_unvac, sprintf('%s/%s_H3_unvac_expectations.csv', result_folder, model_name))
    write.csv(pp$ppH3_vac, sprintf('%s/%s_H3_vac_expectations.csv', result_folder, model_name))
    simulate_multinom_ci(pp$ppH3_unvac, 
                         pp$ppH3_vac,
                         h3_unvac,
                         h3_vac) -> h3_result
    simulate_multinom_ci(pp$ppH1_unvac, 
                         pp$ppH1_vac,
                         h1_unvac,
                         h1_vac) -> h1_result
    
    h3_result %>% 
      group_by(age_group, vac_status, season) %>% 
      summarize(Prediction=sum(mean_value),
                ci_high=sum(ci_high),
                ci_low=sum(ci_low),
                Observed=sum(cases)) -> h3_df
    h1_result %>% 
      group_by(age_group, vac_status, season) %>% 
      summarize(Prediction=sum(mean_value),
                ci_high=sum(ci_high),
                ci_low=sum(ci_low),
                Observed=sum(cases)) -> h1_df
    
    write.csv(h3_df, sprintf('%s/%s_H3_expectations.csv', result_folder, model_name))
    write.csv(h1_df, sprintf('%s/%s_H1_expectations.csv', result_folder, model_name))
    
    NLL <- optim_output$value
    K <- length(mle)
    n <- get_n_points(p0)

    for (p in mle){
      if (p == 0.5){
        K = K - 1
      }
    }
    cAIC <- calc_cAIC(NLL, n, K)
    lik_info <- c(loglik=NLL, df = K, n=n, cAIC=cAIC, mle)
    return(lik_info)
}


calc_lik <- function(par) {
    lik <- model_lik(model(pars=par))
    return(lik)
}

get_squareform = function(incidence, subtype, inc_type){
  # Converts long incidence by age to wide incidence by age
  final_df = data.frame(matrix(ncol=length(groupNames), nrow=length(seasons)))
  rownames(final_df) = seasons
  colnames(final_df) = groupNames
  final_df[ , ] = 0 
  
  for (row in rownames(incidence)){
    age = incidence[row, 'Age']
    if (age >= min_age & age <= max_age){
      ag = age_to_age_group[as.character(age), 'age_group']
      
      if (inc_type=='vac'){
        I_tot = incidence[row, paste0('I_vac_', subtype)]
      }else if(inc_type=='unvac'){
        I_tot = incidence[row, paste0('I_obs_', subtype)]
      }else{
        I_tot = incidence[row, paste0('I_obs_', subtype)] + incidence[row, paste0('I_vac_', subtype)]
      }
      
      season = incidence[row, 'Season']
      if (season == '2009Pan'){
        season = '2009.5'
      }
      final_df[as.character(season), ag] = final_df[as.character(season), ag] + I_tot
    }
  }
  
  return(final_df)
}


convert_birth_year_to_age = function(expectations){
  final_df = data.frame(matrix(ncol=length(groupNames), nrow=length(seasons)))
  rownames(final_df) = seasons
  colnames(final_df) = groupNames
  final_df[ , ] = 0

  for (birth_year in 1918:2018){
    for (season in seasons){
      demo = filter(demo_raw, Birth_year==birth_year, Season==season)
      total_frac = expectations[as.character(season), as.character(birth_year)]

      if (nrow(demo) > 0 & !is.null(total_frac)){

        if (!is.na(demo$a1) & !is.na(total_frac)){
          ag1 = age_to_age_group[as.character(demo$a1), 'age_group']
          f1 = demo$f1
          
          final_df[as.character(season), ag1] = final_df[as.character(season), ag1] + f1 * total_frac
        }
        
        if (!is.na(demo$a2) & !is.na(total_frac)){
          ag2 = age_to_age_group[as.character(demo$a2), 'age_group']
          f2 = demo$f2
          final_df[as.character(season), ag2] = final_df[as.character(season), ag2] + f2 * total_frac
        }
      }
    }
  }
  return(final_df)
}


simulate_multinom_ci = function(exp_unvac, exp_vac, incidence_unvac, incidence_vac){
  ntrials = 10000 # Number of multinomial draws
  incidence_total = cbind(incidence_unvac, incidence_vac) # The total incidence in both classes for each season

  totals = rowSums(incidence_total)
  class_names = names(incidence_total)[1:ncol(incidence_unvac)] # Names of classes
  names(incidence_total) = (c(class_names, paste0(class_names, 'v')))
  final = data.frame(season=numeric(),
                     age_group=numeric(),
                     vac_status=character(),
                     mean_value=numeric(),
                     ci_high=numeric(),
                     ci_low=numeric(),
                     cases=numeric())
  exp = cbind(convert_birth_year_to_age(exp_unvac), 
              convert_birth_year_to_age(exp_vac)) # convert expectations to expectations by age group
  for (season in seasons) {
    size = rowSums(incidence_total[as.character(season), ], na.rm = TRUE) # Total number of cases
    row = exp[as.character(season), ] # Expected probs per age group
    p = row[!is.na(row)] # Remove NAs

    num_classes = length(p)/2 # Number of age classes, we divide by two because the column names are doubled for vac and unvac
    
    classes = names(row)[1:num_classes]
    names(p) = (c(classes, paste0(classes, 'v')))
    means = p * size # Expected value
    names(means) = names(p)

    s = as.data.frame(rmultinom(ntrials, size, p)) # Do the multinomial draws

    # Find the quantiles
    ci_h = apply(s, 1, function(x) quantile(x, probs=.975)) 
    ci_l = apply(s, 1, function(x) quantile(x, probs=.025))
    for (age_group in names(means)){
      case_count = incidence_total[as.character(season), age_group]
      
      if (grepl("v", age_group)){
        ag_name=gsub("v", "", age_group)
        vac_status='vaccinated'
      } else{
        ag_name=age_group
        vac_status='unvaccinated'
      }
      
      temp = data.frame(season=as.numeric(season),
                        age_group=ag_name,
                        vac_status=vac_status,
                        mean_value=as.numeric(means[age_group]), 
                        ci_high=as.numeric(ci_h[age_group]), 
                        ci_low=as.numeric(ci_l[age_group]),
                        cases=case_count)

      final = rbind(final, temp)
    }
  }

  return(final)
}
