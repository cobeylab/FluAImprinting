source('../../models/consolidated_models.R')
all_models = mget(ls())
profile = FALSE
source('./prep_model_inputs.R')
source('./set_fit_parameters.R')

rows = NULL

global_mle = read.csv('../../final_results_for_ms/0-100/result_summary.csv', check.names=FALSE, row.names=1)


simulate_single_season <- function(pp, i) {
  
  
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
    rmultinom(500,
              size = tot_h1,
              prob = as.numeric(cbind(ppH1_unvac[i, cols], ppH1_vac[i, cols]))) -> draws
  }
  # If season is H3 dominated
  else{
    # Remove columns with unnecessary birth years
    cols = !is.na(incH3_unvac[i, ])
    rmultinom(500,
              size = tot_h3,
              prob = as.numeric(cbind(ppH3_unvac[i, cols], ppH3_vac[i, cols]))) -> draws
  }
  
  draws = as.data.frame(draws)
  rownames(draws) = combine(colnames(cols)[cols], paste0('vac',colnames(cols)[cols]))
  return(draws)
}


for (seasonex in seasons){
  if (seasonex != 2009.5)
  {
    result_file = sprintf('./results_ex2009.5_ex%s/result_summary.csv', seasonex)
    all_mles = read.csv(result_file, check.names=FALSE, row.names=1)
    seasonex = sprintf('%s',seasonex)
    
    
    model_name = 'DAHVage_subtype'
    print(seasonex)
    model = all_models[[model_name]]
    pars = initial_condition[parameters[[model_name]]]
    
    
    mle = all_mles[model_name, ]
    mle = mle[model_name, names(pars)]
    temp = as.numeric(mle)
    names(temp) = names(mle)
    mle = temp
    
    pp = model(mle)
    
    
    # Make simulated datasets and compare to real data
    sims = simulate_single_season(pp, as.character(seasonex))
    write.csv(sims, file=sprintf('loo_simulation_%s.csv', seasonex))
  }

}

