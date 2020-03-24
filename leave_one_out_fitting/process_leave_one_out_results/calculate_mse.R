source('../../models/consolidated_models.R')
all_models = mget(ls())
profile = FALSE
source('./prep_model_inputs.R')
source('./set_fit_parameters.R')

rows = NULL

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
    expected = cbind(ppH1_unvac[i, cols], ppH1_vac[i, cols]) * tot_h1
    obs = cbind(incH1_unvac[i, cols], incH1_vac[i, cols])
  }
  # If season is H3 dominated
  else{
    # Remove columns with unnecessary birth years
    cols = !is.na(incH3_unvac[i, ])
    expected = cbind(ppH3_unvac[i, cols], ppH3_vac[i, cols]) * tot_h3
    obs = cbind(incH3_unvac[i, cols], incH3_vac[i, cols])
  }
  
  se = sum((obs - expected)^2)
  return(se)
}


for (seasonex in seasons){
  
  if (seasonex !=2009.5)
  {
    print(seasonex)
    result_file = sprintf('./results_ex2009.5_ex%s/result_summary.csv', seasonex)
    all_mles = read.csv(result_file, check.names=FALSE, row.names=1)
    seasonex = sprintf('%s',seasonex)
    
    for (model_name in names(all_models)){
      if(!grepl('season', model_name)){
        
        
        print(model_name)
        model = all_models[[model_name]]
        pars = initial_condition[parameters[[model_name]]]
        
        
        mle = all_mles[model_name, ]
        mle = mle[model_name, names(pars)]
        temp = as.numeric(mle)
        names(temp) = names(mle)
        mle = temp
        
        pp = model(mle)
        
        
        # Make simulated datasets and compare to real data
        se = simulate_single_season(pp, as.character(seasonex))
        print(se)
        row = c(seasonex, model_name, se)
        rows = rbind(rows, row)
      }
      
  }
  
  }
}
rows = as.data.frame(rows)
names(rows) = c('season_excluded', 'model', 'squared_error')
write.csv(rows, file='squared_error_loo.csv', row.names=FALSE)
