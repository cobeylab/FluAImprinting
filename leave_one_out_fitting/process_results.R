source('../../models/consolidated_models.R')
all_models = mget(ls())

args = commandArgs(trailingOnly=TRUE)
exclude_year = as.numeric(args[1])


library('dplyr')
library('foreach')
library('digest')
source('prep_model_inputs.R')
source('linear_model_functions.R')
source('set_fit_parameters.R')

profile=FALSE

all_params = c('loglik',
               'df',
               'n',
               'cAIC',
               'H1m',
               'H3m',
               'N1m',
               'N2m',
               A_param_names,
               V_param_names,
               Vage_param_names,
               Vimprinting_param_names,
               Vcohort_param_names,
               've_h1',
               've_h3')

nparams = length(all_params)
final_df = data.frame(matrix(ncol=nparams, nrow=15))

rownames(final_df) = all_model_names[1:15]
colnames(final_df) = all_params

h1_vac = get_squareform(inc_age, 'H1', 'vac')
h1_unvac = get_squareform(inc_age, 'H1', 'unvac')
h3_vac = get_squareform(inc_age, 'H3', 'vac')
h3_unvac = get_squareform(inc_age, 'H3', 'unvac')

for (i in 1:length(all_models)){
    model_name = all_model_names[[i]]
    model = all_models[[model_name]]
    print(model_name)
    infile = paste0(result_folder, model_name, '.optimx.parallel.rds')
    if (file.exists(infile)){
        load(infile)

        if (result$convcode != 0){
          print(result)
        }
        print_expectation(optim_output=result,
                          model=model,
                          model_name=model_name) -> result_lik
        
        for (col in names(result_lik)){
            final_df[model_name, col] = result_lik[[col]]
        }
    } else{
      print(paste('Missing', model_name))
    }


}

write.csv(final_df, file=paste0(result_folder, 'result_summary.csv'))
