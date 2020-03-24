options(warn=1, error=traceback)

library('optimx')
source('../../models/consolidated_models.R')

all_models = mget(ls())

profile=FALSE
args = commandArgs(trailingOnly=TRUE)
model_index = as.numeric(args[1])
exclude_year = as.numeric(args[2])

source('./prep_model_inputs.R')
source('./set_fit_parameters.R')

ptm <- proc.time()

model_name = all_model_names[[model_index]]
model = all_models[[model_name]]
outfile = sprintf('%s/%s.optimx.parallel.rds', result_folder, model_name)


initial_condition = initial_condition[parameters[[model_name]]]
upper_bound = upper_bound[parameters[[model_name]]]
lower_bound = lower_bound[parameters[[model_name]]]

print(outfile)
print(model)
print(model_name)
print(initial_condition)
print(lower_bound)
print(upper_bound)
result = optimx(par=initial_condition,
                fn=calc_lik,
                method = 'L-BFGS-B',
                lower = lower_bound,
                upper = upper_bound,
                control = list(kkt=FALSE, maxit=500))
print(proc.time() - ptm)
print(result)
save(result, file=outfile)