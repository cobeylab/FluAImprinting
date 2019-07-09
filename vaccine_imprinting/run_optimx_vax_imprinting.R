options(warn=1, error=traceback)

library('optimx')
library('digest')
source('../models/vaccine_imprinting.R')

all_models = mget(ls())

source('prep_model_inputs.R')
source('set_fit_parameters.R')

profile=FALSE
args = commandArgs(trailingOnly=TRUE)
model_index = 1
jobid = as.numeric(args[2])
x = as.numeric(args[1])/100

jobid = paste0(jobid, '_', model_index)

ptm <- proc.time()

model_name = all_model_names[[model_index]]
model = all_models[[model_name]]
outfile = sprintf('%s/%s_x_%s.optimx.parallel.rds', result_folder, model_name, x)


initial_condition = initial_condition[parameters[[model_name]]]
upper_bound = upper_bound[parameters[[model_name]]]
lower_bound = lower_bound[parameters[[model_name]]]

names(initial_condition) = names(upper_bound)

print(outfile)
print(model)
print(model_name)
print(initial_condition)

result = optimx(par=initial_condition,
                fn=calc_lik,
                method = 'L-BFGS-B',
                lower = lower_bound,
                upper = upper_bound,
                control = list(kkt=FALSE, maxit=500))
print(proc.time() - ptm)
print(result)
save(result, file=outfile)