options(warn=1, error=traceback)

library('optimx')
source('../models/consolidated_models.R')

all_models = mget(ls())

library('pracma')
library('digest')
source('prep_model_inputs.R')
source('set_fit_parameters.R')
vProtect = 0.8


args = commandArgs(trailingOnly=TRUE)
point_index = as.numeric(args[1])
par_name = args[2]
par_min = as.numeric(args[3])
par_max = as.numeric(args[4])
num_points = as.numeric(args[5])
model_index = as.numeric(args[6])
jobid = as.numeric(args[7])
fh = as.numeric(args[8])

jobid = paste0(jobid, '_', point_index)

ptm <- proc.time()

model_name = all_model_names[[model_index]]
model = all_models[[model_name]]
outfile = sprintf('%s/%s.profile.%s.%s.rds',
                  result_folder,
                  model_name,
                  par_name,
                  point_index)
print(outfile)

print(model)
print(model_name)
par_range = linspace(par_min, par_max, num_points)
par_test = par_range[point_index]
fixed_pars = c()
fixed_pars[[par_name]] = par_test

print(par_test)
upper_bound = upper_bound[parameters[[model_name]]]
lower_bound = lower_bound[parameters[[model_name]]]
initial_condition = initial_condition[parameters[[model_name]]]

initial_condition[[par_name]] = par_test
print(upper_bound)
print(lower_bound)
print(initial_condition)
profile = TRUE
result = optimx(par=initial_condition,
                fn=calc_lik,
                method = 'L-BFGS-B',
                lower = lower_bound,
                upper = upper_bound,
                control = list(kkt=FALSE, maxit=500))

new_parname = gsub('-', '.', par_name)
result['L-BFGS-B', new_parname] = par_test
print(proc.time() - ptm)
print(result)
save(result, file=outfile)