# Set intitial VE and A guesses
groups_to_test = rownames(age_groups)[sapply(rownames(age_groups), function(x) x!=ref_group)]
A_param_names = paste('A', groups_to_test, sep='')
A_guess = rep(2, length(A_param_names))
A_upper = rep(100, length(A_param_names))
A_lower = rep(1e-6, length(A_param_names))
names(A_guess) = names(A_upper) = names(A_lower) = A_param_names

V_param_names = paste('VE', row.names(demo_data), sep='_')
V_guess = rep(0.5, length(V_param_names))
V_upper = rep(9.99e-1, length(V_param_names))
V_lower = rep(0, length(V_param_names))
names(V_guess) = names(V_upper) = names(V_lower) = V_param_names

Vage_param_names = c(paste('VE', row.names(vax_age_groups), 'h1', sep='_'), paste('VE', row.names(vax_age_groups), 'h3', sep='_'))
Vage_guess = rep(0.5, length(Vage_param_names))
Vage_upper = rep(9.99e-1, length(Vage_param_names))
Vage_lower = rep(0, length(Vage_param_names))
names(Vage_guess) = names(Vage_lower) = names(Vage_upper) = Vage_param_names

Vcohort_param_names = c(paste('VE', row.names(cohorts), 'h1', sep='_'), paste('VE', row.names(cohorts), 'h3', sep='_'))
Vcohort_guess = rep(0.5, length(Vcohort_param_names))
Vcohort_upper = rep(9.99e-1, length(Vcohort_param_names))
Vcohort_lower = rep(0, length(Vcohort_param_names))
names(Vcohort_guess) = names(Vcohort_lower) = names(Vcohort_upper) = Vcohort_param_names

Vimprinting_param_names = c(paste('VE', 'h1', c('h1', 'h2', 'h3'), sep='_'), paste('VE', 'h3',  c('h1', 'h2', 'h3'), sep='_'))
Vimprinting_group_param_names = c(paste('VE', 'h1', c('h1', 'h3'), sep='_'), paste('VE', 'h3',  c('h1', 'h3'), sep='_'))

Vimprinting_guess = rep(0.5, length(Vimprinting_param_names))
Vimprinting_upper = rep(9.99e-1, length(Vimprinting_param_names))
Vimprinting_lower = rep(0, length(Vimprinting_param_names))
names(Vimprinting_guess) = names(Vimprinting_lower) = names(Vimprinting_upper) = Vimprinting_param_names

parameters = list(
                DAHVage_subtype=c(A_param_names, Vage_param_names, 'H1m', 'H3m'),
                DAHNVage_subtype=c(A_param_names, Vage_param_names, 'H1m', 'H3m', 'N2m'),
                DAHNVage_group=c(A_param_names, Vage_param_names, 'H1m', 'H3m', 'N2m'),
                DAHNVseason_subtype=c(A_param_names, V_param_names, 'H1m', 'H3m', 'N2m'),
                DAHNVseason_group=c(A_param_names, V_param_names, 'H1m', 'H3m', 'N2m'),
                DAHNVmean_subtype=c(A_param_names, 've_h1', 've_h3', 'H1m', 'H3m', 'N2m'),
                DAHNVmean_group=c(A_param_names, 've_h1', 've_h3', 'H1m', 'H3m', 'N2m'),
                DAHNVimprinting_subtype=c(A_param_names, Vimprinting_param_names, 'H1m', 'H3m', 'N2m'),
                DAHNVimprinting_group=c(A_param_names, Vimprinting_group_param_names, 'H1m', 'H3m', 'N2m'),
                DAHNVcohort_subtype=c(A_param_names, Vcohort_param_names, 'H1m', 'H3m', 'N2m'),
                DAHNVcohort_group=c(A_param_names, Vcohort_param_names, 'H1m', 'H3m', 'N2m'),
                DAHVcohort_subtype=c(A_param_names, Vcohort_param_names, 'H1m', 'H3m'),
                DAHVage_subtype_vProtect_h=c(A_param_names, Vage_param_names, 'H1m', 'H3m'))


all_model_names = names(parameters)
initial_condition = c(A_guess,
                      V_guess,
                      Vage_guess,
                      Vimprinting_guess,
                      Vcohort_guess,
                      ve_h1=0.5,
                      ve_h3=0.5,
                      H1m=0.5,
                      H3m=0.5,
                      N2m=0.5)

lower_bound = c(A_lower,
                V_lower,
                Vage_lower,
                Vimprinting_lower,
                Vcohort_lower,
                ve_h1=0,
                ve_h3=0,
                H1m=0,
                H3m=0,
                N2m=0)

upper_bound = c(A_upper,
                V_upper,
                Vage_upper,
                Vimprinting_upper,
                Vcohort_upper,
                ve_h1=9.999e-1,
                ve_h3=9.999e-1,
                H1m=9.999e-1,
                H3m=9.999e-1,
                N2m=9.999e-1)