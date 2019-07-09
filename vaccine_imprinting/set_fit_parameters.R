# Set intitial VE and A guesses
groups_to_test = rownames(age_groups)[sapply(rownames(age_groups), function(x) x!=ref_group)]
A_param_names = paste('A', groups_to_test, sep='')
A_guess = rep(2, length(A_param_names))
A_upper = rep(100, length(A_param_names))
A_lower = rep(1e-6, length(A_param_names))
names(A_guess) = names(A_upper) = names(A_lower) = A_param_names


Vage_param_names = c(paste('VE', row.names(vax_age_groups), 'h1', sep='_'), paste('VE', row.names(vax_age_groups), 'h3', sep='_'))
Vage_guess = rep(0.5, length(Vage_param_names))
Vage_upper = rep(9.99e-1, length(Vage_param_names))
Vage_lower = rep(0, length(Vage_param_names))
names(Vage_guess) = names(Vage_lower) = names(Vage_upper) = Vage_param_names


parameters = list(DAHVage_subtype_vProtect_x=c(A_param_names, Vage_param_names, 'H1m', 'H3m'))

all_model_names = names(parameters)
initial_condition = c(A_guess,
                      Vage_guess,
                      H1m=0.5,
                      H3m=0.5)

lower_bound = c(A_lower,
                Vage_lower,
                H1m=0,
                H3m=0)

upper_bound = c(A_upper,
                Vage_upper,
                H1m=9.999e-1,
                H3m=9.999e-1)