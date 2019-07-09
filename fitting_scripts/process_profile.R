library('foreach')
library('ggplot2')
library('grid')
library('gridExtra')
library('dplyr')
source('prep_model_inputs.R')

groups_to_test = rownames(age_groups)[sapply(rownames(age_groups), function(x) x!=ref_group)]

model_name = 'DAHNVimprinting_group'

params = c('H1m', 
           'H3m',
           'N2m',
           paste0('A', groups_to_test),
           paste0('VE_', groupNames, '_h1'),
           paste0('VE_', groupNames, '_h3'))#,
           #paste0('VE_', cohort_names, '_h1'),
           #paste0('VE_', cohort_names, '_h3'))

mle_file = paste0(result_folder, model_name, '.optimx.parallel.rds')
load(mle_file)
mle = result

plts = list()
final_df = data.frame(matrix(ncol=4, nrow=length(params)))
names(final_df) = c('param', 'mle', 'prof_min', 'prof_max')
for (j in 1:length(params)){
    p = params[j]

    search_str = paste0(result_folder, model_name ,".profile*", p, ".*.rds")
    profile_files = Sys.glob(search_str)
    p = gsub('-', '.', p)
    print(p)
    if (length(profile_files) > 0){
        foreach (i=1:length(profile_files), .combine='rbind') %do%{
            load(profile_files[i])
            param = result[[p]]
            c(result$value, param)
        } -> df

        df = rbind(df, c(mle$value, mle[[p]]))
        colnames(df) = c('loglik', 'param')
        df = data.frame(df)
        df$loglik = -df$loglik
        df = df %>% filter(loglik > max(df$loglik) - 100)
        
        l = loess(loglik ~ param, data=df, span=0.5)
        fit = data.frame(loglik=l$y, param=l$x)
        fit %>% filter(loglik < max(df$loglik) - 1.92) -> f
        fit %>% filter(param == mle[[p]]) -> x
        
        prof_min = min(df$param)
        prof_max = max(df$param)
        for (x in seq(mle[[p]], prof_min, by = -0.001)){
            pred = predict(l, x)
            if (pred < max(df$loglik) - 1.92){
                prof_min = x
                break
            }
        }
        
        for (x in seq(mle[[p]], prof_max, by = 0.001)){
            pred = predict(l, x)
            if (pred < max(df$loglik) - 1.92){
                prof_max = x
                break
            }
        }
        

        ggplot(df, aes(param, loglik)) + 
            geom_point() +
            geom_smooth(method = 'loess', formula = 'y ~ x', span=0.5) +
            geom_hline(yintercept = max(df$loglik) - 1.92) +
            xlab(p) -> plt
        plts[[p]] = plt
        final_df[j, ] = c(p, mle[[p]], prof_min, prof_max)
    }
}

grid.arrange(grobs=plts, ncol=5) -> g
ggsave(paste0(result_folder, model_name, '.all_profiles.png'), dpi=300, plot = g, width = 10, height = 10)
write.csv(final_df, file=paste0(result_folder, model_name, '.profile_liks.csv'))