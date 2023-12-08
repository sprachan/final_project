library(tidyverse)
library(rstan)

# Load Data
load('./data/behav_ind_raw.RData')
load('./data/behav_ind_summarized.RData')
behav_ind_summarized <- mutate(behav_ind_summ,
                               run_hide = run_and_hide + hide) |>
                        filter(!is.na(years),
                               !is.na(tarsus_length),
                               !is.na(weight))
attach(behav_ind_summarized)

model_obj <- list(N = length(band),
                  num_observations = as.integer(num_observations),
                  band = as.numeric(band),
                  tarsus = as.numeric(tarsus_length),
                  weight = as.numeric(weight),
                  wing = as.numeric(wing_length),
                  sex = as.integer(numeric_sex),
                  experience = as.integer(years),
                  exttime = as.numeric(avg_ext_time),
                  passive = passive,
                  bite = bite,
                  run_hide = run_hide,
                  regurgitate = regurgitate,
                  vocalize = vocalize,
                  kick = kick
)
model = stan_model('./scripts/m0.stan')
fit = sampling(model, model_obj, iter = 10000, chains = 1)
params = rstan::extract(fit)

# visualize results
p <- params$p
p <- list('passive' = p[,1], 
          'bite' = p[,2], 
          'run_hide' = p[,3],
          'regurgitate' = p[,4],
          'vocalize' = p[,5],
          'kick' = p[,6])
param_df <- enframe(p, name = 'behavior', value = 'model_p') |>
            unnest_longer(model_p)

ggplot(param_df)+geom_histogram(aes(x = model_p), 
                                bins = 100,
                                fill = 'lightblue',
                                color = 'black',
                                linewidth = 0.25)+
                 facet_wrap(facets = vars(behavior),
                            nrow = 6)+
                 xlim(c(0, 1))+
                xlab('p values for M0')
