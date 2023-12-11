library(tidyverse)
library(rstan)

# TODO: NEED TO CHANGE DATASET

# Load Data
load('./data/behav_ind_raw.RData')
load('./data/behav_ind_summarized.RData')
behav_ind_raw <- mutate(behav_ind_raw,
                        run_hide = run_and_hide + hide)
behav_ind_raw <- na.omit(behav_ind_raw)
attach(behav_ind_raw)

model_obj <- list(B = 5,
                  N = length(band),
                  band = as.numeric(band),
                  tarsus = as.numeric(tarsus_length),
                  weight = as.numeric(weight),
                  wing = as.numeric(wing_length),
                  sex = as.integer(numeric_sex),
                  experience = as.integer(years),
                  exttime = as.numeric(ext_time),
                  passive = passive,
                  bite = bite,
                  run_hide = run_hide,
                  regurgitate = regurgitate,
                  vocalize = vocalize,
                  kick=kick
)
model = stan_model('./scripts/m1.stan')
fit = sampling(model, model_obj, iter = 10000, chains = 1)
params = rstan::extract(fit)

param_df <- cbind(params$coeff_wt, params$coeff_tl, params$coeff_wl) # working on this
colnames(param_df) <- c('parameter',
                        'bite', 
                        'run_hide', 
                        'regurgitate',
                        'vocalize',
                        'kick')



param_df <- enframe(params$coeff_wt, name = 'beta_wt', value = 'beta_wt') |>
            unnest_longer(model_p)

ggplot(param_df)+geom_histogram(aes(x = model_p), 
                                bins = 100,
                                fill = 'lightblue',
                                color = 'black',
                                linewidth = 0.25)+
                 facet_wrap(facets = vars(behavior),
                            nrow = 5)+
                 xlim(c(0, 1))
