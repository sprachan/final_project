library(tidyverse)
library(rstan)

# Load Data
load('./data/behav_ind_raw.RData')
load('./data/behav_ind_summarized.RData')
behav_ind_summarized <- mutate(behav_ind_summ,
                               run_hide = run_and_hide + hide) |>
                        filter(!is.na(years),
                               !is.na(tarsus_length))
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
                  vocalize = vocalize
)
model = stan_model('./scripts/m0.stan')
fit = sampling(model, model_obj, iter = 10000, chains = 1)
params = extract(fit)

# visualize results
p <- params$p
hist(p[,1], main = 'passive', n = 100)
hist(p[,2], main = 'bite', n = 100)
hist(p[,3], main = 'run_hide', n = 100)
hist(p[,4], main = 'regurgitate', n = 100)
hist(p[,5], main = 'vocalize', n = 100)