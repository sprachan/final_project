library(tidyverse)
library(rstan)

# Load Data
load('./data/behav_ind_raw.RData')
load('./data/behav_ind_summarized.RData')
behav_ind_summarized <- mutate(behav_ind_summ,
                               run_hide = run_and_hide + hide)
attach(behav_ind_summarized)

model_obj <- list(N = length(band),
                  band = band,
                  tarsus = tarsus_length,
                  weight = weight,
                  wing = wing_length,
                  sex = sex_numeric,
                  experience = years,
                  exttime = avg_ext_time,
                  passive = passive,
                  bite = bite,
                  run_hide = run_hide,
                  regurgitate = regurgitate,
                  vocalize = vocalize
)
model = stan_model('./scripts/m0.stan')
#fit = sampling(model, model_obj, iter = 1000, chains = 1)
