library(tidyverse)
library(rstan)
library(patchwork)
library("bridgesampling")
library("loo")

# Functions --------------------------------------------------------------------
make_param_df <- function(list_in){
  temp <- list('bite' = list_in[,1], 
               'run_hide' = list_in[,2],
               'regurgitate' = list_in[,3],
               'vocalize' = list_in[,4],
               'kick' = list_in[,5])
  out <- enframe(temp, name = 'behavior', value = 'value') |>
         unnest_longer(value)
  return(out)
}

plot_param <- function(list_in, plot_title = NULL){
  temp <- make_param_df(list_in)
  p <- ggplot(temp)+
       geom_histogram(aes(x = value),
                          bins = 100,
                          fill = 'lightblue',
                          color = 'black',
                          linewidth = 0.25)+
       facet_wrap(facets = vars(behavior),
                  nrow = 5)+
       labs(title = plot_title, x = 'Value', y = 'Count')
  return(p)
}


# Load Data --------------------------------------------------------------------
load('./data/behav_ind_raw.RData')
load('./data/behav_ind_summarized.RData')
behav_ind_raw <- mutate(behav_ind_raw,
                        run_hide = run_and_hide + hide)
behav_ind_raw <- na.omit(behav_ind_raw)
attach(behav_ind_raw)

# STAN Time --------------------------------------------------------------------
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
wt_model = stan_model('./scripts/m1_wt.stan')
tl_model = stan_model('./scripts/m1_tl.stan')
wl_model = stan_model('./scripts/m1_wl.stan')
wt_fit = sampling(wt_model, model_obj, iter = 10000, chains = 1)
tl_fit = sampling(tl_model, model_obj, iter = 10000, chains = 1)
wl_fit = sampling(wl_model, model_obj, iter = 10000, chains = 1)
params_wt = rstan::extract(wt_fit)
params_tl = rstan::extract(tl_fit)
params_wl = rstan::extract(wl_fit)

bf.wt_wl <- bayes_factor( bridge_sampler(wt_fit, silent = TRUE),bridge_sampler(wl_fit, silent = TRUE))
print(bf.wt_wl) #Bayes factor in favor of x1 over x2: 3.95877

bf.wt_tl <- bayes_factor( bridge_sampler(wt_fit, silent = TRUE),bridge_sampler(tl_fit, silent = TRUE))
print(bf.wt_tl) #Bayes factor in favor of x1 over x2: 0.00087 (so x2 over x1 is 1149.425)

bf.tl_wl <- bayes_factor( bridge_sampler(tl_fit, silent = TRUE),bridge_sampler(wl_fit, silent = TRUE))
print(bf.tl_wl) #Bayes factor in favor of x1 over x2: 4714.73077

#so tarsus length is the best model

