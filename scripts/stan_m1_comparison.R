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

plot_histogram <- function(ps, plot_title) {
  temp <- data.frame(values=ps)
  p <- ggplot(temp)+
    geom_histogram(aes(x = values),
                   bins = 100,
                   fill = 'lightblue',
                   color = 'black',
                   linewidth = 0.25)+
    geom_vline(xintercept=mean(temp$values))+ # trying to add line at the mean
    labs(title = plot_title, x = 'Predicted p', y = 'Count')+
    xlim(c(0,1))
  return(p)
}


# Load Data --------------------------------------------------------------------
load('./data/behav_ind_raw.RData')
load('./data/behav_ind_summarized.RData')
behav_ind_raw <- mutate(behav_ind_raw,
                        run_hide = run_and_hide + hide)
behav_ind_raw <- na.omit(behav_ind_raw)
attach(behav_ind_raw)

N = length(band)

# STAN Time --------------------------------------------------------------------
model_obj <- list(B = 5,
                  N = N,
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

# MODEL 0
model0 = stan_model('./scripts/m0.stan')
fit0 = sampling(model0, model_obj, iter = 10000, chains = 1)
params0 = rstan::extract(fit0)

# MODEL 1
exp_model = stan_model('./scripts/m1_experience.stan')
wt_model = stan_model('./scripts/m1_wt.stan')
tl_model = stan_model('./scripts/m1_tl.stan')
wl_model = stan_model('./scripts/m1_wl.stan')
model1 = stan_model('./scripts/m1_comp.stan')
exp_fit = sampling(exp_model, model_obj, iter = 10000, chains = 1)
wt_fit = sampling(wt_model, model_obj, iter = 10000, chains = 1)
tl_fit = sampling(tl_model, model_obj, iter = 10000, chains = 1)
wl_fit = sampling(wl_model, model_obj, iter = 10000, chains = 1)
fit1 = sampling(model1, model_obj, iter = 10000, chains = 1)
params_exp = rstan::extract(exp_fit)
params_wt = rstan::extract(wt_fit)
params_tl = rstan::extract(tl_fit)
params_wl = rstan::extract(wl_fit)
params1 = rstan::extract(fit1)

# MODEL 2
model_obj2 <- model_obj
model_obj2$B <- 3
model2 = stan_model('./scripts/m2.stan')
fit2 = sampling(model2, model_obj2, iter = 10000, chains = 1)
params2 = rstan::extract(fit2)

# MODEL 3
model3 = stan_model('./scripts/m3.stan')
fit3 = sampling(model3, model_obj2, iter = 10000, chains = 1)
params3 = rstan::extract(fit3)

# BAYES FACTORS
bf.wt_0 <- bayes_factor(bridge_sampler(wt_fit, silent = TRUE),bridge_sampler(fit0, silent = TRUE))
print(bf.wt_0) #Bayes factor in favor of x1 over x2: ~18

bf.tl_0 <- bayes_factor(bridge_sampler(tl_fit, silent = TRUE),bridge_sampler(fit0, silent = TRUE))
print(bf.tl_0) #Bayes factor in favor of x1 over x2: 30,000

bf.wl_0 <- bayes_factor(bridge_sampler(wl_fit, silent = TRUE),bridge_sampler(fit0, silent = TRUE))
print(bf.wl_0) #Bayes factor in favor of x1 over x2: 5

bf.exp_0 <- bayes_factor(bridge_sampler(exp_fit, silent = TRUE),bridge_sampler(fit0, silent = TRUE))
print(bf.exp_0) #Bayes factor in favor of x1 over x2:60,978

bf.wt_wl <- bayes_factor(bridge_sampler(wt_fit, silent = TRUE),bridge_sampler(wl_fit, silent = TRUE))
print(bf.wt_wl) #Bayes factor in favor of x1 over x2: 3.95877

bf.wt_tl <- bayes_factor(bridge_sampler(wt_fit, silent = TRUE),bridge_sampler(tl_fit, silent = TRUE))
print(bf.wt_tl) #Bayes factor in favor of x1 over x2: ~0.0009 (so x2 over x1 is 1149.425)

bf.tl_wl <- bayes_factor(bridge_sampler(tl_fit, silent = TRUE),bridge_sampler(wl_fit, silent = TRUE))
print(bf.tl_wl) #Bayes factor in favor of x1 over x2: ~4000

bf.exp_tl <- bayes_factor(bridge_sampler(exp_fit, silent = TRUE),bridge_sampler(tl_fit, silent = TRUE))
print(bf.exp_tl) #Bayes factor in favor of x1 over x2: ~2

bf.1_exp <- bayes_factor(bridge_sampler(fit1, silent = TRUE),bridge_sampler(exp_fit, silent = TRUE))
print(bf.1_exp) #Bayes factor in favor of x1 over x2: .3

bf.exp_1 <- bayes_factor(bridge_sampler(exp_fit, silent = TRUE),bridge_sampler(fit1, silent = TRUE))
print(bf.exp_1) #Bayes factor in favor of x1 over x2: ~4

bf.1_tl <- bayes_factor(bridge_sampler(fit1, silent = TRUE),bridge_sampler(tl_fit, silent = TRUE))
print(bf.1_tl) #Bayes factor in favor of 1 over tarsus: 0.52

bf.tl_1 <- bayes_factor(bridge_sampler(tl_fit, silent = TRUE),bridge_sampler(fit1, silent = TRUE))
print(bf.tl_1) #Bayes factor in favor of tarsus over 1: 1.3

bf.2_0 <- bayes_factor(bridge_sampler(fit2, silent = TRUE),bridge_sampler(fit0, silent = TRUE))
print(bf.2_0) #BF in favor of model 2: 

bf.2_1 <- bayes_factor(bridge_sampler(fit2, silent = TRUE),bridge_sampler(fit1, silent = TRUE))
print(bf.2_1) #BF in favor of model 2: 

bf.2_tl <- bayes_factor(bridge_sampler(fit2, silent = TRUE),bridge_sampler(tl_fit, silent = TRUE))
print(bf.2_tl) #BF in favor of model 2: 1821627687958405896515918810588134719952546551556675007110428671458943530999606117169678155513856.00000

bf.3_tl <- bayes_factor(bridge_sampler(fit3, silent = TRUE),bridge_sampler(tl_fit, silent = TRUE))
print(bf.3_tl) #BF in favor of model 3: 204110383584057263203661603408261102783182484765924533267041282481334922885900119118084970460948299713209226480798662656.00000

bf.3_2 <- bayes_factor(bridge_sampler(fit3, silent = TRUE),bridge_sampler(fit2, silent = TRUE))
print(bf.3_2) #BF in favor of model 3: 46293076263295829147648.00000

# PLOTTING MODEL 1
bite_p <- rep(0,N)
run_p <- rep(0,N)
regurg_p <- rep(0,N)
vocal_p <- rep(0,N)
kick_p <- rep(0,N)

for (i in 1:N) {
  bite_p[i] <- mean(params_tl$p[,i,1])
  run_p[i] <- mean(params_tl$p[,i,2])
  regurg_p[i] <- mean(params_tl$p[,i,3])
  vocal_p[i] <- mean(params_tl$p[,i,4])
  kick_p[i] <- mean(params_tl$p[,i,5])
}

bite_g <- plot_histogram(bite_p, 'Bite')
run_g <- plot_histogram(run_p, 'Run & Hide')
regurg_g <- plot_histogram(regurg_p, 'Regurgitate')
vocal_g <- plot_histogram(vocal_p, 'Vocalize')
kick_g <- plot_histogram(kick_p, 'Kick')

bite_g / run_g / regurg_g / vocal_g / kick_g






