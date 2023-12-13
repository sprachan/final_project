library(tidyverse)
library(rstan)
library(patchwork)
library(bridgesampling)
library(loo)

# Functions --------------------------------------------------------------------
make_param_df <- function(list_in, is_probs = FALSE){
  # requires this order in temp
  if(is_probs == TRUE){
    temp <- list('bite' = list_in[1], 
                 'run_hide' = list_in[2],
                 'regurgitate' = list_in[3],
                 'vocalize' = list_in[4],
                 'kick' = list_in[5])
    out <- enframe(temp, name = 'behavior', value = 'value') |>
           unnest_longer(value) |>
           unnest_longer(value)
    return(out)
  }else{
    temp <- list('bite' = list_in[,1], 
                 'run_hide' = list_in[,2],
                 'regurgitate' = list_in[,3],
                 'vocalize' = list_in[,4],
                 'kick' = list_in[,5])
    out <- enframe(temp, name = 'behavior', value = 'value') |>
      unnest_longer(value)
    return(out)
  }
  
}

plot_param <- function(list_in, plot_title = NULL, is_probs = FALSE){
  temp <- make_param_df(list_in, is_probs = is_probs)
  if(is_probs == TRUE){
    p <- ggplot(temp)+
      geom_histogram(aes(x = value),
                     bins = 100,
                     fill = 'lightblue',
                     color = 'black',
                     linewidth = 0.25)+
      facet_wrap(facets = vars(behavior),
                 nrow = 5)+
      labs(title = plot_title, x = 'Predicted p', y = 'Count')+
      xlim(c(0,1))
    return(p)
  }else{
    p <- ggplot(temp)+
      geom_histogram(aes(x = value),
                     bins = 100,
                     fill = 'lightblue',
                     color = 'black',
                     linewidth = 0.25)+
      geom_vline(aes(xintercept = 0),
                 linetype = 'dashed',
                 col = '#dd3040')
      facet_wrap(facets = vars(behavior),
                 nrow = 5)+
      labs(title = plot_title, x = 'Value', y = 'Count')
    return(p)
  }
  
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

## MODEL 0 ------
model0 = stan_model('./scripts/m0.stan')
fit0 = sampling(model0, model_obj, iter = 10000, chains = 1)
params0 = rstan::extract(fit0)

## MODEL 1(s) ----
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

## MODEL 2 ----
model_obj2 <- model_obj
model_obj2$B <- 3
model2 = stan_model('./scripts/m2.stan')
fit2 = sampling(model2, model_obj2, iter = 10000, chains = 1)
params2 = rstan::extract(fit2)

## MODEL 3 ----
model3 = stan_model('./scripts/m3.stan')
fit3 = sampling(model3, model_obj2, iter = 10000, chains = 1)
params3 = rstan::extract(fit3)


# CHECK ESS --------------------------------------------------------------------
list(fit0, exp_fit, wt_fit, tl_fit, wl_fit, fit1, fit2, fit3) |>
  map(\(x) min(summary(x)$summary[,9]))
# ESS's are all > 1500, in the 1600-2500 range

# BAYES FACTORS ----------------------------------------------------------------
## M1/M0 comparisons ----
bf.wt_0 <- bayes_factor(bridge_sampler(wt_fit, silent = TRUE),
                        bridge_sampler(fit0, silent = TRUE))

print(bf.wt_0) #Bayes factor in favor of x1 over x2: ~18

bf.tl_0 <- bayes_factor(bridge_sampler(tl_fit, silent = TRUE),
                        bridge_sampler(fit0, silent = TRUE))
print(bf.tl_0) #Bayes factor in favor of x1 over x2: 30,000

bf.wl_0 <- bayes_factor(bridge_sampler(wl_fit, silent = TRUE),
                        bridge_sampler(fit0, silent = TRUE))
print(bf.wl_0) #Bayes factor in favor of x1 over x2: 5

bf.exp_0 <- bayes_factor(bridge_sampler(exp_fit, silent = TRUE),
                         bridge_sampler(fit0, silent = TRUE))
print(bf.exp_0) #Bayes factor in favor of x1 over x2:60,978

## Between M1 alternatives comparisons ----
bf.wt_wl <- bayes_factor(bridge_sampler(wt_fit, silent = TRUE),
                         bridge_sampler(wl_fit, silent = TRUE))
print(bf.wt_wl) #Bayes factor in favor of x1 over x2: 3.95877

bf.wt_tl <- bayes_factor(bridge_sampler(wt_fit, silent = TRUE),
                         bridge_sampler(tl_fit, silent = TRUE))
print(bf.wt_tl) #Bayes factor in favor of x1 over x2: ~0.0009 (so x2 over x1 is 1149.425)

bf.tl_wl <- bayes_factor(bridge_sampler(tl_fit, silent = TRUE),
                         bridge_sampler(wl_fit, silent = TRUE))
print(bf.tl_wl) #Bayes factor in favor of x1 over x2: ~4000

bf.exp_tl <- bayes_factor(bridge_sampler(exp_fit, silent = TRUE),
                          bridge_sampler(tl_fit, silent = TRUE))
print(bf.exp_tl) #Bayes factor in favor of x1 over x2: ~2

bf.1_exp <- bayes_factor(bridge_sampler(fit1, silent = TRUE),
                         bridge_sampler(exp_fit, silent = TRUE))
print(bf.1_exp) #Bayes factor in favor of x1 over x2: .3

bf.exp_1 <- bayes_factor(bridge_sampler(exp_fit, silent = TRUE),
                         bridge_sampler(fit1, silent = TRUE))
print(bf.exp_1) #Bayes factor in favor of x1 over x2: ~4

bf.1_tl <- bayes_factor(bridge_sampler(fit1, silent = TRUE),
                        bridge_sampler(tl_fit, silent = TRUE))
print(bf.1_tl) #Bayes factor in favor of 1 over tarsus: 0.52

bf.tl_1 <- bayes_factor(bridge_sampler(tl_fit, silent = TRUE),
                        bridge_sampler(fit1, silent = TRUE))
print(bf.tl_1) #Bayes factor in favor of tarsus over 1: 1.3

## M2 comparisons ----

bf.2_0 <- bayes_factor(bridge_sampler(fit2, silent = TRUE),
                       bridge_sampler(fit0, silent = TRUE))
print(bf.2_0) #BF in favor of model 2: 

bf.2_1 <- bayes_factor(bridge_sampler(fit2, silent = TRUE),
                       bridge_sampler(fit1, silent = TRUE))
print(bf.2_1) #BF in favor of model 2: 

bf.2_tl <- bayes_factor(bridge_sampler(fit2, silent = TRUE),
                        bridge_sampler(tl_fit, silent = TRUE))
print(bf.2_tl) #BF in favor of model 2: 1821627687958405896515918810588134719952546551556675007110428671458943530999606117169678155513856.00000

## M3 comparisons ----
bf.3_tl <- bayes_factor(bridge_sampler(fit3, silent = TRUE),
                        bridge_sampler(tl_fit, silent = TRUE))
print(bf.3_tl) #BF in favor of model 3: 204110383584057263203661603408261102783182484765924533267041282481334922885900119118084970460948299713209226480798662656.00000

bf.3_2 <- bayes_factor(bridge_sampler(fit3, silent = TRUE),
                       bridge_sampler(fit2, silent = TRUE))
print(bf.3_2) #BF in favor of model 3: 46293076263295829147648.00000

# PLOTTING  TL -----------------------------------------------------------------
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

# requires bite, run_hide, regurgitate, vocalize, kick in that order
model_p <- list(bite = bite_p, 
                run_hide = run_p,
                regurgitate = regurg_p,
                vocalize = vocal_p,
                kick = kick_p)

tl_p <- plot_param(model_p, is_probs = TRUE, plot_title = 'TL M1')


# PLOTTING EXP -----------------------------------------------------------------
bite_p <- rep(0,N)
run_p <- rep(0,N)
regurg_p <- rep(0,N)
vocal_p <- rep(0,N)
kick_p <- rep(0,N)

for (i in 1:N) {
  bite_p[i] <- mean(params_exp$p[,i,1])
  run_p[i] <- mean(params_exp$p[,i,2])
  regurg_p[i] <- mean(params_exp$p[,i,3])
  vocal_p[i] <- mean(params_exp$p[,i,4])
  kick_p[i] <- mean(params_exp$p[,i,5])
}

# requires bite, run_hide, regurgitate, vocalize, kick in that order
model_p <- list(bite = bite_p, 
                run_hide = run_p,
                regurgitate = regurg_p,
                vocalize = vocal_p,
                kick = kick_p)

exp_p <- plot_param(model_p, is_probs = TRUE, plot_title = 'EXP M1')

# PLOTTING M1 -----------------------------------------------------------------
bite_p <- rep(0,N)
run_p <- rep(0,N)
regurg_p <- rep(0,N)
vocal_p <- rep(0,N)
kick_p <- rep(0,N)

for (i in 1:N) {
  bite_p[i] <- mean(params1$p[,i,1])
  run_p[i] <- mean(params1$p[,i,2])
  regurg_p[i] <- mean(params1$p[,i,3])
  vocal_p[i] <- mean(params1$p[,i,4])
  kick_p[i] <- mean(params1$p[,i,5])
}

# requires bite, run_hide, regurgitate, vocalize, kick in that order
model_p <- list(bite = bite_p, 
                run_hide = run_p,
                regurgitate = regurg_p,
                vocalize = vocal_p,
                kick = kick_p)

m1_p <- plot_param(model_p, is_probs = TRUE, plot_title = 'M1')

pdf(file = './plots/p_plots.pdf')
tl_p
exp_p
m1_p
dev.off()