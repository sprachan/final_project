library(tidyverse)
library(rstan)
library(patchwork)
library("bridgesampling")
library("loo")

# Load Data
load('./data/behav_ind_raw.RData')
load('./data/behav_ind_summarized.RData')
behav_ind_raw <- mutate(behav_ind_raw,
                        run_hide = run_and_hide + hide)
behav_ind_raw <- na.omit(behav_ind_raw)
attach(behav_ind_raw)


make_param_df <- function(list_in){
  temp <- list('regurgitate' = list_in[,1], 
               'vocalize' = list_in[,2],
               'kick' = list_in[,3])
  out <- enframe(temp, name = 'behavior', value = 'modelled_value') |>
    unnest_longer(modelled_value)
  return(out)
}

plot_param <- function(list_in, plot_title = NULL){
  temp <- make_param_df(list_in)
  print(temp)
  p <- ggplot(temp)+
    geom_histogram(aes(x = modelled_value),
                   bins = 50,
                   fill = 'lightblue',
                   color = 'black',
                   linewidth = 0.25)+
    #geom_vline(xintercept=mean(temp$modelled_value))+ # trying to add line at the mean
    facet_wrap(facets = vars(behavior),
               nrow = 5)+
    labs(title = plot_title, x = NULL, y=NULL)
  return(p)
}

model_obj <- list(B = 3,
                  N = length(band),
                  #band = as.numeric(band),
                  #tarsus = as.numeric(tarsus_length),
                  #weight = as.numeric(weight),
                  #wing = as.numeric(wing_length),
                  #sex = as.integer(numeric_sex),
                  #experience = as.integer(years),
                  #exttime = as.numeric(ext_time),
                  #passive = passive,
                  bite = bite,
                  run_hide = run_hide,
                  regurgitate = regurgitate,
                  vocalize = vocalize,
                  kick=kick
)
model = stan_model('./scripts/m2.stan')
model0 = stan_model('./scripts/m0.stan')
fit = sampling(model, model_obj, iter = 10000, chains = 1)
fit0 = sampling(model0, model_obj, iter = 10000, chains = 1)
params = rstan::extract(fit)
params0 = rstan::extract(fit0)

# Plot -------------------------------------------------------------------------
none <- plot_param(params$p, 'Neither')
b <- plot_param(params$p_b, 'Bite Only')
r <- plot_param(params$p_r, 'Run Only')
both <- plot_param(params$p_br, 'Both Bite & Run')
plot.m0 <- plot_param(params0$p[,3:5], 'Model 0')


none | b | r | both | plot.m0

bf.20 <- bayes_factor( bridge_sampler(fit, silent = TRUE),bridge_sampler(fit0, silent = TRUE))
print(bf.20)

pdf(file = './plots/m2_param_plots.pdf')
p_beta_wt
p_beta_tl
p_beta_wl
dev.off()

pairs(x=fit, pars=c("coeff_wt","coeff_tl"))

