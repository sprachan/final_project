library(tidyverse)
library(rstan)
library(bridgesampling)
library(loo)
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
       geom_vline(aes(xintercept = 0),
                  linetype = 'dashed',
                  col = '#dd3040')+
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
experience_model = stan_model('./scripts/m1_experience.stan')
fit_exp = sampling(experience_model, model_obj, iter = 10000, chains = 1)
params_experience = rstan::extract(fit_exp)

tl_model = stan_model('./scripts/m1_tl.stan')
fit_tl = sampling(tl_model, model_obj, iter = 10000, chains = 1)
params_tl = rstan::extract(fit_tl)
# Plot -------------------------------------------------------------------------
params_only <- params[which(!(names(params) %in% c('lp__', 'p')))] 
plots <- imap(params_only, \(x, idx) plot_param(list_in = x, plot_title = idx))

# save plots
pdf(file = './plots/m1/m1_experience_params.pdf')
plots
dev.off()

# Confidence Intervals on the Parameters ---------------------------------------
# 90% CI
ci90_df <- map(params_only, make_param_df) |>
  map(\(x) group_by(x, behavior)) |>
  map(\(x) summarize(x,
                     lower = quantile(value, 0.05),
                     upper = quantile(value, 0.95)))
lapply(ci90_df, print)
# all CI's contain 0 at this level except regurgitate intercept

# 95% CI
ci95_df <- map(params_only, make_param_df) |>
           map(\(x) group_by(x, behavior)) |>
           map(\(x) summarize(x,
                              lower = quantile(value, 0.025),
                              upper = quantile(value, 0.975)))
lapply(ci95_df, print)
# all CI's contain 0 at this level

# 99% CI
ci99_df <- map(params_only, make_param_df) |>
           map(\(x) group_by(x, behavior)) |>
           map(\(x) summarize(x,
                              lower = quantile(value, 0.005),
                              upper = quantile(value, 0.995)))

lapply(ci99_df, print)
# all CI's contain 0 at this level

# Look at generated ps ---------------------------------------------------------
t <- dim(params$p)[1]
N <- dim(params$p)[2]
B <- dim(params$p)[3]
behavs <- model_obj[11:15]
mse <- matrix(NA, nrow = N, ncol = B)

for(n in 1:N){
  for(b in 1:B){
    x <- behavs[[b]][n]
    prop <- params$p[,n,b]
    mse[n, b] <- (1/t)*sum((prop-x)^2)
  }
}

pdf(file = './plots/m1/m1_experience_mse.pdf')
plot(-log10(mse[,1]), 
     type = 'h',
     main = 'Bite',
     xlab = 'Individual',
     ylab = '-log10(MSE)')

plot(-log10(mse[,2]), 
     type = 'h',
     main = 'Run-Hide',
     xlab = 'Individual',
     ylab = '-log10(MSE)')

plot(-log10(mse[,3]),
     type = 'h',
     main = 'Regurgitate',
     xlab = 'Individual',
     ylab = '-log10(MSE)')

plot(-log10(mse[,4]),
     main = 'Vocalize',
     type = 'h',
     xlab = 'Individual',
     ylab = '-log10(MSE)')

plot(-log10(mse[,5]),
     main = 'Kick',
     type = 'h',
     xlab = 'Individual',
     ylab = '-log10(MSE)')
dev.off()

# Bayes Factor -----------------------------------------------------------------
bf_exp_tl <- bayes_factor(bridge_sampler(fit_exp, silent = TRUE),
                          bridge_sampler(fit_tl, silent = TRUE))
print(bf_exp_tl) # BF in favor of experience over tl is about 3