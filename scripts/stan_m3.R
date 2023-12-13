library(tidyverse)
library(rstan)
library(patchwork)

# Load Data --------------------------------------------------------------------
load('./data/behav_ind_raw.RData')
load('./data/behav_ind_summarized.RData')
behav_ind_raw <- mutate(behav_ind_raw,
                        run_hide = run_and_hide + hide)
behav_ind_raw <- na.omit(behav_ind_raw)
attach(behav_ind_raw)

# Functions --------------------------------------------------------------------
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
  p <- ggplot(temp)+
    geom_histogram(aes(x = modelled_value),
                   bins = 100,
                   fill = 'lightblue',
                   color = 'black',
                   linewidth = 0.25)+
    facet_wrap(facets = vars(behavior),
               nrow = 5)+
    geom_vline(aes(xintercept = 0),
               linetype = 'dashed',
               col = '#dd3040')+
    labs(title = plot_title, x = 'Modelled Value', y=NULL)
  return(p)
}

# STAN  --------------------------------------------------------------------
model_obj <- list(B = 3,
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
model = stan_model('./scripts/m3.stan')

fit = sampling(model, model_obj, iter = 10000, chains = 1)
params = rstan::extract(fit)

# Plot -------------------------------------------------------------------------
beta_tl <- plot_param(params$coeff_tl, 'Tarsus')
beta0 <- plot_param(params$beta0, 'Intercept')


beta_tl_r <- plot_param(params$coeff_tl_r, 'Tarsus: RUN')
beta0_r <- plot_param(params$beta0_r, 'Intercept: RUN')

beta_tl_b <- plot_param(params$coeff_tl_b, 'Tarsus: BITE')
beta0_b <- plot_param(params$beta0_b, 'Intercept: BITE')

beta_tl_br <- plot_param(params$coeff_tl_br, 'Tarsus: BITE & RUN')
beta0_br <- plot_param(params$beta0_br, 'Intercept: BITE & RUN')

none <- beta0 | beta_tl 
bite <- beta0_b | beta_tl_b
run <- beta0_r | beta_tl_r 
both <- beta0_br | beta_tl_br 

none / bite / run / both

#bf.2a <- bayes_factor( bridge_sampler(fit, silent = TRUE),bridge_sampler(fit.a, silent = TRUE))
#print(bf.2a)

pdf(file = './plots/m3_param_plots.pdf')
none 
bite
run
both
dev.off()

pairs(x = fit, 'p_v', 'p_k', 'p_r')

