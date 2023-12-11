library(tidyverse)
library(rstan)

# Functions --------------------------------------------------------------------
make_param_df <- function(list_in){
  temp <- list('bite' = list_in[,1], 
               'run_hide' = list_in[,2],
               'regurgitate' = list_in[,3],
               'vocalize' = list_in[,4],
               'kick' = list_in[,5])
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
       labs(title = plot_title, x = 'Modelled Value', y = 'Count')
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
model = stan_model('./scripts/m1.stan')
fit = sampling(model, model_obj, iter = 10000, chains = 1)
params = rstan::extract(fit)

# Plot -------------------------------------------------------------------------
params_only <- params[which(names(params) != 'lp__')]
plots <- imap(params_only, plot_param) 
plots$coeff_tl <- plots$coeff_tl+xlim(c(-2, 2))
plots$coeff_wl <- plots$coeff_wl+xlim(c(-0.4, 0.4))

pdf(file = './plots/m1_param_plots.pdf')
plots
dev.off()

# Confidence Intervals on the parameters ---------------------------------------




