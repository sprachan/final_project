library(tidyverse)
library(rstan)

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
fit = sampling(wt_model, model_obj, iter = 10000, chains = 1)
params = rstan::extract(fit)

# Plot -------------------------------------------------------------------------
# params_only <- params[which(!(names(params) %in% c('lp__', 'p')))] # coefficients only, no log prior
# plots <- imap(params_only, \(x, idx) plot_param(list_in = x, plot_title = idx))
# 
# # adjust x limits so 0 is centered
# plots$coeff_wt <- plots$coeff_wt+xlim(c(-0.75, 0.75))
# plots$coeff_tl <- plots$coeff_tl+xlim(c(-2, 2)) 
# plots$coeff_wl <- plots$coeff_wl+xlim(c(-0.4, 0.4))
# 
# # Commented out for now because I have already plotted these guys
# pdf(file = './plots/m1_param_plots.pdf')
# plots
# dev.off()

# Confidence Intervals on the Parameters ---------------------------------------
# # 90% CI
# ci90_df <- map(params_only, make_param_df) |>
#   map(\(x) group_by(x, behavior)) |>
#   map(\(x) summarize(x, 
#                      lower = quantile(value, 0.05),
#                      upper = quantile(value, 0.95)))
# lapply(ci90_df, print)
# # all CI's contain 0 except the WL coefficient with kick
# 
# # 95% CI
# ci95_df <- map(params_only, make_param_df) |>
#            map(\(x) group_by(x, behavior)) |>
#            map(\(x) summarize(x, 
#                               lower = quantile(value, 0.025),
#                               upper = quantile(value, 0.975)))
# lapply(ci95_df, print)
# # again, all CI's contain 0 except the WL coefficient with kick 
# 
# # 99% CI
# ci99_df <- map(params_only, make_param_df) |>
#            map(\(x) group_by(x, behavior)) |>
#            map(\(x) summarize(x, 
#                               lower = quantile(value, 0.005),
#                               upper = quantile(value, 0.995)))
# 
# lapply(ci99_df, print)
# lose significance with kick/WL coefficient 

# Look at generated ps ---------------------------------------------------------
## WANT: slice the 3-dimensional parameter p output into 5 5000x174 matrices
#bite_p <- params$p[, , 1] # SOMETHING IS WRONG BECAUSE WE HAVE P'S OUTSIDE [0,1]
