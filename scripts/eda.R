library(tidyverse)
library(corrplot)
load('./data/behav_ind_raw.RData')

behav_ind_raw <- behav_ind_raw |> mutate(run_hide = hide+run_and_hide)

behavs <- cbind(behav_ind_raw$run_hide,
                behav_ind_raw$regurgitate,
                behav_ind_raw$vocalize,
                behav_ind_raw$kick,
                behav_ind_raw$bite)
b <- c('run/hide', 'regurgitate', 'vocalize', 'kick', 'bite')
behav_cor <- cor(behavs)
colnames(behav_cor) <- b
rownames(behav_cor) <- b
corrplot(behav_cor, method = 'color',
         outline = TRUE, 
         col = COL2('PRGn'),
         tl.col = 'black')

# 5 chains, 50,000 iterations, get an ESS of 5000 for each parameter before running the BF

