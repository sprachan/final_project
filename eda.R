library(tidyverse)
library(corrplot)
load('./data/behav_data_individual.RData')

behav.by.ind <- behav.by.ind |> mutate(run_hide = hide+run_and_hide)

behavs <- cbind(behav.by.ind$run_hide,
                behav.by.ind$regurgitate,
                behav.by.ind$vocalize,
                behav.by.ind$kick,
                behav.by.ind$bite)
b <- c('run/hide', 'regurgitate', 'vocalize', 'kick', 'bite')
behav_cor <- cor(behavs)
colnames(behav_cor) <- b
rownames(behav_cor) <- b
corrplot(behav_cor, method = 'color',
         outline = TRUE, 
         col = COL2('PRGn'))

# 5 chains, 50,000 iterations, get an ESS of 5000 for each parameter before running the BF

