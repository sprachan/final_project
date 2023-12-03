##--- Setup: Load dependencies, set plotting parameters ----
#### Requires previous installation of some tidyverse packages: 
#:::: ggplot2, dplyr
library(ggplot2)
library(dplyr)
library(viridis)
library(lubridate)

#set ggplot theme to light
ggplot2::theme_set(theme_light())

source('./scripts/functions.R')

# ===== Load and Wrangle Data ========================================
source('./scripts/data_wrangling.R')

#===== Stats ===========================
source('./scripts/bootstrap.R')
# ===== Plotting =====================================================
#source('./scripts/plotting.R')

