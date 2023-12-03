# Load Packages ================================================================
library(dplyr)

# Read and Tidy Data =========================================================== 

# Behavioral data (collected 2023)
behav_data <- read.csv('./data/behav_data.csv') |>
              mutate(band = as.character(band))

historic_data <- read.csv('./data/historic_data.csv') |>
                 select(Year, 
                        Burrow, 
                        Adult.1.ID, 
                        Adult.2.ID) |>
                 mutate(Adult.2.ID = as.character(Adult.2.ID))

# Read in 2022 data, only take columns I need; rename columns for consistency
data_2022 <- read.csv('./data/petrel_data2022.csv') |>
             select(Band, 
                    ATY.or.Chick, 
                    Burrow) |> 
             rename('band'= Band) |>
             mutate(band = as.character(band))

# Rename columns for consistency
sex_data <- read.csv('./data/petrel_sex.csv') |> 
            select(-Notes) |>
            rename('band' = Band, 'sex' = Sex) |>
            mutate(band = as.character(band))

# 2023 physical characteristic data
phys_data <- read.csv('./data/data_2023.csv') |>
             select(Band, 
                    Burrow, Mass..g., 
                    Wing.Chord..mm., 
                    Tarsus.Length..mm.,
                    Known.Sex,
                    Blood.for.PCR., 
                    Feathers.Collected.) |>
  # clean up names
             rename(weight = Mass..g.,
                    wing_length = Wing.Chord..mm.,
                    tarsus_length = Tarsus.Length..mm.,
                    prev_sex = Known.Sex,
                    blood = Blood.for.PCR.,
                    feathers = Feathers.Collected.,
                    band = Band,
                    burrow = Burrow) |>
  # make feather data binary
             mutate(feathers = case_when(feathers == 'Yes' ~ 1,
                                         feathers == 'No' ~ 0,
                                         is.na(feathers) ~ 0)) |>
             distinct()

#===== Compile individual metadata =============================================
bands <- unique(behav_data$band[!is.na(behav_data$band)])

individuals <- matrix(0, nrow=length(bands), ncol=2)
individuals[,1] <- bands



# Calculate years appeared for each individual (each band number)
for(i in 1:length(bands)){
  b <- bands[i]
  
  # count the number of times current band shows up in historic data
  h <- sum(historic_data$Adult.1.ID==b, historic_data$Adult.2.ID==b, na.rm=TRUE)
  
  # if a band appeared at all in 2022, set t to 1 (for 1 occurrence)
  #> otherwise, set t to 0
  # this accounts for any bands that were (for whatever reason) repeated
  #> in the 2022 data
  t <- sum(data_2022$band == b)
  if(t > 0){
    t <- 1
  }else{
    t = 0
  }
  
  #occurrences is the number of historic data appearances+2022 appearances
  #> for band i
  o <- h+t
  
  #if o is 0, check if we banded it this year (band starts with 3081-02)
  #... If we did, set
  #.. o to 0; if not, set o to NA.
  
  if(o == 0){
    if(grepl('308102', b)){
      o <- 0
    }else{
      o <- NA
    }
  }
  #store the number of occurrences (o) in the appropriate place in the storage
  #...matrix
  individuals[i,2] <- o
}
#turn the storage matrix into a data frame
individuals <- data.frame(band = individuals[,1], 
                          years = individuals[,2]) |>
               mutate(years = as.integer(years)) |>
               left_join(sex_data, by = join_by(band)) |>
               left_join(phys_data, by = join_by(band))


 

#===== Make a by-individual behavioral data frame ====================
# each row contains behavior AND individual information

# "raw" version: each observation is a row; that is, each time an individual
#> is caught, it gets a new row
behav_ind_raw <- behav_data |> select(band, 
                                      passive, 
                                      kick, 
                                      bite, 
                                      run_and_hide, 
                                      hide,
                                      regurgitate, 
                                      vocalize, 
                                      ext_time) |> 
                                 full_join(individuals, by = join_by(band)) |>
                                 # clean up sexes
                                 mutate(sex = case_when(sex == 'M' ~ 'M',
                                                        sex == 'M (corrected)' ~ 'M',
                                                        sex == 'Probable M' ~ 'M',
                                                        sex=='Probable Male (corrected)'~'M',
                                                        sex == 'F' ~ 'F',
                                                        sex == 'Probable F' ~ 'F',
                                                        sex == 'F ' ~ 'F'),
                                         numeric_sex = case_when(is.na(sex) ~ 0,
                                                                 sex == 'M' ~ 1,
                                                                 sex == 'F' ~ 2)
                                       )
              
  



# "summarized" version: each individual is a row
behav_ind_summ <- behav_data |> select(band, 
                                       passive, 
                                       kick, 
                                       bite, 
                                       run_and_hide, 
                                       hide,
                                       regurgitate, 
                                       vocalize, 
                                       ext_time) |> 
                                 group_by(band) |>
                                 summarize(num_observations = n(),
                                           passive = sum(passive),
                                           kick = sum(kick),
                                           bite = sum(bite),
                                           run_and_hide = sum(run_and_hide),
                                           hide = sum(hide),
                                           regurgitate = sum(regurgitate),
                                           vocalize = sum(vocalize),
                                           avg_ext_time = mean(ext_time)
                                          ) |>
                                 full_join(individuals, by = join_by(band)) |>
                                 # clean up sexes
                                 mutate(sex = case_when(sex == 'M' ~ 'M',
                                                        sex == 'M (corrected)' ~ 'M',
                                                        sex == 'Probable M' ~ 'M',
                                                        sex=='Probable Male (corrected)'~'M',
                                                        sex == 'F' ~ 'F',
                                                        sex == 'Probable F' ~ 'F',
                                                        sex == 'F ' ~ 'F'),
                                        numeric_sex = case_when(is.na(sex) ~ 0,
                                                                sex == 'M' ~ 1,
                                                                sex == 'F' ~ 2)
                                       )
                     
save(behav_ind_raw, file = './data/behav_ind_raw.RData')
save(behav_ind_summ, file = './data/behav_ind_summarized.RData')


