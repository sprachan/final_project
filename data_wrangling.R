#====== Reading in Data =================================================
# Behavioral data (collected 2023)
behav.data <- read.csv('./data/behav_data.csv')

historic.data <- read.csv('./data/historic_data.csv') |>
                 select(Year, Burrow, Adult.1.ID, Adult.2.ID) #only take the relevant columns

# Read in 2022 data, only take columns I need; rename columns for consistency
data.2022 <- read.csv('./data/petrel_data2022.csv') |>
             select(Band, ATY.or.Chick, Burrow) |> 
             rename('band'=Band)

# Rename columns for consistency
sex.data <- read.csv('./data/petrel_sex.csv') |> 
            rename('band' = Band, 'sex' = Sex)

#====== Make Column names and types consistent ============================

# Make sure bands are characters, not integers!
behav.data$band <- as.character(behav.data$band)

# ATY1 bands are already as character vectors,
#::: so only need to make sure that Adult 2 bands are 
#::: character vectors
historic.data$Adult.2.ID <- as.character(historic.data$Adult.2.ID)

# Make sure band is a character vector in 2022 and petrel sex data
data.2022$band <- as.character(data.2022$band)
sex.data$band <- as.character(sex.data$band)

#===== Calculate Years Experience ======================================
##Goal: Create a data frame ("individuals") that has a row for each individual
##...with a column that contains an integer for the years experience
##...for each bird, where years experience is the number of times the bird
##...has been captured and grubbed in the historic data through 2022, 
##... not including 2020 and 2021, which had little data collected with
##...different protocols.

# Get a vector of the unique bands in the behavioral data; get rid of any NAs
#...of which there shouldn't be any, but just in case.
bands <- unique(behav.data$band[!is.na(behav.data$band)])

# Set up a storage matrix. First column is the band, second column ...
# ...is the sum of the times the bird appeared in historic
individuals <- matrix(0, nrow=length(bands), ncol=2)
individuals[,1] <- bands
#column 2 = appearances in historic and 2022

# Calculate years appeared for each individual (each band number)
for(i in 1:length(bands)){
  #get the current band number
  b <- bands[i]
  
  #count the number of times current band shows up in historic data
  h <- sum(historic.data$Adult.1.ID==b, historic.data$Adult.2.ID==b, na.rm=TRUE)
  
  #if a band appeared at all in 2022, set t to 1 (for 1 occurrence)
  #... otherwise, set t to 0
  # this accounts for any bands that were (for whatever reason) repeated
  # ...in the 2022 data
  t <- sum(data.2022$band==b)
  if(t >0){
    t <- 1
  }else{
    t=0
  }
  
  #occurrences is the number of historic data appearances+2022 appearances
  #... for band i
  o <- h+t
  
  #if o is 0, check if we banded it this year (band starts with 3081-02)
  #... If we did, set
  #.. o to 0; if not, set o to NA.
  
  if(o == 0){
    if(grepl('308102', b)){
      o <- 0
      cat(b, 'banded 2023', '   ')
    }else{
      o <- NA
    }
  }
  #store the number of occurrences (o) in the appropriate place in the storage
  #...matrix
  individuals[i,2] <- o
}
#turn the storage matrix into a data frame
individuals <- data.frame(band=individuals[,1], years=individuals[,2])
#make sure the years experience is coded as an integer
individuals$years <- as.integer(individuals$years)

#======= Estimate age ====================================
#set up a storage matrix
store <- matrix(0, nrow=length(bands), ncol=2)
#first column is the list of bands
store[,1] <- bands

#similar to getting years experience, but instead estimate age by taking
#...the first year the bird appeared on Kent as year 3 and adding to that
#...the number of years it has been since then.
for(i in 1:length(bands)){
  #get the current band number
  b <- bands[i]
  
  #find the all appearances of the band (rows)in historic data
  r <- which(historic.data$Adult.1.ID==b | historic.data$Adult.2.ID==b)
  
  #find the appearances of the band in 2022 data
  r22 <- which(data.2022$band==b)
  
  #if the band appeared in historic data, get the row
  #::: of the minimum year that appears
  if(length(r)>0){
    #get the minimum year value
    m <- historic.data[which(historic.data[,1]==min(historic.data[r,1])), 1][1]
    #get the age by counting the years between the first appearance and today and
    #...adding 3 (because we assume the bird is at least 3 when it first breeds)
    y <-2023-m+3
    
  }else if(grepl('308102', b)){
    #if the band didn't appear in historic data, first check 
    #...if the bird was banded in 2023. If it was, then set y to 3 (assuming
    #...that the bird is breeding for the first time in its 3rd year)
    y <- 3
  }else if(length(r22)>0){
    #otherwise, if it appeared in 2022 (but not in historic data and wasn't
    #...newly banded in 2023), set y to 4
    y <- 4
  }else{
    #otherwise, set to NA
    y <- NA
  }
  store[i,2] <- y
}
#turn the storage matrix into a data frame
store <- data.frame(band=store[,1], est_age=store[,2])

#add this age information to the individuals data frame
individuals <- left_join(individuals, store, by=join_by(band))
remove(store)

individuals$est_age <- as.integer(individuals$est_age)

#===== Get sexes =====================================================
#add sexes to individuals data frame
individuals <- left_join(individuals, sex.data, by=join_by(band))


#===== Make a by-individual behavioral data frame ====================
#Creates a data frame where each individual has a row, and the value
#...in each behavioral column (eg., passive, kick, bite) is the number of
#...times the bird exhibited that behavior. If we caught the bird once,
#...this will be a 0 or a 1; if we caught it multiple times, it will be
#...any integer from 0 to N where N is the number of times we caught it.
#Also in this data frame is the information we got in previous steps about
#...bird experience, estimated age, and sex, plus the number of times
#...we caught the bird.
#We also filter out any birds whose years experience we were unable
#...to calculate ie., any bird that (1) wasn't banded this year; (2) didn't
#...appear last year; (3) didn't appear in the historic data. That is,
#...this filters out any birds that were banded in 2020 and 2021.

behav.by.ind <- behav.data |> 
  select(band, passive, kick, bite, run_and_hide, hide,
         regurgitate, vocalize, ext_time)|> 
  group_by(band) |>
  summarize(num_observations=n(),
            passive=sum(passive),
            kick=sum(kick),
            bite=sum(bite),
            run_and_hide=sum(run_and_hide),
            hide=sum(hide),
            regurgitate=sum(regurgitate),
            vocalize=sum(vocalize),
            avg_ext_time=mean(ext_time)) |>
  full_join(individuals, by=join_by(band)) |>
  filter(!is.na(years))

##===== Clean up sexes ===========
behav.by.ind <- behav.by.ind |>
  mutate(sex=case_when(
    sex == 'M'|sex=='M (corrected)'|sex=='Probable M'~ 'M',
    sex=='Probable Male (corrected)'~'M',
    sex == 'F'|sex=='Probable F' | sex == 'F '~ 'F',
  ))

##===== Make a "raw" version of this data ==========
#where individuals caught multiple
#... times will have multiple rows
behav.by.ind.raw <- behav.data |>
  dplyr::select(band, passive, kick, bite, run_and_hide, hide,
         regurgitate, vocalize, ext_time, observation_date) |>
  group_by(band) |>
  arrange(observation_date) |>
  mutate(trial_number=row_number()) |>
  tidyr::pivot_longer(cols=c(passive, kick, bite, 
                             run_and_hide, hide,
                             regurgitate, vocalize), 
                      names_to='behavior_type',
                      values_to='observed') |>
  full_join(individuals, by=join_by(band),
            relationship='many-to-many') |>
  filter(!is.na(years)) |>
  mutate(sex=case_when(
    sex == 'M'|sex=='M (corrected)' ~ 'M',
    sex=='Probable Male (corrected)'~'M',
    sex == 'F' ~ 'F',
    sex == 'Probable M' & Notes== 'Partner F on 2023 PCR' ~'M',
    sex == 'Probable M' & Notes == 'Partner F in historic data' ~ NA,
    sex == 'Probable F' ~ 'F'
  ))

repeaters <- behav.data |> group_by(band) |>
  summarize(num_observations=n()) |>
  filter(num_observations>1)

##===== Calculating Bite-Run Score ===================================
#this is a behavioral score, with +1 meaning the individual always bit (never
#... ran)
#and -1 meaning the individual always ran (never bite).

behav.by.ind <- behav.by.ind |> 
  mutate(bite_run=(bite-(run_and_hide+hide))/num_observations)

#===== Add experience categories ===================================
behav.by.ind <- behav.by.ind |> 
  mutate(experience_cat = case_when(
    years<=1~'0-1 years',
    years>=2 ~ '2+ years')
  ) |> 
  mutate(experience_cat = as.factor(experience_cat))

behav.by.ind.raw <- behav.by.ind.raw |> 
  mutate(experience_cat = case_when(
    years<=1~'0-1 years',
    years>=2 ~ '2+ years')
  ) |> 
  mutate(experience_cat = as.factor(experience_cat))

#===== Make a dataframe with just the oldest birds ======================
oldest <- behav.by.ind.raw |>
  filter(years>=4,
         est_age>=7)

#===== DF with just the youngest birds ===================
youngest <- behav.by.ind.raw |>
  filter(years==0)

## ===== Calculate behavior frequencies based on years experience ==========
#Mean gets the number of times the behavior was observed standardized
#...by the number of times we caught the individuals
freq.by.years <- behav.by.ind |> group_by(experience_cat) |>
  summarize(passive=mean(passive), 
            kick=mean(kick),
            bite=mean(bite),
            run_and_hide=mean(c(run_and_hide, hide)),
            hide=mean(hide),
            regurgitate=mean(regurgitate),
            vocalize=mean(vocalize)
  )
#caveman solution to get this in a form good for plotting--will try and
#... make this more sophisticated later

exp_cat <- c(rep('0 years', 7), rep('1 year', 7), rep('2-3 years', 7), rep('4+years', 7))
beh <- rep(c('passive', 'kick', 'bite', 'run and hide', 'hide', 'regurgitate', 'vocalize'), 
           4)
prop <- c(freq.by.years[1,2:8], freq.by.years[2,2:8], 
          freq.by.years[3, 2:8], freq.by.years[4, 2:8]) |> as.numeric()

freq.years.plotting <- data.frame(experience_cat = exp_cat, behavior=beh, 
                                  proportion=prop)

### Same thing, finer grain ---------
#only get years experience where there's at least 3 birds in the group
#something strange is happening here.
freq.years.fine <- behav.by.ind |> group_by(years) |>
  filter(n()>=3) |>
  summarize(passive=sum(passive)/n(), 
            kick=sum(kick)/n(),
            bite=sum(bite)/n(),
            run_hide=sum(run_and_hide, hide)/(2*n()),
            regurgitate=sum(regurgitate)/n(),
            vocalize=sum(vocalize)/n()
  )


