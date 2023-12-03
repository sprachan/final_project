library(viridis)
v.blue <- viridis(3)[2]
v.blue.t <- viridis(3, alpha=0.5)[2]
v.purp.t <- viridis(1, alpha=0.5)
#===== Overall Distribution of Behaviors ===================================
behavs <- select(behav.data,
                 passive, 
                 kick, 
                 bite, 
                 run_and_hide,
                 hide, 
                 regurgitate, 
                 vocalize)

behav.summary <- data.frame(behavior=c('P', 'K', 'B', 'R', 'Hi', 'U', 'V'),
                         total= apply(behavs, MARGIN=2, FUN=sum),
                         frequency=apply(behavs, MARGIN=2, FUN=mean),
                         row.names=NULL) |>
                  mutate(proportion=total/sum(total))

ggplot(behav.summary)+
  geom_col(aes(x=behavior, y=frequency), 
           color='black', 
           linewidth=0.25, 
           fill='midnightblue')+
  xlab('Behavior')+
  ylab('Frequency')
ggsave('./plots/behav_summ_frequency.png', 
       height=4, width=4, units='in')

# ===== Distribution of Experiences and Ages ====================================
#### Distribution of Experiences -----
filter(individuals, !is.na(years)) |>
  ggplot()+
  geom_histogram(aes(x=years), 
                 bins=15,
                 fill=v.blue, 
                 color='black', 
                 linewidth=0.25)
ggsave('./plots/experience_distribution_hist.png', height=3, width=5, units='in')

#### Distribution of Ages -----
filter(individuals, !is.na(est_age)) |>
  ggplot()+
  geom_histogram(aes(x=est_age), 
                 bins=15,
                 fill=v.blue, 
                 color='black')

### Experiences and Ages, together -----
  filter(individuals, !is.na(years), !is.na(est_age)) |>
  ggplot()+
  geom_histogram(aes(x=est_age), 
                 bins=15, 
                 fill= v.blue.t,
                 color='black')+
  geom_histogram(aes(x=years), 
                 bins=15, 
                 fill=v.purp.t, 
                 color='black')

### Experience vs Age -----
#Some birds don't show up every year, so this won't be a perfect line.
ggplot(behav.by.ind, aes(x=years, y=est_age))+
  geom_count()+
  labs(x='Years of Experience', 
       y='Estimated Age')

#===== Behavior by Experience ================================
## Not standardized ========

#### Scatterplot, bite and run =======
ggplot(behav.by.ind, aes(x=years))+
  geom_jitter(aes(y=bite, color='Bite'),
              height=0.1,
              width=0.2)+
  geom_jitter(aes(y=run_and_hide+hide, color='Run'),
              height=0.1,
              width=0.2)

#### Count scatter, separate behaviors ======
##### bite ====
behav.by.ind.raw |>
  filter(behavior_type=='bite',
        !is.na(years)) |>
  ggplot(aes(x=years, y=observed))+
  geom_count()+
  labs(x='Years of Handling Experience', y='Observed Biting')

##### run ========
behav.by.ind.raw |>
  filter(behavior_type=='run_and_hide',
         !is.na(years)) |>
  ggplot(aes(x=years, y=observed))+
  geom_count()+
  labs(x='Years of Handling Experience', y='Observed Running/Hiding')

## Standardized =======
#### Biting proportion vs experience counts ==============
behav.by.ind|>
  group_by(band) |>
  summarize(bite=bite/num_observations) |>
  full_join(individuals, by=join_by(band)) |>
  filter(!is.na(years)) |>
  ggplot()+
  geom_count(aes(x=years, y=bite))+
  labs(y='Proportion Biting', x='Years of Handling Experience')+
  theme_bw()+
  theme(
    axis.title=element_text(size=25),
    axis.text=element_text(size=15)
  )
ggsave('./plots/bite-run_score_by_experience.png', width=12,
       height=6, units='in')
#### Running proportion vs experience counts ========
behav.by.ind |>
  group_by(band) |>
  summarize(run_hide=run_and_hide/num_observations) |>
  full_join(individuals, by=join_by(band)) |>
  filter(!is.na(years))|>
  ggplot()+
  geom_count(aes(x=years, y=run_hide))+
  labs(y='Proportion Running Responses', x='Years of Handling Experience')+
  theme_linedraw()+
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 13)
  )

#### Vocalizing proportion vs experience counts =====
behav.by.ind |>
  group_by(band) |>
  summarize(vocalize=vocalize/num_observations) |>
  full_join(individuals, by=join_by(band)) |>
  filter(!is.na(years))|>
  ggplot()+
  geom_count(aes(x=years, y=vocalize))

#### Regurgitating proportion vs experience counts ======
behav.by.ind |>
  group_by(band) |>
  summarize(regurgitate=regurgitate/num_observations) |>
  full_join(individuals, by=join_by(band)) |>
  filter(!is.na(years))|>
  ggplot()+
  geom_count(aes(x=years, y=regurgitate))

#### Bite and Run vs experience counts =======
# p <- behav.by.ind |>
#   group_by(band) |>
#   summarize(run_hide=run_and_hide/num_observations,
#             bite=bite/num_observations) |>
#   full_join(individuals, by=join_by(band)) |>
#   filter(!is.na(years))|>
#   ggplot()


p <- behav.by.ind.raw|>
  select(band, behavior_type, observed, years) |> 
  arrange(band) |> 
  dplyr::filter(behavior_type=='bite'|behavior_type=='run_and_hide') |> 
  mutate(behavior_type=recode(behavior_type, 
                              'bite'='Bite', 'run_and_hide'='Run/Hide')) |>
  group_by(band, behavior_type) |> 
  mutate(num_observations=n()) |>
  summarize(observed_raw=sum(observed),
            observed_std=sum(observed)/num_observations[1],
            num_observations=num_observations[1],
            years=years[1]) |>
  ggplot()
##### faceted --------
p+geom_count(aes(x=years, y=observed_std))+
  facet_wrap(facets=vars(behavior_type), nrow=2)+
  theme_bw()+
  labs(x='Years of Handling Experience',
       y='Proportion of Visits')

##### overlay -----------
p+geom_jitter(aes(x=years, y=observed_std, color=behavior_type,),
              alpha=0.6,
              size=3)+
  theme_bw()+
  labs(x='Years of Handling Experience',
       y='Proportion of Visits with Behavior',
       color='Behavior')+
  theme(axis.title=element_text(size=20),
        axis.text=element_text(size=15),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
ggsave('./plots/scatter_combined.png', width=12,
       height=6, units='in')
#### Boxplot, bite-run vs experience =======

behav.by.ind |>
  ggplot(aes(x=experience_cat, y=bite_run))+
  geom_jitter(shape=21, 
              size=4,
             color='black',
             fill='black',
             alpha=0.5,
             width=0.25)+
  geom_segment(aes(x=0.75, y=mean(no.experience), 
                   xend=1.25, yend=mean(no.experience)),
               linetype=2,
               linewidth=1.25,
               color='red')+
  geom_segment(aes(x=1.75, y=mean(experienced),
                   xend=2.25, yend=mean(experienced)),
               linetype=2,
               linewidth=1.25,
               color='red')+
  theme_bw()+
  theme(axis.title=element_text(size=20),
        axis.text=element_text(size=15))+
  labs(
    x='Experience Level',
    y='Bite-Run Score'
  )

ggsave('./plots/bite-run_score_by_experience.png', width=12,
       height=6, units='in')

#Behavior vs Sex ===========================================
behav.by.ind.raw |>
  group_by(band) |>
  filter(!is.na(sex),
         behavior_type!='regurgitate',
         behavior_type!='kick',
         behavior_type!='vocalize') |>
  mutate(observed=as.factor(observed)) |>
  ggplot()+
  geom_bar(aes(x=observed, fill=sex), 
           position='dodge')+
  facet_wrap(facets=vars(behavior_type))

#### Running
behav.by.ind |>
  group_by(band) |>
  summarize(run_hide=run_and_hide/num_observations) |>
  full_join(individuals, by=join_by(band)) |>
  filter(!is.na(years))|>
  ggplot()+
  geom_count(aes(x=years, y=run_hide))

behav.by.ind |>
  filter(!is.na(sex)) |>
  ggplot(aes(x=sex, y=bite_run))+
  geom_jitter(alpha=0.5, width=0.25, size=2)+
  geom_segment(aes(x=0.75, xend=1.25,
                   y=mean(females), yend=mean(females)),
               linetype=2,
               linewidth=1.25,
               color='red')+
  geom_segment(aes(x=1.75, xend=2.25,
                   y=mean(males), yend=mean(males)),
               linetype=2,
               linewidth=1.25,
               color='red')+
  theme_bw()+
  theme(axis.title=element_text(size=30),
        axis.text=element_text(size=20))+
  labs(x='Sex', y='Bite-Run Score')
ggsave('./plots/bite-run_by_sex.png', width=12, height=6, units='in')

#===== Sex vs Experience ====================================
filter(behav.by.ind, !is.na(sex)) |>
  group_by(years, sex) |>
  summarize(count=n()) |> 
  ungroup() |>
  group_by(years) |>
  mutate(proportion=count/sum(count)) |>
  ggplot()+
  geom_col(aes(x=years, y=proportion, fill=sex),
           color='black')+
  labs(x='Years of Experience', y='Count')

filter(behav.by.ind, !is.na(sex)) |>
  group_by(experience_cat, sex) |>
  summarize(count=n()) |>
  ungroup() |>
  group_by(experience_cat) |>
  mutate(proportion=count/sum(count)) |>
  ggplot()+
  geom_col(aes(x=experience_cat, y=proportion, fill=sex),
           width=0.5, color='black')+
  labs(x='Experience Category', y='Proportion')+
  scale_fill_manual(values=c('lightblue', 'midnightblue'))
ggsave('./plots/sex_by_experience.png', width=3, height=2, units='in')

#===== Repeated Behaviors =========================================
behav.data |>
  dplyr::select(band, passive, kick, bite, run_and_hide, hide,
                regurgitate, vocalize, ext_time, observation_date) |>
  filter(band %in% repeaters$band) |>
  group_by(band) |>
  arrange(observation_date) |> 
  mutate(trial_number=row_number()) |>
  tidyr::pivot_longer(cols=c(passive, kick, bite, 
                             run_and_hide, hide,
                             regurgitate, vocalize), 
                      names_to='behavior_type',
                      values_to='observed') |>
  mutate(observed = as.factor(observed)) |>
  full_join(individuals, by=join_by(band),
            relationship='many-to-many') |>
  filter(!is.na(years),
         behavior_type == 'bite'|behavior_type =='run_and_hide') |>
  ggplot(aes(x=trial_number))+
  geom_tile(aes(y=band, fill=observed))+
  facet_wrap(facets=vars(behavior_type),
             nrow=1)+
  scale_fill_manual(values=c('midnightblue', 'lightblue'))+
  labs(x='Trial Number',
       y='Band')
ggsave('./plots/behavior_repeaters.png', height=4, width=6,
       units='in')
#### age as function of aggression and experience
