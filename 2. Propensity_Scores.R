####=======================   2. PROPENSITY SCORES      =============================================####

####=======================    LOAD LIBRARIES       =============================================####

#### INSTALL LIBRARIES ####
library(tidyverse)
library(haven)
library(survey)
library(sf)
library(sp)
library(raster)
library(rgdal)
library(modeest)
library(RColorBrewer)
library(DAPSm)

#library(devtools)
#devtools::install_github("gpapadog/DAPSm")

####=======================    LOAD DATA       =============================================####

#### LOAD DATA ####
# already removed NA and cropped to Hanoi inner city 
load("survey_recoded_noNA_cropped_nonBinary.RData") 

####=======================    CASE STUDIES OF INTEREST      =============================================####

# FOR MODE OF CHOICE
# treatment = 12725 (motorbike) 
# control = 9452 (the rest)

# FOR AWARENESS OF BAN
# treatment = 13604 (yes) 
# control = 8573 (no/don't care)

# FOR FUTURE VEHICLE 
# treatment = 7037 (car) 
# control = 15140 (the rest)
####=======================    PREP DATA     =============================================####

# DAPSm with extensive search for the optimal weight - Recommended
#Instead, we can fit the algorithm for varying values of w and assess balance of covariates at every step. This can be performed by

# Because we are doing multiple case studies, do the below and run the rest of the code for each case study before moving onto the next (i.e., we are running code for one case study at a time)

#### AWARE BAN CASE STUDY #### - OUTCOME = opinion_ban　(aware = 1, no/don't care =0)
survey_recoded_noNA$prop.scores <- glm(aware_ban ~ age + occup + gender + purp + freqpweek + reason + own_car + own_motob + own_ebike + own_bike + status +own + reas_not_car + reas_not_motob + reas_not_ebike + reas_not_bike + vehic + fut_veh + alt_car + alt_ebike + alt_bus + alt_ltrain + alt_taxi + alt_walk + opinion_car + opinion_motob + opinion_ebike + opinion_bike + opinion_taxi + opinion_bus + travtime, 
                                       family = binomial, 
                                       data = survey_recoded_noNA)$fitted.values
#### MODE OF CHOICE #### - OUTCOME = opinion_ban (motorbike = 1, other =0)
survey_recoded_noNA$prop.scores <- glm(vehic ~ age + occup + gender + purp + freqpweek + reason + own_car + own_motob + own_ebike + own_bike + status + own + reas_not_car + reas_not_motob + reas_not_ebike + reas_not_bike + aware_ban + fut_veh + alt_car + alt_ebike + alt_bus + alt_ltrain + alt_taxi + alt_walk + opinion_car + opinion_motob + opinion_ebike + opinion_bike + opinion_taxi + opinion_bus + travtime, 
                                       family = binomial,
                                       data = survey_recoded_noNA)$fitted.values

#### FUTURE VEHICLE #### - OUTCOME = opinion_ban (car = 1, other =0)
survey_recoded_noNA$prop.scores <- glm(fut_veh ~ age + occup + gender + own_motob + purp + freqpweek + reason + own_car + vehic + own_ebike + own_bike + status + own + reas_not_car + reas_not_motob + reas_not_ebike + reas_not_bike + aware_ban  + alt_car + alt_ebike + alt_bus + alt_ltrain + alt_taxi + alt_walk + opinion_car + opinion_ban +  opinion_ebike + opinion_bike + opinion_taxi + opinion_bus + travtime, 
                                       family = binomial,
                                       data = survey_recoded_noNA)$fitted.values


####=======================    PLOT COMMON SUPPORT AREA     =============================================####
# repeat the below for each case study
labs <- paste("Own Motorbike", c("Yes", "No"))
survey_recoded_noNA %>%
  mutate(own_motob = ifelse(own_motob == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = prop.scores)) +
  geom_histogram(color = "white") +
  facet_wrap(~own_motob) +
  xlab("Probability of owning a motorbike") +
  theme_bw()


#### REORDER DATAFRAME  ####
survey_recoded_noNA <- survey_recoded_noNA %>%
  dplyr::select(c(own_motob, vehic, age, occup, gender, purp, reason, own_car, aware_ban, own_ebike, own_bike, status,
                  own, reas_not_car, reas_not_motob, reas_not_ebike, reas_not_bike, fut_veh, alt_car,
                  alt_ebike, alt_bus, alt_ltrain, alt_taxi, alt_walk, opinion_car, opinion_ban, opinion_motob,
                  opinion_ebike, opinion_bike, opinion_taxi, opinion_bus, travtime, freqpweek, x, y, prop.scores))

####=======================    SAVE      =============================================####

# SAVE
save(survey_recoded_noNA, file = "survey_prop.scores.RData")