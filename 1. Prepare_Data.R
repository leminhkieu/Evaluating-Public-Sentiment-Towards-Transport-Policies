####=======================   1. PREPARE DATA      =============================================####

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
survey <- read_csv("~/Dropbox/My Mac (Rikas-MBP.hub)/Documents/Research_Assistant/data/hn_survey_raw.csv")
hanoi <- st_read("HN.shp")
hanoinner <- st_read("hanoi_innercity.shp")

survey <- st_read("croppedInnercitySHP.shp") #19219 observations 

####=======================    REMOVE NA AND UNNECESSARY COLUMNS       =============================================####

# check NA
survey_NA <- colMeans(is.na(survey))*100
# --> remove columns that have more than 10% NA values in the column 

# removed variables that are more than 10%

# freq_car                      # hosp_acc
# freq_motob                    # leis_acc
# freq_ebike                    # bank_acc
# freq_bike
# freq_taxi
# freq_bus
# school_acc
# market_acc

##### remove the above variables 

survey_noNA <- survey %>%
  dplyr::select(-c(freq_car, freq_motob, freq_ebike, freq_bike, freq_taxi, freq_bus, school_acc,
                   market_acc, hosp_acc, leis_acc, bank_acc))

# remove unneeded columns 
survey_noNA <- survey_noNA %>%
  dplyr::select(-c(id, origlat, origlon, destlat, destlon, index)) 

###### BELOW 1 AFTER DOING VARIABLE IMPORTANCE SO REMOVE BOTTOM TWO VARIABLES
# - type

# REMOVE
survey_noNA <- survey_noNA %>%
  dplyr::select(-(type))

# also remove alt_veh because difficult to recode and all the information is captured in alt_bus, alt_car, alt_ebike, alt_bike, alt_ltrain, alt_taxi, alt_walk
survey_noNA <- survey_noNA %>%
  dplyr::select(-(alt_veh))

# REMOVE ANY NA ROWS 
survey_noNA <- survey_noNA %>%
  drop_na()

####=======================    CROP DATA TO INNER CITY      =============================================####

# Select districts:
some_districts <- c("Hoàn Kiếm", "Hai Bà Trưng", "Ba Đình", "Tây Hồ",
                    "Hoàng Mai", "Thanh Xuân", "Nam Tu Liem", "Bac Tu Liem",
                    "Cau Giay", "Dong Da", "Long Bien")

hanoinner <- hanoinner %>%
  filter(NAME_2 %in% some_districts)


# SUBSET SURVEY DATAFRAME
survey_noNA1 <- survey_noNA %>%
  rename(Latitude = homelat, Longitude = homelon)

# CROP TO EXTENT OF HANOI SHAPEFILE 

coordinates(survey_noNA) <- ~ Longitude + Latitude 
survey_noNA <- crop(survey_noNA, extent(hanoinner))
survey_noNA <- as.data.frame(survey_noNA)

####=======================    RECODE      =============================================####

# RECODE CATEGORICAL --> NUMERICAL 

survey_recoded_noNA <- survey_noNA1 %>% 
  mutate(occup = recode(occup, 
                        "student" = 1,
                        "retired" = 0,
                        "private" = 0,
                        "fdi" = 0,
                        "state" = 0),
         gender = recode(gender,
                         "male" = 0,
                         "female" = 1),
         own = recode(own,
                      "owner" = 1,
                      "rent" = 0,
                      "parent_house" = 0,
                      "morgate" = 1),
         age = recode(age,
                      "less_18" = 9,
                      "18_25" = 21.5,
                      "26_35" = 30.5,
                      "36_55" = 45.5,
                      "56_75"= 65.5,
                      "more_75" = 75),
         opinion_ban = recode(opinion_ban,
                              "strongdisagree" = 1,
                              "disagree" = 2,
                              "neutral" = 3,
                              "agree" = 4,
                              "strongagree" = 5),
         purp = recode(purp,
                       "education" = 1,
                       "work" = 1,
                       "visit" = 0,
                       "shopping" = 0,
                       "caring" = 0,
                       "leisure" = 0), 
         aware_ban = recode(aware_ban,
                            "yes" = 1,
                            "no" = 0,
                            "donotcare" = 0),
         fut_veh = recode(fut_veh,
                          "car" = 1,
                          "no" = 0,
                          "moto" = 0,
                          "bike" = 0,
                          "ebike" = 0),
         freqpweek = recode(freqpweek,
                            "1_3" = 2,
                            "4_7" = 5.5,
                            "8_10" = 9,
                            "11_13" = 12,
                            "14_16"= 15,
                            "17_20" = 18.5,
                            "more_20" = 20),
         reason = recode(reason,
                         "convinient" = 1,
                         "cost convinient" = 0,
                         "cost timesaving other" = 0,
                         "timesaving" = 0,
                         "cost convinient timesaving" = 0,
                         "cost" = 0,
                         "convinient timesaving" = 1,
                         "convinient other" = 0,
                         "timesaving other" = 0,
                         "convinient timesaving other" = 0,
                         "cost other" = 0, 
                         "cost timesaving" = 0,
                         "cost convinient timesaving other" = 0,
                         "cost convinient other" = 0),
         own_car = recode(own_car,
                          "0" = 0,
                          "1" = 1,
                          "2" = 1,
                          "3" = 1,
                          "4" = 1),
         own_motob = recode(own_motob,
                            "0" = 0,
                            "1" = 1,
                            "2" = 1,
                            "3" = 1,
                            "4" = 1,
                            "more_5" = 1),
         own_ebike = recode(own_ebike,
                            "0" = 0,
                            "1" = 1,
                            "2" = 1,
                            "3" = 1,
                            "4" = 1,
                            "NA" = 0),
         own_bike = recode(own_bike,
                           "0" = 0,
                           "1" = 1,
                           "2" = 1,
                           "3" = 1,
                           "4" = 1),
         status = recode(status,
                         "permanent" = 1,
                         "long_stay" = 0,
                         "short_stay" = 0),
         reas_not_car = recode(reas_not_car,
                               "cost" = 1,
                               "jam" = 0,
                               "parking" = 0,
                               "unsafe" = 0,
                               "slow" = 0),
         reas_not_motob = recode(reas_not_motob,
                                 "unsafe" = 1,
                                 "jam" = 0,
                                 "parking" = 0,
                                 "cost" = 0,
                                 "slow" = 0),
         reas_not_ebike = recode(reas_not_ebike,
                                 "slow" = 1,
                                 "jam" = 0,
                                 "parking" = 0,
                                 "cost" = 0,
                                 "unsafe" = 0),
         reas_not_bike = recode(reas_not_bike,
                                "slow" = 1,
                                "jam" = 0,
                                "parking" = 0,
                                "cost" = 0,
                                "unsafe" = 0),
         opinion_car = recode(opinion_car,
                              "verybad" = 0,
                              "good" = 1,
                              "verygood" = 1,
                              "neutral" = 0,
                              "bad" = 0),
         opinion_motob = recode(opinion_motob,
                                "verybad" = 0,
                                "good" = 1,
                                "verygood" = 1,
                                "neutral" = 0,
                                "bad" = 0),
         opinion_bike = recode(opinion_bike,
                               "verybad" = 0,
                               "good" = 1,
                               "verygood" = 1,
                               "neutral" = 0,
                               "bad" = 0),
         opinion_ebike = recode(opinion_ebike,
                               "verybad" = 0,
                               "good" = 1,
                               "verygood" = 1,
                               "neutral" = 0,
                               "bad" = 0),
         opinion_bus = recode(opinion_bus,
                              "verybad" = 0,
                              "good" = 1,
                              "verygood" = 1,
                              "neutral" = 0,
                              "bad" = 0),
         opinion_taxi = recode(opinion_taxi,
                               "verybad" = 0,
                               "good" = 1,
                               "verygood" = 1,
                               "neutral" = 0,
                               "bad" = 0),
         vehic = recode(vehic,
                        "taxi" = 0,
                        "moto" = 1,
                        "walk" = 0,
                        "ebike" = 0,
                        "bus" = 0,
                        "bike" = 0,
                        "car" = 0,
                        "tram" = 0))


####=======================    SAVE      =============================================####

# SAVE
save(survey_recoded_noNA, file = "survey_recoded_noNA_cropped_nonBinary.RData")








