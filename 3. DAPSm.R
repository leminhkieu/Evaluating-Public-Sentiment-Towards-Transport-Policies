####=======================   3. DAPSm      =============================================####

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
load("survey_prop.scores.RData") 

####=======================    CALCULATE BALANCE/WEIGHT       =============================================####

#### balance - greedy algorithm  ####
bal <- CalcDAPSWeightBalance(survey_recoded_noNA, weights = seq(0, 1, length.out = 40),
                             cov.cols = 2:32, trt.col = 1, out.col = 33,
                             coords.columns = c(34, 35), caliper = 0.1, coord_dist = TRUE,
                             matching_algorithm = 'greedy')

#### PLOT  ####
PlotWeightBalance(bal$balance, weights = seq(0, 1, length.out = 40), cutoff = 0.15)

####=============================  MAP  (Papadogeorgou et al., 2019)  =================================####
# different weight, treatment and outcome for different case studies
# repeat the above and below for all case studies 

daps1 <- DAPSest(survey_recoded_noNA, out.col = 33, trt.col = 1, caliper = 0.1,
                 weight = 0.9487179, coords.columns = c(34, 35),
                 pairsRet = TRUE, cov.cols = 2:32, cutoff = 0.1,
                 w_tol = 0.001, coord_dist = TRUE, caliper_type = 'DAPS',
                 matching_algorithm = 'greedy')


####### PLOT PAIRS ON A MAP   #########

# get map of Vietnam 
vietnam_dis <- raster::getData("GADM", country = "Vietnam", level = 2)

# Only select Hanoi
hanoi <- vietnam_dis[vietnam_dis$NAME_1 == "Hà Nội", ]

# crop 
hanoi <- crop(hanoi, extent(105.755, 105.91, 20.95, 21.10))

# Convert to data frame
# fortify-spatial: SpatialPolygonsDataFrame to convert into a dataframe
# To figure out the correct variable name for region, inspect as.data.frame(model)
hanoi_df <- hanoi %>% 
  fortify(region = "NAME_2")   #name of variable used to split up regions

some_districts <- c("Hoàn Kiếm", "Hai Bà Trưng", "Ba Đình", "Tây Hồ", 
                    "Hoàng Mai", "Thanh Xuân", "Nam Tu Liem", "Bac Tu Liem",
                    "Cau Giay", "Dong Da", "Long Bien")
# Recode label
hanoi_df %>% mutate(id = case_when(!id %in% some_districts ~ "Others", TRUE ~ id)) -> hanoi_df_relabeled 

# get pairs
fullpairs <- daps1$pairs
fullpairs <- as.data.frame(fullpairs)

# draw line between matched pairs 
line_data <- matrix(NA, nrow = 2 * nrow(fullpairs), ncol = 3)
colnames(line_data) <- c('lon', 'lat', 'group')
for (ii in 1:nrow(fullpairs)) {
  line_data[(2 * ii - 1) : (2 * ii), 1] <- as.numeric(fullpairs[ii, c(3, 7)])
  line_data[(2 * ii - 1) : (2 * ii), 2] <- as.numeric(fullpairs[ii, c(3, 7) + 1])
  line_data[(2 * ii - 1) : (2 * ii), 3] <- rep(ii, 2)
}
line_data <- as.data.frame(line_data)

# plot 
ggplot2::ggplot() +
  theme_bw() +
  geom_polygon(ggplot2::aes(long, lat, group = group), color = 'grey55',
               fill = 'grey85', data = hanoi_df %>% filter(id != "Others")) + 
  #fill = 'grey85', data = hanoi_df_relabeled %>% filter(id != "Others")) + 
  geom_point(data=fullpairs, aes(x=Trt.x, y=Trt.y),color="#4831D4",size=1) + 
  geom_point(data=fullpairs, aes(x=Con.x, y=Con.y),color="#CCF381",size=1) + 
  geom_line(ggplot2::aes(x = lon, y = lat, group = group), data = line_data,
            size = ggplot2::rel(0.4), col = 'black')

# extract treatment and control rows for before matching 
pre_Treatment <- survey_recoded_noNA %>%
  dplyr::filter(vehic == 1)

pre_Control <- survey_recoded_noNA %>%
  dplyr::filter(vehic == 0)

# post matching treatment and control 
survey_recoded_noNA$ID <- seq.int(nrow(survey_recoded_noNA))

treatment <- survey_recoded_noNA %>%
  filter(ID %in% fullpairs$IDtrt)

control <- survey_recoded_noNA %>%
  filter(ID %in% fullpairs$IDcon)

# plot 
ggplot2::ggplot() +
  theme_bw() +
  geom_polygon(ggplot2::aes(long, lat, group = group), color = 'grey55',
               fill = 'grey85', data = hanoi_df %>% filter(id != "Others")) + 
  geom_point(data=pre_Treatment, aes(x=x, y=y),color="#4831D4",size=1) + 
  geom_point(data=pre_Control, aes(x=x, y=y),color="#CCF381",size=1)


####=======================    SAVE      =============================================####
## NOTE ## 
# we saved the below and plotted and formatted all plots in QGIS 
# repeat this for all case studies

write.csv(pre_Control,"PRE_CONTROL_OwnMotob_Freqpweek.csv", row.names = FALSE)
write.csv(pre_Control,"POST_CONTROL_OwnMotob_Freqpweek.csv", row.names = FALSE)
write.csv(pre_Control,"PRE_TREATMENT_OwnMotob_Freqpweek.csv", row.names = FALSE)
write.csv(pre_Control,"POST_TREATMENT_OwnMotob_Freqpweek.csv", row.names = FALSE)

write.csv(line_data,"LINEDATA_TREATMENT_OwnMotob_Freqpweek.csv", row.names = FALSE)


# save whole workspace for step 4
# save for each case study 
save.image(file='Own_Motob_casestudy.RData')
