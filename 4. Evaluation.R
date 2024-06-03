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
library(corrplot)
library(MASS)

#library(devtools)
#devtools::install_github("gpapadog/DAPSm")

####=======================    LOAD DATA       =============================================####

#### LOAD DATA ####
survey <- read_csv("~/Dropbox/My Mac (Rikas-MBP.hub)/Documents/Research_Assistant/data/hn_survey_raw.csv")

# load workspace 
load('Own_Motob_casestudy.RData')
####=================================== ORGANISE/PREP DATA =====================================####
# 16451 obs. 
# look at associations between explanatory variables and an ordinal outcome 
# An OR > 1 corresponds to a risk factor that is associated with greater probability of higher levels of the outcome variable

# make a new survey file to keep column values as categorical 
survey_categ <- survey %>%
  filter(rowid %in% survey_recoded_noNA$rowid)
# save 
save(survey_categ, file = "survey_categ_nonBinary.RData")
# set ID 
survey_categ$ID <- seq.int(nrow(survey_categ))

# separate treatment and control 
treatment <- survey_categ %>%
  filter(ID %in% treatment$ID)

control <- survey_categ %>%
  filter(ID %in% control$ID)

# do the same for post match
# treatment <- survey_categ %>%
#   filter(ID %in% fullpairs$IDtrt)
# 
# control <- survey_categ %>%
#   filter(ID %in% fullpairs$IDcon)

# bind together 
match <- rbind(treatment, control)

# recode 
match <- match %>% 
  mutate(opinion_ban = recode(opinion_ban,
                              "1" = "disagree",
                              "2" = "disagree",
                              "3" = "neutral",
                              "4" = "agree",
                              "5" = "agree"))

#Ordering the dependent variable
match$opinion_ban = factor(match$opinion_ban, levels = c("disagree", "neutral", "agree"), ordered = TRUE)

# check 
table(match$opinion_ban)
summary(match)

# relevel categorical predictor variables so interpretation is easier and more interesting! 

levels(match$purp) = c("work", "education", "visit", "caring", "shopping", "leisure")
levels(match$vehic) = c("moto", "walk", "bus", "bike", "car", "ebike", "tram", "taxi")
levels(match$freqpweek) = c("1_3", "4_7", "8_10", "11_13", "14_16", "17_20", "more_20")
levels(match$reason) = c("convinient", "cost convinient", "timesaving", "cost convinient timesaving", "cost", "convinient timesaving", "cost convinient timesaving other", "cost timesaving other", "cost convinient other", "timesaving other", "convinient other", "cost timesaving", "convinient timesaving other", "cost other")
levels(match$status) = c("permanent", "short_stay", "long_stay")
levels(match$own) = c("owner", "rent", "parent_house", "morgate")
levels(match$reas_not_car) = c("cost", "parking", "jam", "slow", "unsafe")
levels(match$reas_not_bike) = c("unsafe", "slow", "cost", "parking", "jam")
levels(match$reas_not_motob) = c("unsafe", "slow", "cost", "parking", "jam")
levels(match$reas_not_ebike) = c("cost", "slow", "unsafe", "parking", "jam")
levels(match$fut_veh) = c("car", "no", "moto", "ebike", "bike")
levels(match$opinion_bus) = c("car", "no", "moto", "ebike", "bike")
unique(match$opinion_bus)

match_binary <- match

# check correlation 
match_corr <- match_binary %>%
  dplyr::select(-c(opinion_ban, x, y, prop.scores, ID))

match_corr <- match_binary %>%
  dplyr::select(c(opinion_car, opinion_bike, opinion_bus, opinion_motob, opinion_taxi, opinion_ebike))

M<-cor(match_corr)
head(round(M,2))
corrplot(M, tl.cex=0.5)

corrplot(M, method="number")

# check correlation 
# remove own_ebike + opinion_car

####=================================== ORDINAL LOGIT (Ripley, 2022) =====================================####
#Build ordinal logistic regression model
# # fit proportional odds model
ordinal_mod = polr(opinion_ban ~ own_motob + vehic + age + occup + gender + purp + reason + own_car + aware_ban + own_ebike + own_bike + status + own + reas_not_car + reas_not_motob + reas_not_ebike + reas_not_bike + fut_veh + alt_car + alt_ebike + alt_bus + alt_ltrain + alt_taxi + alt_walk + opinion_car + opinion_motob + opinion_ebike + opinion_bike + opinion_taxi + opinion_bus + travtime + freqpweek, 
                   data = match, Hess = TRUE)

# # fit proportional odds model - removed correlated objects
ordinal_mod_bin = polr(opinion_ban ~ own_motob + vehic + age + occup + gender + purp + reason + own_car + aware_ban + own_bike + status + own + reas_not_car + reas_not_motob + reas_not_ebike + reas_not_bike + fut_veh + alt_car + alt_ebike + alt_bus + alt_ltrain + alt_taxi + alt_walk + opinion_car  + opinion_ebike + opinion_bike + opinion_taxi + opinion_bus + travtime + freqpweek, 
                       data = match_binary, Hess = TRUE)

summary(ordinal_mod_bin)

faraway::ilogit(ordinal_mod$zeta)

# Conclusion: The estimated proportion of individuals that disagree with ban is 0.25 
# and the estimated proportion of individuals that agree with ban is 0.66


# OR and 95% CI
CI <-  confint(ordinal_mod)
data.frame(
  OR   = exp(ordinal_mod$coefficients),
  lower = exp(CI[,1]),
  upper = exp(CI[,2])
)


####=================================== INTERPRETATION =====================================####

#### The significance of coefficients and intercepts

summary_table <- coef(summary(ordinal_mod))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table


# all together
CI    <- confint(ordinal_mod_bin)
TSTAT <- summary(ordinal_mod_bin)$coef[1:nrow(CI), "t value"]
primVehic_opBan_bin <- data.frame(
  AOR   = exp(ordinal_mod_bin$coefficients),
  lower = exp(CI[,1]),
  upper = exp(CI[,2]),
  p     = 2*pnorm(abs(TSTAT), lower.tail = F)
)

car::Anova(ordinal_mod, type = 3)

# SAVE
write.table(primVehic_opBan_bin, "ordMod_prim_vehic_newest_OpBan_binary.csv", row.names=TRUE)

# if the p-value  <0.05, they are statistically significant at 95% CI

####====================================  ABSOLUTE STANDARISED MEAN DIFFERENCES   ====================================####

# CONTROL DF
control <- fullpairs %>%
  dplyr::select(c(Con.X, Con.prop.scores, Con.x, Con.y, IDcon, Con.Y)) %>%
  rename(A = Con.X, prop.scores = Con.prop.scores, x = Con.x, y = Con.y, ID = IDcon, Y = Con.Y)

# TREATMENT DF 
treatment <- fullpairs %>%
  dplyr::select(c(Trt.X, Trt.prop.scores, Trt.x, Trt.y, IDtrt, Trt.Y)) %>%
  rename(A = Trt.X, prop.scores = Trt.prop.scores, x = Trt.x, y = Trt.y, ID = IDtrt, Y = Trt.Y)

# BIND 
md <- rbind(treatment, control)


library(tableone)

## VECTOR OF COVARIATES

vars <- c("vehic", "age", "occup", "freqpweek", "OD_dist", "reason", "purp", "own_car", "own_motob", "own_bike", "status","own", "reas_not_car", "reas_not_motob", "reas_not_ebike", "reas_not_bike", "aware_ban", "fut_veh", "alt_car","alt_ebike", "alt_bus", "alt_ltrain", "alt_taxi", "alt_walk", "opinion_car", "opinion_motob", "opinion_taxi", "opinion_ebike", "opinion_bike", "opinion_bus", "travtime")

## Construct a table
tabUnmatched <- CreateTableOne(vars = vars, data = survey_recoded_noNA, test = FALSE)
## Show table with SMD
print(tabUnmatched, smd = TRUE)

# all variables above 0.1
smd <- bal$balance

# get the weight 0.05 values for each columns 

#### REORDER DATAFRAME  ####
survey_recoded_noNA <- survey_recoded_noNA %>%
  dplyr::select(c(fut_veh, vehic, age, occup, gender, purp,freqpweek, reason, own_car, aware_ban, own_ebike, own_bike, status, own, reas_not_car, reas_not_motob, reas_not_ebike, reas_not_bike, own_motob, alt_car,
                  alt_ebike, alt_bus, alt_ltrain, alt_taxi, alt_walk, opinion_car, freqpweek, opinion_motob,
                  opinion_ebike, opinion_bike, opinion_taxi, opinion_bus, travtime, opinion_ban, x, y, prop.scores))
smd[3,,]

balance_smd <- data.frame (var  = c("Primary Vehicle", "Age", "Occupation","Gender", "Purpose", "Reason", "Car Ownership",
                                    "Awareness of ban", "ebike Ownership", "Bike Ownership", "Status", "Home Ownership", "Reason no car",  "Reason no motorbike", "Reason no ebike", "Reason no bike", "Future Vehicle", "Alternative to car", 
"Alternative to ebike", "Alternative to bus", "Alternative to train", "Alternative to taxi","Alternative to walking", "Opinion cars" , "Opinion ban", "Opinion motorbike", "Opinion ebike",  
                                    "Opinion bike", "Opinion taxi", "Opinion bus", "Travel time"),
                           Unmatched = c(0.8306668, -0.08464677, -0.3931681, -0.2464000, 0.57667626, -0.003325046, 1.2992057, 
                                         0.673906486, 0.4601915, 0.4601915, 0.91182545, 0.290718, -1.08055779, 
                                         0.28997727, 0.1793555, 1.1097331, 0.5718234, 0.8738174,
                                         0.13192547, 0.30539793, 0.31913594, 0.46282067, 
                                         0.24604381, 1.4194537, 0.12877142, 1.41825506, 1.3626785,
                                         1.30199332, 1.39841971, 1.00675681, -0.2174353),
                           Matched = c(0.2471608, 0.12119033, -0.1333448, -0.1342934, 0.01928232, 0.071849090, 0.1556198, 
                                       0.008996457, 0.1679127, 0.1679127, 0.05384757, 0.000000, -0.02726564, 
                                       0.04254443, 0.0343306, 0.1074087, 0.1060048, 0.0482058,
                                       0.03095231, 0.04014779, 0.09951786, 0.06651111, 
                                       -0.02177281, -0.0338588, -0.04726864, 0.00448503, -0.0579303,
                                       -0.07513468, 0.02760559, -0.02261559, -0.1284406)
)


# PLOT OF STANDARDISED MEAN DIFFERENCES 

balance_smd_abs <- balance_smd

balance_smd_abs$Unmatched <- abs(balance_smd_abs$Unmatched)
balance_smd_abs$Matched <- abs(balance_smd_abs$Matched)

ggplot2::ggplot(aes(y = .data$var, x = .data$stat, group = .data$Sample), data = balance_smd) +
  ggplot2::theme(panel.background = element_rect(fill = "white"),
                 axis.text.x = element_text(color = "black"),
                 axis.text.y = element_text(color = "black"),
                 panel.border = element_rect(fill = NA, color = "black"),
                 plot.background = element_blank(),
                 legend.background = element_blank(),
                 legend.key = element_blank()
  ) +
  ggplot2::scale_shape_manual(values = shapes) +
  ggplot2::scale_size_manual(values = size) +
  ggplot2::scale_discrete_manual(aesthetics = "stroke", values = stroke) +
  ggplot2::scale_fill_manual(values = fill) +
  ggplot2::scale_color_manual(values = colors) +
  ggplot2::labs(y = NULL, x = wrap(xlab, wrap))

lp <- lp + ggplot2::geom_vline(xintercept = baseline.xintercept,
                               linetype = 1, color = "gray5")

if (is_not_null(threshold.xintercepts)) {
  lp <- lp + ggplot2::geom_vline(xintercept = threshold.xintercepts,
                                 linetype = 2, color = "gray8")
}

shapes = c("circle", "triangle")
colors = c("blue", "red")

cols <- c("Matched"="blue","Unmatched"="red")
line_types <- c("Matched"='triangle',"Unmatched"='blue')

ggplot() +
  geom_point(data = balance_smd_abs, aes(x = Unmatched, y = var, col = "red", shape = "circle", size = 2)) + 
  geom_point(data = balance_smd_abs, aes(x = Matched, y = var, col = "blue", shape = "triangle", size = 2)) + 
  geom_vline(xintercept = 0.1, lty = 2, color = "gray5") + 
  geom_vline(xintercept = 0, lty = 1, color = "gray8") + 
  ggplot2::scale_shape_manual(values = shapes) +
  ggplot2::scale_color_manual(values = colors) +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.border = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank() 
  ) +
  theme(legend.position = "none") +
  xlab("Absolute Standardised Mean Differences") 


#####  COMMON SUPPORT PLOT POST-MATCHING 
labs <- paste("Vehicle Type", c("Motorbike", "Other"))
md %>%
  mutate(A = ifelse(A == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = prop.scores)) +
  geom_histogram(color = "white") +
  facet_wrap(~A) +
  xlab("Probability of having a motorbike as primary vehicle") +
  theme_bw()
