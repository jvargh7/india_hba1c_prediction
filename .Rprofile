library(haven)
library(tidyverse)

if(Sys.info()["user"] == "JVARGH7"){
  path_india_hba1c_emory_folder <- "C:/Cloud/OneDrive - Emory University/Papers/CARRS HbA1c Prediction Internal"
  path_india_hba1c_box_folder <- "C:/Users/jvargh7/Box/Papers/CARRS HbA1c Prediction"
  path_dhs_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program"
}
if(Sys.info()["user"] == "alaynabaker"){
  # path_india_hba1c_internal_folder <- "/Users/alaynabaker/Documents/Hba1c Prediction/FINAL"
  # path_india_hba1c_box_folder <- "C:/Users/jvargh7/Box/Papers/CARRS HbA1c Prediction"
  # path_dhs_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program"
}


fasting_time <- 7.9

bmi_max = 6000

bmi_cutoff <- c(1850, 2500, 3000)
bmiasian_cutoff <- c(1850,2300,2750)

# Non-Asians
female_wc_cutoff = 80 
female_whr_cutoff = 0.80
male_wc_cutoff = 94 
male_whr_cutoff = 0.95


# Asians
# female_wc_cutoff = 80 
# female_whr_cutoff = 0.85
# male_wc_cutoff = 90 
# male_whr_cutoff = 0.9

fpg_cutoff <- 126
rpg_cutoff <- 220
# Alternative cutoff for RPG --> used in ncp_preprocessing2.R
rpg_cutoff2 <- 200
# Alternative cutoff for RPG --> used in ncp_preprocessing3.R
rpg_cutoff3 <- 160

source("C:/code/external/nfhs_cascade/functions/bp_processing.R")

sbp_cutoff <- 140
dbp_cutoff <- 90
sbppre_cutoff <- 130
dbppre_cutoff <- 85

# Alternative cutoff for SBP and DBP --> used in ncp_preprocessing3.R
sbp_cutoff2 <- 130
dbp_cutoff2 <- 80
sbppre_cutoff2 <- 120
dbppre_cutoff2 <- 80

fpg_target <- 126
rpg_target <- 180 #Indian DM guidelines
# Indian HTN guidelines (Shah 2020: 130/80 for <= 60y, 140/90 otherwise)
# ICMR 2016 guidelines
# 4.4.1 The current target for control of BP for patients under 80 years of age should
# be systolic blood pressure less than 140 mm and diastolic blood pressure less than 90 mm.
# 4.4.2 The current target for control of BP for patients 80 years or older should be
# less than 150 mm systolic and less than 90 mm diastolic.

sbp_target <- c(140,150) 
agebp_cutoff <- 80
dbp_target <- c(90,90)

fpgpre_cutoff <- 100
rpgpre_cutoff <- 140


gc()


