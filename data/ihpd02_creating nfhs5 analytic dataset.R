rm(list=ls());gc();source(".Rprofile")

library(haven)
library(tidyverse)
library(lubridate)


# Based on nfhs5_couples/preprocessing/n5c01_creating couples data.R

source("functions/nfhs5_bp_processing.R")
source("data/ihpd_nfhs5 preprocessing.R")

# Variables ------
iapr_id_vars <- c("cluster","hhid","linenumber")

iahr_variables <- readxl::read_excel("data/India HbA1c Prediction Variable List.xlsx",
                                        sheet = "nfhs5 variables") %>% 
  dplyr::filter(!is.na(iahr7a))

household <- read_dta(paste0(path_dhs_data,"/IA/IAHR7CDT/IAHR7CFL.dta"),
                      col_select = iahr_variables$iahr7a)  %>% 
  rename_with(~ iahr_variables$new_var[which(iahr_variables$iahr7a == .x)], 
              .cols = iahr_variables$iahr7a)

# FEMALE ------------
iair7c_female_variables <- readxl::read_excel("data/India HbA1c Prediction Variable List.xlsx",
                                              sheet = "nfhs5 variables") %>% 
  dplyr::filter(!is.na(iair7a))

female_pr_variables <- readxl::read_excel("data/India HbA1c Prediction Variable List.xlsx",
                                          sheet = "nfhs5 variables") %>% 
  dplyr::filter((is.na(iair7a) & !is.na(iapr7a_women))|new_var %in% c(iapr_id_vars,"age"))

female <- read_dta(paste0(path_dhs_data,"/IA/IAIR7CDT/IAIR7CFL.dta"),
                   col_select = iair7c_female_variables$iair7a)  %>% 
  rename_with(~ iair7c_female_variables$new_var[which(iair7c_female_variables$iair7a == .x)], 
              .cols = iair7c_female_variables$iair7a)  %>% 
  dplyr::filter(!is.na(age))

female_pr <- read_dta(paste0(path_dhs_data,"/IA/IAPR7CDT/IAPR7CFL.dta"),
                      col_select = female_pr_variables$iapr7a_women)  %>% 
  rename_with(~ female_pr_variables$new_var[which(female_pr_variables$iapr7a_women == .x)], 
              .cols = female_pr_variables$iapr7a_women) %>% 
  dplyr::filter(!is.na(age))

female_processed <- female %>% 
  mutate(sex = "Female") %>% 
  left_join(female_pr %>% 
              dplyr::select(-age),
            by=iapr_id_vars) %>% 
  ihpd_nfhs5_preprocessing(.) %>% 
  left_join(household,
            by=c("cluster","hhid"))


# MALE ---------

iamr7c_male_variables <- readxl::read_excel("data/India HbA1c Prediction Variable List.xlsx",
                                            sheet = "nfhs5 variables") %>% 
  dplyr::filter(!is.na(iamr7a)) 

male_pr_variables <- readxl::read_excel("data/India HbA1c Prediction Variable List.xlsx",
                                        sheet = "nfhs5 variables") %>% 
  dplyr::filter((is.na(iamr7a) & !is.na(iapr7a_men))|new_var %in% c(iapr_id_vars,"age")) 


male <- read_dta(paste0(path_dhs_data,"/IA/IAMR7CDT/IAMR7CFL.dta"),
                 col_select = iamr7c_male_variables$iamr7a)  %>% 
  rename_with(~ iamr7c_male_variables$new_var[which(iamr7c_male_variables$iamr7a == .x)], 
              .cols = iamr7c_male_variables$iamr7a)

male_pr <- read_dta(paste0(path_dhs_data,"/IA/IAPR7CDT/IAPR7CFL.dta"),
                    col_select = male_pr_variables$iapr7a_men)  %>% 
  rename_with(~ male_pr_variables$new_var[which(male_pr_variables$iapr7a_men == .x)], 
              .cols = male_pr_variables$iapr7a_men)  %>% 
  dplyr::filter(!is.na(age))

# Preprocessing --------

male_processed <- male %>% 
  mutate(sex = "Male") %>% 
  left_join(male_pr %>% 
              dplyr::select(-age),
            by=iapr_id_vars)  %>% 
  ihpd_nfhs5_preprocessing(.) %>% 
  left_join(household,
            by=c("cluster","hhid"))

nfhs5_analytic <- bind_rows(female_processed %>% mutate(sex = "Female"),
                            male_processed %>% mutate(sex = "Male"))

saveRDS(nfhs5_analytic,paste0(path_india_hba1c_box_folder,"/working/ihpd02_nfhs5 analytic dataset.RDS"))
write_csv(nfhs5_analytic,paste0(path_india_hba1c_box_folder,"/working/ihpd02_nfhs5 analytic dataset.csv"))

nfhs5_analytic %>% 
  group_by(sex) %>% 
  summarize(across(everything(),~sum(!is.na(.)))) %>% 
  write_csv(.,"data/ihpd02_counts of observations in nfhs5 analytic dataset.csv")
