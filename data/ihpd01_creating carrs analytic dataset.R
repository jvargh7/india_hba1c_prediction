rm(list=ls());gc();source(".Rprofile")


# Please specify exposures, outcomes, and co-variates-----
# The outcome of interest is HbA1c. The covariates (“machine learning predictors”) of interest are Height, 
# BMI, SBP, DBP, waist circumference, hip circumference, socio-demographic characteristics (age, sex), 
# comorbidities and medication use (diabetes, hypertension, cancer, heart disease), 
# health behaviors (status [ever/current/never] and frequency of tobacco and alcohol use), 
# physical activity (IPAQ) and family history (diabetes, hypertension). 

carrs_variables <- readxl::read_excel("data/India HbA1c Prediction Variable List.xlsx",sheet="carrs variables")

# LAB -----------
lab <- read_sas(paste0(path_india_hba1c_emory_folder,"/working/Varghese/carrs_lab 2024_03_01.sas7bdat"),
                col_select = na.omit(carrs_variables$lab)) %>% 
  rename_with(~ carrs_variables[!is.na(carrs_variables$lab),]$new_var[which(na.omit(carrs_variables$lab) == .x)], 
              .cols = na.omit(carrs_variables$lab))

lab %>% 
  group_by(carrs) %>% 
  summarize(s_fpg_120 = sum(!is.na(fpg_120)),
            s_gtt_120 = sum(!is.na(gtt_120)))

table(lab$fup,lab$carrs)

lab %>% dplyr::filter(carrs == 1) %>% ggplot(data=.,aes(x=lab_date)) + geom_histogram()
lab %>% dplyr::filter(carrs == 2) %>% ggplot(data=.,aes(x=lab_date)) + geom_histogram()

# LONGITUDINAL -----------

longitudinal <- read_sas(paste0(path_india_hba1c_emory_folder,"/working/Varghese/carrs_longitudinal 2024_02_21.sas7bdat"),
                         col_select = na.omit(carrs_variables$longitudinal)) %>% 
  rename_with(~ carrs_variables[!is.na(carrs_variables$longitudinal),]$new_var[which(na.omit(carrs_variables$longitudinal) == .x)], 
              .cols = na.omit(carrs_variables$longitudinal)) 


BL <- read_sas(paste0(path_india_hba1c_emory_folder,"/working/Varghese/carrs_BL 2024_02_21.sas7bdat"),
                     col_select = na.omit(carrs_variables$BL)) %>% 
  rename_with(~ carrs_variables[!is.na(carrs_variables$BL),]$new_var[which(na.omit(carrs_variables$BL) == .x)], 
              .cols = na.omit(carrs_variables$BL)) %>% 
  mutate(fup = 0)



major_followup <- read_sas(paste0(path_india_hba1c_emory_folder,"/working/Varghese/carrs_major fup 2024_02_21.sas7bdat"),
                           col_select = na.omit(carrs_variables$major_fup)) %>% 
  rename_with(~ carrs_variables[!is.na(carrs_variables$major_fup),]$new_var[which(na.omit(carrs_variables$major_fup) == .x)], 
              .cols = na.omit(carrs_variables$major_fup)) %>% 
  mutate(fup = case_when(fup == "FU1" ~ 1,
                         fup == "FU4" ~ 4,
                         TRUE ~ NA_real_))


with(major_followup,table(carrs,fup,useNA="always"))


# Analytic dataset ----------


longitudinal_variables = c("onset_hd","onset_ca","onset_ckd",
                           "medication_bp","medication_dm","medication_hd",
                           "medication_ckd","alcohol","smokecurr",
                           "freq_alcohol_weekly","freq_smoke_weekly")

BL_sociodemographics = c("age","religion","caste","sex")
BL_fixed = BL %>% 
  dplyr::select(linenumber,carrs,doi,one_of(BL_sociodemographics)) %>% 
  rename(baseline_doi = doi)


analytic_dataset <- bind_rows(BL %>% 
                                dplyr::select(-one_of(c(longitudinal_variables,BL_sociodemographics))) %>%
                                mutate(type = "baseline"),
                              major_followup  %>% 
                                dplyr::select(-one_of(c(longitudinal_variables,BL_sociodemographics))) %>%
                                mutate(type = "major followup")) %>% 
                               
  left_join(BL_fixed,
            by = c("linenumber","carrs")) %>% 
  mutate(age = case_when(!is.na(doi) ~ floor(age + as.numeric(difftime(doi,baseline_doi,units="days")/365.25)),
                         TRUE ~ NA_real_)) %>% 
  left_join(longitudinal %>% dplyr::select(-doi),
            by=c("linenumber","carrs","fup")) %>% 
  dplyr::filter(!is.na(doi)) %>% 
  mutate(married = case_when(marital %in% c(2) ~ 1,
                             marital %in% c(1,3,4,5) ~ 0,
                             TRUE ~ NA_real_),
         toilet_harmonized = case_when(toilet %in% c(1,2,4) ~ 0,
                                       toilet %in% c(3) ~ 1,
                                       TRUE ~ NA_real_),
         fuel_harmonized = case_when(fuel %in% c(1,3,4) ~ 0,
                                     fuel %in% c(2) ~ 1,
                                     TRUE ~ NA_real_),
         water_harmonized = case_when(water %in% c(4,5) ~ 1,
                                      water %in% c(2,3,1,6) ~ 0,
                                      TRUE ~ NA_real_)) %>% 
  dplyr::select(-fuel,-water,-toilet) %>% 
  rename(
    fuel = fuel_harmonized,
    water = water_harmonized,
    toilet = toilet_harmonized) %>% 
  mutate(religion = case_when(religion == 1 ~ "Hindu",
                              religion == 2 ~ "Islam",
                              TRUE ~ "Other"),
         scst = case_when(caste == 1 ~ 1,
                          TRUE ~ 0))

saveRDS(analytic_dataset,paste0(path_india_hba1c_box_folder,"/working/ihpd01_carrs analytic dataset.RDS"))
write_csv(analytic_dataset,paste0(path_india_hba1c_box_folder,"/working/ihpd01_carrs analytic dataset.csv"))

analytic_dataset <- readRDS(paste0(path_india_hba1c_box_folder,"/working/ihpd01_carrs analytic dataset.RDS"))

summary(analytic_dataset$age)

analytic_dataset %>% 
  group_by(carrs,fup) %>% 
  summarize(across(everything(),~sum(!is.na(.)))) %>% 
  write_csv(.,"data/ihpd01_counts of observations in carrs analytic dataset.csv")

