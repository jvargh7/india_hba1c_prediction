
rm(list=ls());gc();source(".Rprofile")

library(survey)
library(srvyr)

nfhs5_fulldata <- read_csv(paste0(path_india_hba1c_box_folder,"/working/Prediction/NFHS5_FullData_W_Pred.csv")) %>% 
  rename(xgboost_hba1c_predictions = 'XGBOOST Abs Error_predictions',
         gbm_hba1c_predictions = 'Gradient Boosting_predictions',
         rf_hba1c_predictions = 'Random Forest_predictions',
         stacked_hba1c_predictions = 'stacked_model_predictions')
# nfhs5_justpred <- read_csv(paste0(path_india_hba1c_box_folder,"/working/Prediction/NFHS5_Just_Pred.csv"))

c_vars = c("glucose", "waistcircumference", "hipcircumference", "weight", "height",
           "sbp", "dbp", "bmi", "age", 
           "alcohol",  "eduyr")

p_vars = c("htn", "dm",
           "history_htn", # Renamed, see below
           "history_dm",  # Renamed, see below
           "medication_bp",  # Renamed, see below
           "medication_dm", # Renamed, see below
           "smokecurr",
           "alcohol",  "marital", "kitchen",
           "fuel", "water", "toilet", "tv", "fridge", "wmachine", "mobile",
           "computer", "car", "scooter", "bicycle")

g_vars = c("religion","sex")

id_vars = c("hhid","linenumber","cluster","psu","state","district","strata","sampleweight")

nfhs_data <- read_csv(paste0(path_india_hba1c_box_folder,"/working/ihpd02_nfhs5 analytic dataset.csv")) %>% 
  mutate(weight = case_when(weight >= 9990 ~ NA_real_,
                            TRUE ~ weight/10),
         height = case_when(height >= 9990 ~ NA_real_,
                            TRUE ~ height/10),
         smokecount = case_when(smokecount == 80 ~ NA_real_,
                                TRUE ~ smokecount),
         sex = case_when(sex == "Male" ~ 1,
                         sex == "Female" ~ 2,
                         TRUE ~ NA_real_
         ),
         hb_adjusted = hb_adjusted/10,
         new_bmi = weight/(height/100)^2,
         diagnosed_bp = case_when(is.na(diagnosed_bp) ~ 0,
                                  TRUE ~ diagnosed_bp),
         medication_dm = case_when(is.na(medication_dm) ~ 0,
                                   TRUE ~ medication_dm)) %>% 
  dplyr::filter(age >= 20)  %>% 
  dplyr::select(-marital) %>% 
  dplyr::select(-medication_bp,-medication_dm) %>% 
  rename(marital = married,
         history_htn = diagnosed_bp,
         history_dm = diagnosed_dm,
         medication_bp = treated_bp,
         medication_dm = treated_dm) %>% 
  
  dplyr::select(one_of(id_vars,c_vars,p_vars,g_vars)) %>% 
  left_join(nfhs5_fulldata %>% 
              dplyr::select(#cluster,
                            hhid,linenumber,contains("predictions"),
                            district,age,sex,weight,glucose,waistcircumference,hipcircumference
                            ),
            # by = c("hhid","linenumber","cluster"))
            by = c("hhid","linenumber","district","age","sex","glucose","weight","waistcircumference","hipcircumference"))

nfhs5_district_estimates = nfhs_data %>% 
  group_by(sex) %>% 
  mutate(normalizedweight = sampleweight/sum(sampleweight)) %>% 
  ungroup() %>% 
  dplyr::filter(history_dm == 1) %>% 
  mutate(dm_control_hba1c = case_when(is.na(gbm_hba1c_predictions) ~ NA_real_,
                                      gbm_hba1c_predictions < log(8.0) ~ 1,
                                      gbm_hba1c_predictions >= log(8.0) ~ 0,
                                      TRUE ~ NA_real_),
         dm_control_hba1c_lt7 = case_when(is.na(gbm_hba1c_predictions) ~ NA_real_,
                                          gbm_hba1c_predictions < log(7.0) ~ 1,
                                          gbm_hba1c_predictions >= log(7.0) ~ 0,
                                          TRUE ~ NA_real_),
         dm_control_rbg = case_when(is.na(glucose) ~ NA_real_,
                                    glucose < 180 ~ 1,
                                    glucose >= 180 ~ 0,
                                    TRUE ~ NA_real_)) %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer") %>% 
  group_by(district) %>% 
  summarize(dm_control_hba1c = survey_mean(dm_control_hba1c,prop_method = "logit",na.rm=TRUE),
            dm_control_hba1c_lt7 = survey_mean(dm_control_hba1c_lt7,prop_method = "logit",na.rm=TRUE),
            dm_control_rbg = survey_mean(dm_control_rbg,prop_method = "logit",na.rm=TRUE))


nfhs5_district_counts = nfhs_data %>% 
  dplyr::filter(history_dm == 1) %>% 
  group_by(district) %>% 
  tally()

nfhs5_district_estimates %>% 
  left_join(nfhs5_district_counts,
            by="district") %>% 

write_csv(.,"analysis/ihpan02_nfhs5 district estimates.csv")

