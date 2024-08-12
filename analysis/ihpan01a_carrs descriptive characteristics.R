rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/functions/nhst/table1_summary.R")

if(Sys.info()["user"] == "JVARGH7"){
  carrs <- read_csv(paste0(path_india_hba1c_box_folder,"/working/Data/ORIGINAL/ihpd01_carrs analytic dataset.csv")) %>% 
    group_by(hhid,linenumber) %>% 
    mutate(any_height_available = sum(!is.na(height))) %>% 
    ungroup()
  carrs_data2 <- read.csv(paste0(path_india_hba1c_box_folder,"/working/Data/Shared Columns/sharedCarrs.csv"))
  # missing_obs = carrs %>% anti_join(carrs_data,by=c("hhid","linenumber","carrs","fup")) %>%
  # inner_join(carrs_data2 %>% dplyr::select(hhid,linenumber,carrs,fup),by=c("hhid","linenumber","carrs","fup"))
  carrs_data <- carrs %>% 
    group_by(hhid,linenumber) %>% 
    dplyr::filter(any_height_available>=1) %>%
    mutate(height = case_when(any_height_available == 0 ~ NA_real_,
                              is.na(height) & fup == 0 ~ zoo::na.locf0(height),
                              is.na(height) & fup == 4 ~ zoo::na.locf(height,na.rm=TRUE),
                              TRUE ~ height)) %>% 
    ungroup() %>%
    dplyr::filter(site %in% c("Chennai","Delhi"),!is.na(hba1c)) %>% 
    dplyr::filter(sex <= 2)  %>% 
    mutate(bmi  = weight/(height/100)^2) %>% 
    mutate(across(one_of("history_htn","history_dm","medication_bp","medication_dm"),.fns=function(x) case_when(is.na(x) ~ 0,
                                                                                                                TRUE ~ x))) %>% 
    mutate(log_hba1c = log(hba1c)) %>% 
    mutate(
      # sbp = rowMeans(.[,c("sbp1","sbp2","sbp3")],na.rm=TRUE),
      # https://stackoverflow.com/questions/53084598/row-wise-min-on-right-hand-when-using-dplyrcase-when
      sbp = case_when((abs(sbp1-sbp2) <= 5) ~ pmin(sbp1,sbp2,na.rm=TRUE),
                      TRUE ~ pmin(sbp1,sbp2,sbp3,na.rm=TRUE)),
      
      # "sb18d" has 108 everywhere
      # dbp = rowMeans(.[,c("dbp1","dbp2","dbp3")],na.rm=TRUE),
      
      dbp = case_when(sbp == sbp1 ~ dbp1,
                      sbp == sbp2 ~ dbp2,
                      sbp == sbp3 ~ dbp3),
      
      dbp = case_when(is.na(dbp) ~ pmin(dbp1,dbp2,dbp3,na.rm=TRUE),
                      TRUE ~ dbp))
  
}
if(Sys.info()["user"] == "alaynabaker"){
  carrs_data <- read.csv("/Users/alaynabaker/Documents/Hba1c Prediction/FINAL/Data/Shared Columns/sharedCarrs.csv")
  
}

c_vars = c("glucose", "waistcircumference", "hipcircumference", "weight", "height",
           "sbp", "dbp", "bmi", "age", "hba1c",
           "eduyr","fpg")

p_vars = c("history_htn", # adjusted - NAs = 0 (categorical)
           "history_dm", # adjusted - NAs = 0 (categorical)
           "medication_bp", # adjusted - NAs = 0 (categorical)
           "medication_dm", "smokecurr",
           "alcohol",  "married", "kitchen",
           "fuel", "water", "toilet", "tv", "fridge", "wmachine", "mobile",
           "computer", "car", "scooter", "bicycle")

g_vars = c("sex", "religion","carrs","fup")

table_df = carrs_data %>% 
  mutate(dataset = case_when(fup == 0 ~ "Training",
                             fup == 4 ~ "Test")) %>% 
  bind_rows(.,
            {.} %>% 
              mutate(dataset="Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "dataset")

write_csv(table_df,"analysis/ihpan01a_carrs descriptive characteristics.csv")


# CARRS Characteristics by Follow-up

table_carrs_fup_df = carrs_data %>% 
  mutate(carrs_fup = case_when(carrs == 1 & fup == 0 ~ "CARRS1_Baseline",
                               carrs == 2 & fup == 0 ~ "CARRS2_Baseline",
                               carrs == 1 ~ "CARRS1_Followup",
                               carrs == 2 ~ "CARRS2_Followup",
                               TRUE ~ NA_character_)) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars,id_vars = "carrs_fup")

write_csv(table_carrs_fup_df,"analysis/ihpan01a_carrs descriptive characteristics by carrs_fup.csv")

