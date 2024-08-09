rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/functions/survey/svysummary.R")

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

g_vars = c("religion")

id_vars = c("hhid","linenumber","psu","state","sex", "district","strata","sampleweight")

if(Sys.info()["user"] == "JVARGH7"){
  # nfhs_data <- read.csv(paste0(path_india_hba1c_box_folder,"/working/Data/Shared Columns/sharedNFHS5.csv"))
  nfhs5 <- read_csv(paste0(path_india_hba1c_box_folder,"/working/ihpd02_nfhs5 analytic dataset.csv"))
  
  nfhs_data <- nfhs5 %>% 
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

    dplyr::select(one_of(id_vars,c_vars,p_vars,g_vars))
  
}
if(Sys.info()["user"] == "alaynabaker"){
  nfhs_data <- read.csv("/Users/alaynabaker/Documents/Hba1c Prediction/FINAL/Data/Shared Columns/sharedNFHS5.csv")
  nfhs5 <- read.csv("/Users/alaynabaker/Documents/Hba1c Prediction/FINAL/Data/ORIGINAL/ihpd02_nfhs5 analytic dataset.csv")
  
}



table_df = nfhs_data %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer") %>% 
   svysummary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars)

write_csv(table_df,"analysis/ihpan01b_nfhs5 descriptive characteristics.csv")
