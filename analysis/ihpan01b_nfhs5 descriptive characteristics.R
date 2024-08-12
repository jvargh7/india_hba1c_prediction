rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/functions/survey/svysummary.R")
source("C:/code/external/functions/survey/svysd.R")

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

id_vars = c("hhid","linenumber","psu","state","district","strata","sampleweight")

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



nfhs_svy = nfhs_data %>% 
  group_by(sex) %>% 
  mutate(normalizedweight = sampleweight/sum(sampleweight)) %>% 
  ungroup() %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")


table_df = nfhs_svy %>% 
   svysummary(.,c_vars = c_vars,p_vars = p_vars,g_vars = g_vars)


table_df_sd = nfhs_svy %>% 
  svysd(.,c_vars = c_vars)

table_df_n = nfhs_data %>% 
  summarize(across(c(c_vars,p_vars,g_vars),~sum(!is.na(.)))) %>% 
  pivot_longer(cols=everything(),names_to="variable",values_to="n")

bind_rows(table_df %>% mutate(est = case_when(type == "Continuous" ~ "mean",
                                              TRUE ~ "proportion")),
          table_df_sd %>% mutate(est = "sd")) %>% 
  left_join(table_df_n,
            by=c("variable")) %>% 


write_csv(.,"analysis/ihpan01b_nfhs5 descriptive characteristics.csv")
