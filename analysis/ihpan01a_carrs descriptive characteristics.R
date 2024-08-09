rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/functions/nhst/table1_summary.R")

if(Sys.info()["user"] == "JVARGH7"){
  carrs_data <- read.csv(paste0(path_india_hba1c_box_folder,"/working/Data/Shared Columns/sharedCarrs.csv"))
  
}
if(Sys.info()["user"] == "alaynabaker"){
  carrs_data <- read.csv("/Users/alaynabaker/Documents/Hba1c Prediction/FINAL/Data/Shared Columns/sharedCarrs.csv")
  
}

c_vars = c("glucose", "waistcircumference", "hipcircumference", "weight", "height",
           "sbp", "dbp", "bmi", "age", 
           "alcohol",  "eduyr")

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
