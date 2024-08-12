rm(list=ls());gc();source(".Rprofile")

final_tuned_results_vip <- readxl::read_excel(paste0(path_india_hba1c_box_folder,"/working/Model Tuning/final_tuned_results.xlsx"),sheet="var importance",
                                              col_names = paste0(rep(c("xgboost","gbm","rf"),each=2),"_",
                                                                 rep(c("variable","importance"),times=3)),skip = 2) %>%
  mutate(across(contains("importance"),~as.numeric(.))) %>% 
  pivot_longer(cols=everything(),names_sep = "_",names_to=c("model",".value")) %>% 
  mutate(model = factor(model,levels=c("xgboost","gbm","rf"),labels=c("XGBoost","GBM","Random Forests"))) %>% 
  mutate(variable = factor(variable,
                           levels=c("age",
                                    "sex",
                                    "height",
                                    "weight",
                                    "bmi",
                                    "htn",
                                    "dm",
                                    "dm_treatment",
                                    "bp_treatment",
                                    "waistcircumference",
                                    "hipcircumference",
                                    "sbp",
                                    "dbp",
                                    "glucose",
                                    "eduyr",
                                    "alcohol",  
                                    "married", "kitchen",
                                    "fuel", 
                                    "water", 
                                    "toilet", 
                                    "tv", 
                                    "fridge", 
                                    "wmachine", 
                                    "mobile",
                                    "computer", 
                                    "car", 
                                    "scooter", 
                                    "bicycle"
                                    ),
                           labels=c("Age",
                                    "Sex",
                                    "Height",
                                    "Weight",
                                    "BMI",
                                    "Diagnosed Hypertension",
                                    "Diagnosed Diabetes",
                                    "Treated Diabetes",
                                    "Treated Hypertension",
                                    "Waist Circumference",
                                    "Hip Circumference",
                                    "Systolic BP",
                                    "Diastolic BP",
                                    "Glucose",
                                    "Years of schooling",
                                    "Alcohol",
                                    "Married","Kitchen",
                                    "Improved Fuel","Improved Water",
                                    "Improved Toilet",
                                    "Owns Television",
                                    "Owns Refrigerator",
                                    "Owns Washing Machine",
                                    "Owns Mobile",
                                    "Owns Computer",
                                    "Owns Car",
                                    "Owns Scooter",
                                    "Owns Bicycle")))

(figA = final_tuned_results_vip %>% 
  ggplot(data=.,aes(x=importance,y=variable,fill=model)) +
  geom_col(position = position_dodge()) +
  theme_bw() +
  xlab("Variable Importance") +
  ylab("") +
  scale_y_discrete(limits=rev) +  
  scale_fill_manual(name = "",values=c("#A1C3AC","#ACD9EA","#D0ACC9"))+
  theme(legend.position = "bottom")) %>% 
  ggsave(.,filename=paste0(path_india_hba1c_box_folder,"/figures/variable importance plot.jpg"),width=6,height = 6)
