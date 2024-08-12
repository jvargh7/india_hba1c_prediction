
rm(list=ls());gc();source(".Rprofile")

mean_vars = c("glucose", "waistcircumference", "hipcircumference", "weight", "height",
              "sbp", "dbp", "bmi", "age","fpg")

median_vars = c("hba1c","eduyr")
table_carrs_fup_df <- read_csv("analysis/ihpan01a_carrs descriptive characteristics by carrs_fup.csv")

table_df <- table_carrs_fup_df %>% 
  dplyr::mutate(selected_rows = case_when(variable %in% mean_vars & est %in% c("mean","sd") ~ 1,
                                          variable %in% median_vars & est %in% c("median","q25","q75") ~ 1,
                                          !variable %in% c(mean_vars,median_vars) ~ 1,
                                          TRUE ~ 0
  )) %>% 
  dplyr::filter(selected_rows == 1) %>% 
  dplyr::select(carrs_fup,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = case_when(variable %in% mean_vars ~ paste0(round(mean,1)," (",round(sd,1),")"),
                            variable %in% median_vars ~ paste0(round(median,1)," (",round(q25,1),", ",round(q75,1),")"),
                            TRUE ~ paste0("",round(proportion,1),"%")
  )) %>% 
  dplyr::select(variable,group,carrs_fup,output) %>% 
  pivot_wider(names_from=carrs_fup,values_from=output) %>% 
  dplyr::select(variable,group,everything())

write_csv(table_df,"paper/table_descriptive characteristics by carrs followup.csv")  
