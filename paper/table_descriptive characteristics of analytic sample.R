rm(list=ls());gc();source(".Rprofile")
mean_vars = c("glucose", "waistcircumference", "hipcircumference", "weight", "height",
              "sbp", "dbp", "bmi", "age","fpg")

median_vars = c("hba1c","eduyr")


carrs_summary <- read_csv("analysis/ihpan01a_carrs descriptive characteristics.csv")
nfhs5_summary <- read_csv("analysis/ihpan01b_nfhs5 descriptive characteristics.csv") %>% 
  mutate(dataset = "NFHS") %>%
  rename(value = estimate) %>% 
  mutate(est = case_when(variable %in% c(median_vars) & est == "mean" ~ "median",
                         TRUE ~ est)) %>% 
  dplyr::select(dataset,variable,est,group,value)


table_df <- bind_rows(carrs_summary,
                      nfhs5_summary) %>% 
  dplyr::mutate(selected_rows = case_when(variable %in% mean_vars & est %in% c("mean","sd") ~ 1,
                                          variable %in% median_vars & est %in% c("median","q25","q75") ~ 1,
                                          !variable %in% c(mean_vars,median_vars) ~ 1,
                                          TRUE ~ 0
  )) %>% 
  dplyr::filter(selected_rows == 1) %>% 
  dplyr::select(dataset,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = case_when(variable %in% mean_vars ~ paste0(round(mean,1)," (",round(sd,1),")"),
                            variable %in% median_vars & dataset == "NFHS" ~ paste0(round(median,1)),
                            variable %in% median_vars ~ paste0(round(median,1)," (",round(q25,1),", ",round(q75,1),")"),
                            TRUE ~ paste0("",round(proportion,1),"%")
  )) %>% 
  dplyr::select(variable,group,dataset,output) %>% 
  pivot_wider(names_from=dataset,values_from=output) %>% 
  dplyr::select(variable,group,Total,Training,Test,NFHS)

write_csv(table_df,"paper/table_descriptive characteristics by analytic sample.csv")  

