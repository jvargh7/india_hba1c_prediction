rm(list=ls());gc();source(".Rprofile")
mean_vars = c("glucose", "waistcircumference", "hipcircumference", "weight", "height",
              "sbp", "dbp", "bmi", "age","fpg")

median_vars = c("hba1c","eduyr")


carrs_summary <- read_csv("analysis/ihpan01a_carrs descriptive characteristics.csv") %>% 
  dplyr::filter(est == "missing") %>% 
  dplyr::select(dataset,variable,value) %>%
  distinct(dataset,variable,value,.keep_all = TRUE) %>% 
  pivot_wider(names_from="dataset",values_from = "value")

nfhs5_summary <- read_csv("analysis/ihpan01b_nfhs5 descriptive characteristics.csv") %>% 
  mutate(dataset = "NFHS") %>%
  rename(value = estimate) %>%
  dplyr::filter(est %in% c("mean","proportion")) %>% 
  mutate(est = case_when(variable %in% c(median_vars) & est == "mean" ~ "median",
                         TRUE ~ est)) %>% 
  # Not perfect but it works
  mutate(NFHS5 = max(n) - n) %>% 
  dplyr::select(variable,NFHS5)


(table_missing = carrs_summary %>% 
  left_join(nfhs5_summary,
            by=c("variable"))) %>% 
  dplyr::select(variable,Total,Training,Test,NFHS5) %>% 
  write_csv(.,"paper/table_missing data in key covariates.csv")
