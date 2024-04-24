# This is the main preprocessing file used for Diabetes and Hypertension cascades
# Compared to ncp_preprocessing2:
# 1. Uses lowest SBP measure
# 2. Uses 220 mg/dL for rpg_cutoff


ihpd_nfhs5_preprocessing <- function(df, sex = "Female"){
  
  df %>% 
    mutate(sampleweight = sampleweight/(10^6),
           day_interview = case_when(is.na(day_interview) | day_interview == 98 ~ 15,
                                     month_interview == 2 & year_interview %in% c(2008,2012,2016,2020) & day_interview > 29 ~ 29,
                                     month_interview == 2 & day_interview > 28 ~ 28,
                                     month_interview %in% c(4,6,9,11) & day_interview > 30 ~ 30,
                                     TRUE ~ as.numeric(day_interview))) %>% 
    mutate(interview = as_date(paste0(year_interview,"-",month_interview,"-",day_interview)),
           phase = case_when(interview <= "2020-03-23" ~ 1,
                             interview > "2020-03-23" ~ 2,
                             TRUE ~ NA_real_)
    )  %>% 
    nfhs5_bp_processing(.) %>% 
    
    # Different in qc/qc15to49_preprocessing and nfhs5_couples/preprocessing/n5couples_preprocessing ---------
  mutate(screened_dm = case_when(screened_dm == 1 ~ 1,
                                 TRUE ~ 0),
         diagnosed_dm = case_when(toldhigh_dm == 1 ~ 1,
                                  TRUE ~ 0),
         treated_dm = case_when(medication_dm == 1 ~ 1,
                                TRUE ~ 0),
         
         
         screened_bp = case_when(screened_bp == 1 ~ 1,
                                 TRUE ~ 0),
         diagnosed_bp = case_when(toldhigh_bp == 1 ~ 1,
                                  TRUE ~ 0),
         treated_bp = case_when(medication_bp == 1 ~ 1,
                                TRUE ~ 0)) %>% 
    
    mutate(bmi_underweight = case_when(bmi > bmi_max ~ NA_real_,
                                       bmi < bmi_cutoff[1] ~ 1,
                                       bmi >= bmi_cutoff[1] ~ 0,
                                       TRUE ~ NA_real_),
           
           
           bmi_overweight = case_when(bmi > bmi_max ~ NA_real_,
                                      bmi >= bmi_cutoff[2] & bmi < bmi_cutoff[3] ~ 1,
                                      bmi < bmi_cutoff[2] | bmi >= bmi_cutoff[3] ~ 0,
                                      TRUE ~ NA_real_),
           
           
           bmi_obese = case_when(bmi > bmi_max ~ NA_real_,
                                 bmi >= bmi_cutoff[3] ~ 1,
                                 bmi < bmi_cutoff[3] ~ 0,
                                 TRUE ~ NA_real_)) %>% 
    
    mutate(fasting = case_when(lastate > 94 | lastdrank > 94 ~ NA_real_,
                               lastate > fasting_time & lastdrank > fasting_time ~ 1,
                               lastate <=fasting_time | lastdrank <= fasting_time ~ 0,
                               TRUE ~ NA_real_),
           
           dm = case_when(diagnosed_dm == 1 ~ 1,
                          is.na(glucose) | glucose > 498 ~ NA_real_,
                          fasting == 1 & glucose >= fpg_cutoff ~ 1,
                          fasting == 0 & glucose >= rpg_cutoff ~ 1,
                          is.na(fasting) & glucose >= rpg_cutoff ~ 1,
                          fasting == 1 & glucose < fpg_cutoff ~ 0,
                          fasting == 0 & glucose < rpg_cutoff ~ 0,
                          is.na(fasting) & glucose < rpg_cutoff ~ 0,
                          TRUE  ~ NA_real_)
           
           
    ) %>% 
    
    # Option 2: Should this be average of last 2 measurements?
    # Option 3: ICMR suggests take 2 measurements 1 min apart, if difference in SBP > 5mmHg, take 3rd. Take lowest among closest.
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
                      TRUE ~ dbp),
      
      htn = case_when(diagnosed_bp == 1 ~ 1,
                      is.na(sbp) | is.na(dbp) ~ NA_real_,
                      sbp >= sbp_cutoff ~ 1,
                      dbp >= dbp_cutoff ~ 1,
                      sbp < sbp_cutoff ~ 0,
                      dbp < dbp_cutoff ~ 0,
                      TRUE ~ NA_real_)
    ) %>% 
    
    
     # BMI
    mutate_at(vars(bmi),function(x) case_when(x > 6000 ~ NA_real_,
                                              TRUE ~ as.numeric(x))) %>%
    # Circumferences
    mutate_at(vars(waistcircumference,hipcircumference),function(x) case_when(x > 240 ~ NA_real_,
                                                                              TRUE ~ as.numeric(x))) %>% 
    
    
    # Glucose
    mutate_at(vars(glucose), function(x) case_when(is.na(x) | x > 498 ~ NA_real_,
                                                   TRUE ~ as.numeric(x))) %>% 
    # Caste
    mutate(na_caste = case_when(is.na(caste) | caste == 8 ~ 1,
                                TRUE ~ 0)) %>% 
    mutate_at(vars(caste),function(x) case_when(x == 1 ~ "Schedule Caste",
                                                x == 2 ~ "Schedule Tribe",
                                                x == 3 ~ "OBC",
                                                x == 4 ~ "General",
                                                x == 8 ~ "General",
                                                TRUE ~ "General")) %>% 
    # Education
    mutate(na_education = case_when(is.na(education) | education == 9 ~ 1,
                                    TRUE ~ 0)) %>% 
    mutate_at(vars(education),function(x) case_when(x == 0 ~ "No education",
                                                    x == 1 ~ "Primary",
                                                    x == 2 ~ "Secondary",
                                                    x == 3 ~ "Higher",
                                                    x == 9 ~ "No education",
                                                    TRUE ~ "No education")) %>% 
    # Religion
    mutate_at(vars(religion),function(x) case_when(x == 1 ~ "Hindu",
                                                   x == 2 ~ "Muslim",
                                                   TRUE ~ "Other")) %>% 
    # insurance, alcohol
    mutate_at(vars(
      alcohol,insurance), function(x) case_when(x == 0 ~ 0,
                                                x == 1 ~ 1,
                                                TRUE ~ NA_real_)) %>% 
    # Smoking
    mutate_at(vars(smokecurr), function(x) case_when(x == 1 ~ 1,
                                                     x == 0 ~ 0,
                                                     TRUE ~ NA_real_)) %>% 
    
    mutate(smokecount = case_when(smokecount > 80 ~ NA_real_,
                                  TRUE ~ as.numeric(smokecount))) %>% 
    
    mutate(
      eduyr = case_when(education == "No education" ~ 0,
                        TRUE ~ as.numeric(eduyr))
    ) %>% 
   
    mutate(bmi_category = case_when(bmi > bmi_max ~ NA_real_,
                                    bmi >= bmi_cutoff[3] ~ 4,
                                    bmi >= bmi_cutoff[2] ~ 3,
                                    bmi >= bmi_cutoff[1] ~ 2,
                                    bmi < bmi_cutoff[1] ~ 1,
                                    TRUE ~ NA_real_),
           
           highwc = case_when(sex == "Female" & waistcircumference >= female_wc_cutoff ~ 1,
                              sex == "Female" & waistcircumference < female_wc_cutoff ~ 0,
                              sex == "Male" & waistcircumference >= male_wc_cutoff ~ 1,
                              sex == "Male" & waistcircumference < male_wc_cutoff ~ 0,
                              TRUE ~ NA_real_
           ),
           waist_hip = case_when(!is.na(hipcircumference) ~ waistcircumference/hipcircumference,
                                 TRUE ~ NA_real_)
    ) %>% 
    
    mutate(bmi_category = factor(bmi_category,levels=c(1:4),labels=c("Underweight","Normal","Overweight","Obese")),
           highwhr = case_when(sex == "Female" & waist_hip >= female_whr_cutoff ~ 1,
                               sex == "Female" & waist_hip < female_whr_cutoff ~ 0,
                               sex == "Male" & waist_hip >= male_whr_cutoff ~ 1,
                               sex == "Male" & waist_hip < male_whr_cutoff ~ 0,
                               TRUE ~ NA_real_),
           age_category = case_when(age %in% c(18:39) ~ 1,
                                    age %in% c(40:64) ~ 2,
                                    age >= 65 ~ 3,
                                    TRUE ~ NA_real_)) %>% 
    mutate(age_category = factor(age_category,levels=c(1:3),labels=c("18-39","40-64","65 plus")),
           
           # State wealth quintile - urban/rural
           swealthq_ur = case_when(!is.na(suwealthq) ~ suwealthq,
                                   TRUE ~ srwealthq),
           # State wealth factor score - urban/rural
           swealths_ur = case_when(!is.na(suwealths) ~ suwealths,
                                   TRUE ~ srwealths)
    ) %>% 
    mutate(bmi = bmi/100)  %>% 
    mutate(age_category10 = cut(age,breaks=c(18,30,40,50,60,70,80,100),include.lowest=TRUE,right=FALSE),
           age_category5 = cut(age,breaks=seq(15,100,by=5),include.lowest=TRUE,right=FALSE)) %>% 
    
    return(.)
}

