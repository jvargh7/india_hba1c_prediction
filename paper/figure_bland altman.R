rm(list=ls());gc();source(".Rprofile")


carrs_predictions <- read_csv(paste0(path_india_hba1c_box_folder,"/working/Prediction/CARRS_Predictions.csv")) %>% 
  rename(xgboost_hba1c_predictions = xgboost_tuned_predictions,
         gbm_hba1c_predictions = gradient_boosting_tuned_predictions,
         rf_hba1c_predictions = random_forest_tuned_predictions,
         stacked_hba1c_predictions = stacked_model_predictions) %>% 
  mutate(across(one_of("Actual","xgboost_hba1c_predictions",
                       "gbm_hba1c_predictions","rf_hba1c_predictions",
                       "stacked_hba1c_predictions"),.fns=~exp(.))) %>% 
  mutate(diff_gbm_actual = (Actual - gbm_hba1c_predictions),
         mean_gbm_actual = (gbm_hba1c_predictions + Actual)/2) %>% 
  mutate(sd_diff_gbm_actual = sd(diff_gbm_actual),
         mean_diff_gbm_actual = mean(diff_gbm_actual))

fig_ba = ggplot(data = carrs_predictions) +
  geom_point(aes(x=mean_gbm_actual,y=diff_gbm_actual)) +
  geom_hline(yintercept = 0,color = "red",linetype = "solid") +
  geom_hline(yintercept = mean(carrs_predictions$diff_gbm_actual), color = "salmon", linetype = "dashed") +
  geom_hline(yintercept = mean(carrs_predictions$diff_gbm_actual) + 1.96 * sd(carrs_predictions$diff_gbm_actual), 
             color = "forestgreen", linetype = "dashed") +
  geom_hline(yintercept = mean(carrs_predictions$diff_gbm_actual) - 1.96 * sd(carrs_predictions$diff_gbm_actual), 
             color = "forestgreen", linetype = "dashed") +
  geom_vline(xintercept = 7,linetype = "dashed",color="darkgreen") +
  labs(title = "Bland-Altman Plot: HbA1c",
       x = "Mean of Actual and Predicted Values",
       y = "Difference between Actual and Predicted Values") +
  theme_bw()


fig_ba %>% 
  ggsave(.,filename=paste0(path_india_hba1c_box_folder,"/figures/bland altman for carrs test data.jpg"),width=6,height = 4)
