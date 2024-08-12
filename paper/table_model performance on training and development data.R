rm(list=ls());gc();source(".Rprofile")

final_tuned_results_metrics <- readxl::read_excel(paste0(path_india_hba1c_box_folder,"/working/Model Tuning/final_tuned_results.xlsx"),sheet="metrics") %>% 
  rename(average_mse = 'Average MSE',
         average_mae = 'Average MAE',
         average_rmse = 'Average RMSE',
         average_r2 = 'Average R^2',
         average_maxerr = 'Average Max Error') %>% 
  dplyr::select(-average_mse) %>% 
  dplyr::select(Model,average_rmse,average_mae,average_r2,average_maxerr) %>% 
  mutate(across(is.numeric,~round(.,2)))

write_csv(final_tuned_results_metrics,"paper/table_model performance on test.csv")
