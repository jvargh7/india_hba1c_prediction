source("C:/code/external/functions/preprocessing/dictionary_file.R")



file_list <- list.files(paste0(path_india_hba1c_folder,"/working/Varghese"))



all_dicts <- map_dfr(file_list,
                     function(f_l){
                       
                       df = read_sas(paste0(path_india_hba1c_folder,"/working/Varghese/",f_l))
                       dictionary_file(df,type = "sas7bdat",name=f_l,return_dictionary = TRUE) %>% 
                         mutate(file_name = f_l)
                       
                     }
                     
                     )
