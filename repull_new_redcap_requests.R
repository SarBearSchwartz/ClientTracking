library(tidyverse)
library(redcapAPI)

rcon <- redcapAPI::redcapConnection("https://redcap.cehs.usu.edu/api/", 
                                    "3833AFDE5DB376603FBEA9747787212E")

dir_save <- "C:/Users/A00315273/Documents/GitHub/ClientTracking/REDCap_pulls"

today_date <- Sys.Date()
today_time <- Sys.time()

latest_pull_date <- dir_save %>% 
  list.files(full.name = TRUE) %>%  
  file.info() %>% 
  dplyr::pull(mtime) %>% 
  max() 


if (today_time > latest_pull_date){
  
  df_export <- redcapAPI::exportRecords(rcon)
  
  last_request_dt <- df_export$client_info_timestamp %>% 
    max(na.rm = TRUE)

  
  if (last_request_dt > latest_pull_date){
    save(df_export, 
         file = glue::glue("{dir_save}/requests_{today}.RData"))
  }
}




log <- read.csv("{dir_save}/StatStudio_reqest_task_log.csv") %>% 

  



write.csv(log, 
          file = glue::glue("{dir_save}/StatStudio_reqest_task_log.csv"))


