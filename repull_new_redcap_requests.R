
### -------- Setup -------- ###

library(tidyverse)
library(redcapAPI)

today_date <- Sys.Date()
today_time <- Sys.time()

dir_save <- "C:/Users/A00315273/Documents/GitHub/ClientTracking/REDCap_pulls"



### -------- REDCap Pull -------- ###


rcon <- redcapAPI::redcapConnection("https://redcap.cehs.usu.edu/api/", 
                                    "3833AFDE5DB376603FBEA9747787212E")


latest_pull_date <- dir_save %>% 
  list.files(full.name = TRUE) %>%  
  file.info() %>% 
  dplyr::pull(mtime) %>% 
  max() 

df_export_orig <- redcapAPI::exportRecords(rcon,
                                           checkboxLabels = TRUE) %>% 
  dplyr::filter(redcap_event_name == "request_arm_1") %>% 
  dplyr::filter(proj_title != "") 



### -------- General Wrangling -------- ###

df_export <- df_export_orig %>% 
  dplyr::mutate(anum = sprintf("A%08d", as.numeric(anum))) %>% 
  dplyr::mutate_at(vars(client_name_first,
                        client_name_last,
                        client_email,
                        client_phone,
                        client_dept_other,
                        proj_title,
                        proj_type_other,
                        proj_collab_who,
                        grant_fund,
                        grant_type,
                        grant_date,
                        proj_description,
                        proj_help,
                        proj_time,
                        proj_file1, proj_file2, proj_file3, proj_file4),
                   as.character) %>% 
  dplyr::mutate(client_usu_role = client_usu_role %>% 
                  forcats::fct_recode("Student" = "Graduate Student / Research Assistant",
                                      "Faculty" = "Faculty / Research Staff")) %>% 
  dplyr::mutate(ts = case_when(is.na(ts) ~ as.Date(client_info_timestamp),
                               !is.na(ts) ~ as.Date(ts))) %>% 
  dplyr::rename(request_date = ts) %>% 
  dplyr::mutate(record_num = as.numeric(record_num)) %>% 
  dplyr::select(-redcap_event_name, 
                -redcap_survey_identifier,
                -starts_with("initial_"),
                -ends_with("timestamp"),
                -ends_with("complete")) %>% 
  dplyr::arrange(desc(record_num))


save(df_export, file = glue::glue("{dir_save}/all_full_requests.RData"))



max(df_export$record_num)


### -------- Client Database -------- ###

df_clients <- df_export %>% 
  dplyr::select(request_date, anum:client_citi) %>% 
  dplyr::arrange(client_name_last, client_name_first, desc(request_date)) %>% 
  dplyr::group_by(anum) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-request_date) %>% 
  dplyr::arrange(client_name_last, client_name_first) %>% 
  dplyr::mutate(client_dept = stringr::word(client_dept, sep = "\\ -") %>% 
                  factor() %>% 
                  forcats::fct_recode("Other" = "Other (within CEHS)")) %>% 
  dplyr::mutate(student_degree = factor(student_degree) %>% 
                  forcats::fct_recode("Doc" = "PhD (or other doctorate)"))

save(df_clients, file = glue::glue("{dir_save}/all_clients.RData"))





### -------- Grant Database -------- ###

df_grants <- df_export %>% 
  dplyr::filter(proj_type == "Grant Submission") %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(proj_files = sum(!is.na(proj_file1),
                                 !is.na(proj_file2),
                                 !is.na(proj_file3),
                                 !is.na(proj_file4))) %>% 
  dplyr::ungroup() %>% 
  tidyr::unite(col = "proj_hypo",
               proj_hypo, 
               proj_hypo2,
               sep = ", ", 
               remove = TRUE, 
               na.rm = TRUE) %>% 
  dplyr::mutate(across(starts_with("proj_phase"), 
                       ~ forcats::fct_recode(.x, 
                                             NULL = ""))) %>%
  tidyr::unite(col = "proj_phases",
               starts_with("proj_phase"),
               sep = ", ",
               remove = TRUE,
               na.rm = TRUE) %>%
  dplyr::select(record_num, request_date, anum,
                proj_title,
                proj_type,
                proj_collab_who,
                starts_with("grant"),
                proj_description,
                proj_sample,
                proj_hypo,
                proj_past,
                proj_phases,
                proj_help,
                proj_time,
                proj_files) %>% 
  dplyr::arrange(desc(record_num))


save(df_grants, file = glue::glue("{dir_save}/all_grants.RData"))




### -------- Project Database -------- ###

df_projects <- df_export %>% 
  dplyr::filter(proj_type != "Grant Submission") %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(proj_files = sum(!is.na(proj_file1),
                                     !is.na(proj_file2),
                                     !is.na(proj_file3),
                                     !is.na(proj_file4))) %>% 
  dplyr::ungroup() %>% 
  tidyr::unite(col = "proj_hypo",
               proj_hypo, 
               proj_hypo2,
               sep = ", ", 
               remove = TRUE, 
               na.rm = TRUE) %>% 
  dplyr::mutate(across(starts_with("proj_phase"), 
                       ~ forcats::fct_recode(.x, 
                                             NULL = ""))) %>%
  tidyr::unite(col = "proj_phases",
               starts_with("proj_phase"),
               sep = ", ",
               remove = TRUE,
               na.rm = TRUE) %>%
  dplyr::select(record_num, request_date, anum,
                proj_title,
                proj_type,
                proj_irb, proj_irb_number, proj_irb_text,
                proj_collab_who,
                proj_description,
                proj_sample,
                proj_hypo,
                proj_past,
                proj_phases,
                proj_help,
                proj_time,
                proj_files) %>% 
  dplyr::arrange(desc(record_num))
  
save(df_projects, file = glue::glue("{dir_save}/all_projects.RData"))





### -------- Tracking Database: Add new pull -------- ###


df_log <- read.csv(glue::glue("{dir_save}/StatStudio_reqest_task_log.csv")) %>% 
  dplyr::mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
  dplyr::mutate(max_record_num = rowsums(projects, grants, 100))

log_add <- data.frame(date = today_date,
                      clients = nrow(df_clients),
                      projects = nrow(df_projects),
                      grants = nrow(df_grants),
                      max_record_num = max(df_export$record_num))

df_newlog <- df_log %>% 
  rbind(log_add) %>% 
  dplyr::arrange(desc(date))

write.csv(df_newlog,
          file = glue::glue("{dir_save}/StatStudio_reqest_task_log.csv"),
          row.names = FALSE)
 



df_new <- df_projects %>% 
  dplyr::left_join(df_clients, by = "anum") %>% 
  dplyr::mutate(combo = glue::glue("{record_num}_{client_dept}_{client_name_last}_{client_name_first}_{request_date}")) %>% 
  dplyr::mutate(name = glue::glue("{client_name_first} {client_name_last}")) %>% 
  dplyr::mutate(roll = case_when(client_usu_role == "Student" ~ 
                                   glue::glue("{student_degree} {client_usu_role} (w/{student_mentor})"),
                                 TRUE ~
                                   glue::glue("{client_dept} {client_usu_role}"))) %>% 
  dplyr::filter(record_num %in% (df_newlog$max_record_num[1]):(df_newlog$max_record_num[2]))



purrr::walk(.x = df_new$combo,
            ~ rmarkdown::render(
              input = "index.Rmd",
              output_file = glue::glue("request_reports/request_{.x}.pdf"),
              params = list(record_num = df_new %>% 
                              dplyr::filter(combo == {.x}) %>% 
                              dplyr::pull(record_num),
                            proj_type = df_new %>% 
                              dplyr::filter(combo == {.x}) %>% 
                              dplyr::pull(proj_type),
                            proj_title = df_new %>% 
                              dplyr::filter(combo == {.x}) %>% 
                              dplyr::pull(proj_title),
                            date =  df_new %>%
                              dplyr::filter(combo == {.x}) %>%
                              dplyr::pull(request_date),
                            name =  df_new %>%
                              dplyr::filter(combo == {.x}) %>%
                              dplyr::pull(name),
                            anum =  df_new %>%
                              dplyr::filter(combo == {.x}) %>%
                              dplyr::pull(anum),
                            roll =  df_new %>%
                              dplyr::filter(combo == {.x}) %>%
                              dplyr::pull(roll),
                            dept =  df_new %>%
                              dplyr::filter(combo == {.x}) %>%
                              dplyr::pull(client_dept))
            )
)

