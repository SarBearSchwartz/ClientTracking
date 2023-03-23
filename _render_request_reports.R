library(tidyverse)
library(readxl)


load("REDCap_pulls/all_projects.RData")
load("REDCap_pulls/all_clients.RData")

log <- read.csv("REDCap_pulls/StatStudio_reqest_task_log.csv")

new_num <- sum(log[1,-1] - log[2, -1])



df <- df_projects %>% 
  dplyr::left_join(df_clients, by = "anum") %>% 
  dplyr::mutate(combo = glue::glue("{record_num}_{client_dept}_{client_name_last}_{client_name_first}_{request_date}")) %>% 
  dplyr::mutate(name = glue::glue("{client_name_first} {client_name_last}")) %>% 
  dplyr::mutate(roll = case_when(client_usu_role == "Student" ~ 
                                   glue::glue("{student_degree} {client_usu_role} (w/{student_mentor})"),
                                 TRUE ~
                                   glue::glue("{client_dept} {client_usu_role}")))  
  

# df <- df %>% 
#   dplyr::filter(record_num %in% c(584, 521))

purrr::walk(.x = df$combo,
            ~ rmarkdown::render(
              input = "index.Rmd",
              output_file = glue::glue("request_reports/request_{.x}.pdf"),
              params = list(record_num = df %>% 
                              dplyr::filter(combo == {.x}) %>% 
                              dplyr::pull(record_num),
                            proj_type = df %>% 
                              dplyr::filter(combo == {.x}) %>% 
                              dplyr::pull(proj_type),
                            proj_title = df %>% 
                              dplyr::filter(combo == {.x}) %>% 
                              dplyr::pull(proj_title),
                            date =  df %>%
                              dplyr::filter(combo == {.x}) %>%
                              dplyr::pull(request_date),
                            name =  df %>%
                              dplyr::filter(combo == {.x}) %>%
                              dplyr::pull(name),
                            anum =  df %>%
                              dplyr::filter(combo == {.x}) %>%
                              dplyr::pull(anum),
                            roll =  df %>%
                              dplyr::filter(combo == {.x}) %>%
                              dplyr::pull(roll),
                            dept =  df %>%
                              dplyr::filter(combo == {.x}) %>%
                              dplyr::pull(client_dept))
            )
)
