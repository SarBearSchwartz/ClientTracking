
### -------- Setup -------- ###

library(tidyverse)
library(redcapAPI)

today_date <- Sys.Date()
today_time <- Sys.time()

dir_save <- "C:/Users/A00315273/Documents/GitHub/ClientTracking/REDCap_pulls"


### -------- Load last client list -------- ###

load("REDCap_pulls/all_clients.RData")


opt_client <- df_clients %>% 
  dplyr::mutate(across(where(is.character), 
                stringr::str_trim)) %>% 
  dplyr::arrange(desc(anum))  %>% 
  dplyr::mutate(anum9 = stringr::str_sub(anum, start = 2)) %>% 
  dplyr::mutate(anum9 = as.numeric(anum9)+900000000) %>% 
  dplyr::mutate(text = glue::glue("{anum9}, ({anum}) {client_name_last}, {client_name_first}")) %>% 
  dplyr::select(client_usu_role, client_dept, text) %>% 
  dplyr::group_by(client_usu_role, client_dept) %>% 
  dplyr::summarise(client_list = paste(text, collapse = "| ")) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(client_usu_role = client_usu_role %>% 
                  forcats::fct_recode("fac" = "Faculty",
                                      "std" = "Student")) %>% 
  dplyr::mutate(field_name  = glue::glue("client_{client_usu_role}_{tolower(client_dept)}")) %>% 
  dplyr::select(field_name , client_list)



### -------- Load last project list -------- ###

load("REDCap_pulls/all_projects.RData")


opt_proj <- df_projects %>% 
  dplyr::left_join(df_clients, by = "anum") %>% 
  dplyr::mutate(across(where(is.character), 
                       stringr::str_trim)) %>% 
  dplyr::mutate(proj_type = proj_type %>% 
                  forcats::fct_recode("diss" = "Dissertation",
                                      "thesis" = "Thesis",
                                      "pub" = "Publication/Article",
                                      "grant" = "Grant Submission",
                                      "other" = "Abstract",
                                      "other" = "Poster or Presentation",
                                      "other" = "Other")) %>% 
  dplyr::mutate(proj_title = case_when(
    proj_type == "diss" ~ glue::glue("DISSERTATION: {client_name_first} {client_name_last}"),
    proj_type == "thesis" ~ glue::glue("THESIS: {client_name_first} {client_name_last}"),
    proj_type == "pub" ~ glue::glue("PUB: {proj_title}"),
    proj_type == "grant" ~ glue::glue("GRANT: {proj_title}"),
    proj_type == "other" ~ glue::glue("Other: {proj_title}"))) %>% 
  dplyr::mutate(text = glue::glue("{record_num}, ({record_num}) {proj_title}")) %>% 
  dplyr::arrange(desc(record_num))  %>% 
  dplyr::group_by(proj_type, client_dept) %>% 
  dplyr::summarise(proj_list = paste(text, collapse = "| "))  %>%  
  dplyr::ungroup() %>% 
  dplyr::mutate(field_name  = glue::glue("project_{proj_type}_{tolower(client_dept)}")) %>% 
  dplyr::select(field_name , proj_list)




### -------- Load last grants list -------- ###

load("REDCap_pulls/all_grants.RData")


opt_grant <- df_grants %>% 
  dplyr::left_join(df_clients, by = "anum") %>% 
  dplyr::mutate(across(where(is.character), 
                       stringr::str_trim)) %>% 
  dplyr::mutate(proj_type = proj_type %>% 
                  forcats::fct_recode("diss" = "Dissertation",
                                      "thesis" = "Thesis",
                                      "pub" = "Publication/Article",
                                      "grant" = "Grant Submission",
                                      "other" = "Abstract",
                                      "other" = "Poster or Presentation",
                                      "other" = "Other")) %>% 
  dplyr::mutate(proj_title = case_when(
    proj_type == "diss" ~ glue::glue("DISSERTATION: {client_name_first} {client_name_last}"),
    proj_type == "thesis" ~ glue::glue("THESIS: {client_name_first} {client_name_last}"),
    proj_type == "pub" ~ glue::glue("PUB: {proj_title}"),
    proj_type == "grant" ~ glue::glue("GRANT: {proj_title}"),
    proj_type == "other" ~ glue::glue("Other: {proj_title}"))) %>% 
  dplyr::mutate(text = glue::glue("{record_num}, ({record_num}) {proj_title}")) %>% 
  dplyr::arrange(desc(record_num))  %>% 
  dplyr::group_by(proj_type, client_dept) %>% 
  dplyr::summarise(grant_list = paste(text, collapse = "| "))  %>%  
  dplyr::ungroup() %>% 
  dplyr::mutate(field_name  = glue::glue("project_{proj_type}_{tolower(client_dept)}")) %>% 
  dplyr::select(field_name , grant_list)


### -------- Pull existing MetaData down from REDCap -------- ###


rcon <- redcapAPI::redcapConnection("https://redcap.cehs.usu.edu/api/", 
                                    "B67A84D215F06D379D18EF208741D354")


meta_start <- REDCapR::redcap_metadata_read(redcap_uri = "https://redcap.cehs.usu.edu/api/", 
                                            token = "B67A84D215F06D379D18EF208741D354") 


# View(meta_start$data)

### -------- Update MetaData-------- ###


meta_start$data <- meta_start$data %>% 
  dplyr::left_join(opt_client, by = "field_name") %>% 
  dplyr::left_join(opt_proj, by = "field_name") %>%
  dplyr::left_join(opt_grant, by = "field_name") %>% 
  dplyr::mutate(select_choices_or_calculations = case_when(
    field_name == "meet_consult" ~ "1, Sarah Schwartz| 89, Liam O'Neil",
    !is.na(client_list) ~ client_list,
    !is.na(proj_list) ~ proj_list,
    !is.na(grant_list) ~ grant_list,
    TRUE ~ select_choices_or_calculations)) %>% 
  dplyr::select(-client_list, -proj_list) 


### -------- push new MetaData back to REDCap-------- ###



REDCapR::redcap_metadata_write(ds = meta_start$data,
                               redcap_uri = "https://redcap.cehs.usu.edu/api/", 
                               token = "B67A84D215F06D379D18EF208741D354")
