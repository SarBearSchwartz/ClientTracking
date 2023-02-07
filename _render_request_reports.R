load("REDCap_pulls/all_projects.RData")

df_projects <- df_projects[1:4,]

purrr::walk(.x = df_projects$record_num,
            ~ rmarkdown::render(
              input = "index.Rmd",
              output_file = glue::glue("request_reports/Request_{.x}.pdf"),
              params = list(record_num = {.x},
                            proj_title =  df_projects %>% 
                              dplyr::filter(record_num == {.x}) %>% 
                              dplyr::pull(proj_title),
                            proj_type =  df_projects %>% 
                              dplyr::filter(record_num == {.x}) %>% 
                              dplyr::pull(proj_type))
            )
)
