library(rmarkdown)
Sys.setenv(RSTUDIO_PANDOC = 'C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools')
rmarkdown::render(
  input = "C:\\Users\\A00315273\\Documents\\GitHub\\ClientTracking\\testing_report.Rmd",
  output_file = "C:\\Users\\A00315273\\Documents\\GitHub\\ClientTracking\\testing_report.pdf",
  output_dir = "C:\\Users\\A00315273\\Documents\\GitHub\\ClientTracking"
)