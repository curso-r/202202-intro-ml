library(magrittr, include.only = "%>%")



# render o arquivo --------------

rmarkdown::render(
  input = "README.Rmd",
  output_format = "github_document",
  clean = TRUE,
  output_file = "README.md"

)
