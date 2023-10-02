library(rmarkdown)
render(input="Readme.Rmd", 
       output_file = "README.md",
       runtime = "static",
       clean = TRUE,
       output_format = "github_document",
       output_options = list(
       toc = TRUE, 
       toc_depth = 3,
       number_sections = TRUE))