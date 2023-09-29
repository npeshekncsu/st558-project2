library(rmarkdown)
render("README.Rmd",  output_format = "github_document",
       output_options = list(
       toc = TRUE, 
       toc_depth = 3,
       df_print = 'tibble',
       number_sections = TRUE))
