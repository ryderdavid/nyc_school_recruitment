library(rmarkdown)

output_dir <- "./docs"
render("report19.Rmd", output_dir = output_dir, params = list(output_dir = output_dir))
