library(rmarkdown)

output_dir <- "./docs"

source('setup.R')

render("recruitment_history.Rmd", output_dir = output_dir, params = list(output_dir = output_dir))
render("ad_targeting.Rmd", output_dir = output_dir, params = list(output_dir = output_dir))

