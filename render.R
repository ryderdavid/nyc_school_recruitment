library(markdown)
library(rmarkdown)
library(knitr)

output_dir <- "./docs"

source('setup.R')

render("recruitment_history.Rmd", output_dir = output_dir, params = list(output_dir = output_dir))
render("recruitment_strategies.Rmd", output_dir = output_dir, params = list(output_dir = output_dir))

# rpubsUpload(title = 'School Recruiting Strategies', './docs/recruitment_strategies.html')



