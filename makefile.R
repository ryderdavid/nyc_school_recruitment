check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# need local package gdal, units (udunits on arch), v8-3.14 (source on AUR), gcc-fortran, unixodbc

packages <- c('here', 'sf', 'raster', 'devtools', 'acs', 'tidycensus', 'tidyverse', 'tigris', 'sp', 
              'tmap', 'tmaptools', 'readxl', 'ggplot2', 'rgdal', 'spdplyr', 'RColorBrewer', 
              'viridis', 'viridisLite', 'rstudioapi', 'magrittr', 'getPass', "kableExtra", 'rmarkdown')

check.packages(packages)

# set_sourcefile_wd <- function() {
#   library(rstudioapi) # 
#   current_path <- getActiveDocumentContext()$path 
#   setwd(dirname(current_path ))
#   print( getwd() )
# }


if(!dir.exists("data")) {
  
  dir.create("data/downloads", recursive = T)
  
} else if (!dir.exists("data/downloads")) {
  
  dir.create("data/downloads")
  
}

set_sourcefile_wd <- function() {
  library(rstudioapi) #
  current_path <- getActiveDocumentContext()$path
  setwd(dirname(current_path ))
  print( getwd() )
}

set_sourcefile_wd()

source('datasets.R')

render('report.Rmd')
