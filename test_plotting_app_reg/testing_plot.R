


check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# need local package gdal, units (udunits on arch), v8-3.14 (source on AUR), gcc-fortran, unixodbc

packages <- c('sf', 'devtools', 'acs', 'tidycensus', 'tidyverse', 'tigris', 'sp', 
              'tmap', 'tmaptools', 'readxl', 'ggplot2', 'rgdal', 'spdplyr', 'RColorBrewer', 
              'viridis', 'viridisLite', 'rstudioapi')

check.packages(packages)



set_sourcefile_wd <- function() {
  library(rstudioapi) # 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}


set_sourcefile_wd()


example <- read_csv("example.csv")

plot_conversions_by_sd <- function(year = seq(1950,2050), palette = "Greys", data = example) {
  
  yr <- year
  p <-  palette
  
  # This plots color of each 
  ggplot(sd_stats_top, aes(x = as.factor(SchoolDist), y = count)) + 
    geom_bar(stat = "identity", fill = "grey55") + coord_flip() +
    # scale_color_brewer(palette = "Greys") +
    geom_bar(data = filter(sd_stats_top, registered == 1), 
             aes(fill = year), position = position_stack(reverse = T), stat = "identity") +
    scale_fill_brewer(palette = "YlOrBr") + 
    ggtitle(paste(min(levels(sd_stats$year)), "-", max(levels(sd_stats$year)), sep = "")) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    xlab("School District") +
    guides(fill = guide_legend(title = "Year"))

  
}  







# plot applications to registrations proportions 
plot_conversions_by_sd(year = 2017:2018)
plot_conversions_by_sd(2017, palette = "YlOrBr")
plot_conversions_by_sd(2018, palette = "BuGn")