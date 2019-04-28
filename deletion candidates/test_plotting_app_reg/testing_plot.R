


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
example$year <- as.factor(example$year)

plot_conversions_by_sd <- function(year = seq(1950,2050), palette = "Greys", data = example) {
  
  yr <- year
  p <-  palette
  
  # This plots color of each 
  ggplot(data, aes(x = as.factor(SchoolDist), y = count)) + 
    geom_bar(stat = "identity", fill = "grey55") + 
    theme(axis.title.x = element_blank()) + 
    coord_flip() +
    # scale_color_brewer(palette = "Greys") +
    geom_bar(data = filter(data, registered == 1), 
             aes(fill = year), position = position_stack(reverse = T), stat = "identity") +
    scale_fill_brewer(palette = p) + 
    theme(legend.position = "bottom",
          legend.spacing.x = unit(0.25, 'cm'),
          legend.title = element_blank()) + 
    ggtitle(paste("Applications to Registrations: ", min(levels(data$year)), "-", max(levels(data$year)), 
                  "\n(total applications shown in grey)", sep = "")) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    xlab("School District")
  
}  







# plot applications to registrations proportions 
plot_conversions_by_sd(year = 2017:2018, palette = "Reds")
plot_conversions_by_sd(2017, palette = "YlOrBr")
plot_conversions_by_sd(2018, palette = "BuGn")
