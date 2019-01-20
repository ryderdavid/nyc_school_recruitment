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

source("functions.R")

set_sourcefile_wd()


# Plot by year
plot_reg_by_year(2017, p = "Reds")
plot_reg_by_year(2018, p = "Greens")
plot_reg_by_year(2019) # this shouldn't work yet.


# plot applications and registrations by school district
tmap_arrange(plot_sd_chloropleth(year = 2017), 
             plot_sd_chloropleth(year = 2018, palette = "BuGn"), 
             plot_sd_chloropleth(year = 2019, palette = "Reds"), 
             nrow = 1, ncol = 3, asp = .85)

tmap_arrange(plot_sd_chloropleth(year = 2017, regonly = T), 
             plot_sd_chloropleth(year = 2018, palette = "BuGn", regonly = T), 
             nrow = 1, ncol = 2, asp = NA)



# plot applications to registrations proportions 
plot_conversions_by_sd(year = 2017:2018)
plot_conversions_by_sd(2017, palette = "YlOrBr")
plot_conversions_by_sd(2018, palette = "BuGn")



plot_app_to_reg_by_year(year = 2017:2018, palette="Set1", top.n = 10)






# Get maps of distribution of demographic indicators in NYC School Districts. 
districts_demo_snapshot <- read_csv("https://data.cityofnewyork.us/resource/dndd-j759.csv")
sd_demo_snap_18 <- districts_demo_snapshot %>% filter(year == "2017-18")




sd_demo_snap_18_spatial <- merge(nyc_sds_fixed, sd_demo_snap_18, 
                                 by.x = "SchoolDist", by.y = "administrative_district") %>% 
  mutate(total_enrollment = as.numeric(total_enrollment))

sd_demo_snap_18_spatial <- sd_demo_snap_18_spatial %>% mutate(name_label = paste("SD", SchoolDist, sep = ""))

# Map ELLs in 2018
tmap_mode("plot")
tmap_nyc_background() +
  tm_shape(sd_demo_snap_18_spatial) + tm_borders() +
  tm_fill(col = "english_language_learners_2", title = "% ELL",
          palette = "Blues") +
  tm_text(text = "name_label") +
  tm_layout(main.title = paste("Percentage of ELL by district\n",
                               "total enrollment, 2018"),
            main.title.position = "center",
            legend.position = c("left","top")) + 
  tm_credits(paste("NYC Department of Education,", 
                   "\n2013 - 2018 Demographic Snapshot - District"),
             position = c("center", "BOTTOM"),
             align = "center")

# Map ELLs in 2018 - zoomed:
tmap_mode("plot")
tmap_man_bx_background() +
  tm_shape(sd_demo_snap_18_spatial) + tm_borders() +
  tm_fill(col = "english_language_learners_2", title = "% ELL",
          palette = "Blues") +
  tm_text(text = "name_label") +
  tm_layout(main.title = paste("Percentage of ELL by district\n",
                               "total enrollment, 2017-18"),
            main.title.position = "center",
            legend.position = c("left","top"))






# First, creating a dataset of demographic data of all of NYC's schools in 2018.
# Then, we will pull in a shapefile of points of NYC schools. Then, merge the files and plot by which are most 


# Data from NYC Data site, 2013-2018 Schools Demographics Snapshot, Export CSV for Excel format
schools_demo_snapshot <- 
  read_csv("https://data.cityofnewyork.us/api/views/s52a-8aq6/rows.csv?accessType=DOWNLOAD&bom=true&format=true")



names(schools_demo_snapshot) %<>% 
  str_to_lower() %>% 
  str_replace_all("\\s", "_") %>% 
  str_replace_all("#", "num") %>% 
  str_replace_all("%", "pct") %>% 
  str_replace_all("\\(|\\)", "") %>% 
  str_replace_all("\\&", "") %>% 
  str_replace_all("_{2,}", "_")

schools_demo_18 <- schools_demo_snapshot %>% filter(year == "2017-18")
schools_demo_18

nrow(schools_demo_snapshot)


download.file(url = "https://data.cityofnewyork.us/download/jfju-ynrr/application%2Fzip", destfile = "school_points.zip")
unzip("school_points.zip", exdir = "school_points", overwrite = T)

school_points <- readOGR(dsn = "school_points", layer = "Public_Schools_Points_2011-2012A") 
school_points <- spTransform(school_points, CRS(NYC_CRS))

school_points$ATS_CODE


tmap_mode("view")
tm_tiles("CartoDB.PositronNoLabels") +
tm_shape(sd_demo_snap_18_spatial) + tm_borders() +
  tm_fill(col = "english_language_learners_2", title = "% ELL",
          palette = "BuGn") +
  tm_text(text = "name_label") +
  tm_layout(main.title = paste("Percentage of ELL by district\n",
                               "total enrollment, 2018"),
            main.title.position = "center",
            legend.position = c("left","top"))


tmap_man_bx_zoom()


