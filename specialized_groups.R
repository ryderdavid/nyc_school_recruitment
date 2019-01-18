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

source("functions.R")


# Plot by year
plot_reg_by_year(2017, p = "Reds")
plot_reg_by_year(2018, p = "Greens")
plot_reg_by_year(2019)


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




## Plot Applications by Zip Code:
##### TURN INTO FUNCTION I CAN CALL
tm_shape(nyc_area_zips, ylim = c(40.681061, 40.930), 
         xlim = c(-74.041447, -73.78)) + 
  tm_fill(col = "grey90") +
  
  tm_shape(man_bx_zips) + tm_fill(col = "grey90") + tm_layout(bg.color = "grey75") +
  tm_shape(man_bx_merge) + 
  tm_borders(lw = 1.5, alpha = .2) + 
  tm_fill(col = "n", title = "Applications", palette = "YlOrBr", colorNA = NULL) + 
  tm_text(text = "GEOID10", size = "n", style = "pretty", size.lim = c(100, 400),
          shadow = T, legend.size.show = F, fontface = "bold") +
  tm_layout(main.title = paste("Distribution of", 
                               format(sum(man_bx_apps_by_zip$n), big.mark = ","),
                               paste("applications in Manhattan",
                                     "and the Bronx,\n2017-2019 School Years, by Zip Code Tabulation Area")),
            main.title.position = "center",
            legend.position = c("left", "top"),
            main.title.size = 1.2) + 
  tm_credits("Source: HUM II Recruitment Data, 2017-2019", 
             position = c("right", "bottom")) + 
  tm_shape(sam_gompers_hs) + tm_symbols(size = .5, col = "#226bf8", style = "pretty", 
                                        border.lwd = 1.5, border.col = "white", shape = 23)





sd_demo_snapshot <- read_csv("https://data.cityofnewyork.us/resource/dndd-j759.csv")
sd_demo_snap_18 <- sd_demo_snapshot %>% filter(year == "2017-18")

sd_demo_snap_18_spatial <- merge(nyc_sds_fixed, sd_demo_snap_18, 
                                 by.x = "SchoolDist", by.y = "administrative_district") %>% 
  mutate(total_enrollment = as.numeric(total_enrollment))

sd_demo_snap_18_spatial

tmap_man_bx_background() + 
  tm_shape(sd_demo_snap_18_spatial) + tm_borders() +
  tm_fill(col = "english_language_learners_2", title = "% ELL") +
  tm_text(text = "SchoolDist") +
  tm_layout(main.title = paste("Percentage of ELL by district\n",
                               "total enrollment, 2018"),
            main.title.position = "center",
            legend.position = c("left","top"))


tmap_man_bx_zoom()


