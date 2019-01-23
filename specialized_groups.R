check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# To build on linux, need local package gdal, units (udunits on arch), v8-3.14 (source on AUR), gcc-fortran, unixodbc

packages <- c('sf', 'devtools', 'acs', 'tidycensus', 'tigris', 'sp', 
              'tmap', 'tmaptools', 'readxl', 'ggplot2', 'rgdal', 'spdplyr', 'RColorBrewer', 
              'viridis', 'viridisLite', 'rstudioapi', 'tidyverse', 'getPass')

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
plot_reg_by_year(2019) # this shouldn't work yet.


# plot applications and registrations by school district
tmap_arrange(plot_sd_choropleth(year = 2017), 
             plot_sd_choropleth(year = 2018, palette = "BuGn"), 
             plot_sd_choropleth(year = 2019, palette = "Reds"), 
             nrow = 1, ncol = 3, asp = .85)

tmap_arrange(plot_sd_choropleth(year = 2017, regonly = T), 
             plot_sd_choropleth(year = 2018, palette = "BuGn", regonly = T),
             nrow = 1, ncol = 2, asp = NA)



# plot applications to registrations proportions 
plot_conversions_by_sd(year = 2017:2018)
plot_conversions_by_sd(2017, palette = "YlOrBr")
plot_conversions_by_sd(2018, palette = "BuGn")



plot_app_to_reg_by_year(year = 2017:2018, palette="Set1", top.n = 10)








# Map ELLs in 2018
sd_demo_snap_18 <- districts_demo_snapshot %>% filter(year == "2017-18")

sd_demo_snap_18_spatial <- merge(nyc_sds, sd_demo_snap_18, 
                                 by.x = "SchoolDist", 
                                 by.y = "administrative_district") %>% 
  mutate(total_enrollment = as.numeric(total_enrollment))



tmap_mode("plot")

sd_demo_snap_18_spatial <- sd_demo_snap_18_spatial %>% 
  mutate(name_label = paste("SD", SchoolDist, pct_ell, sep = ""))

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






###### CURRENTLY WORKING ON: 

sch_dem_snp <- schools_demo_snapshot %>% filter(year == "2017-18") %>%
  dplyr::select(dbn, school_name, year, total_enrollment, num_ell, 
                pct_ell, num_pov, pct_pov, num_swd, pct_swd)

sch_dem_snp <- inner_join(school_points, sch_dem_snp, by=c("ats_code" = "dbn"))


avg_swd <- mean(sch_dem_snp$pct_swd)
avg_ell <- mean(sch_dem_snp$pct_ell)

# filter schools spdf for schools in 90th or greater percentile of % ELL
# enrollment
schools_with_high_ell <- sch_dem_snp %>% filter(pct_ell > quantile(pct_ell, .9))
schools_with_high_ell

hs <- sch_dem_snp %>% filter(str_detect(grades, "09,10,11,12"))

hs_zones <- st_join(nyc_hs_zones, hs)

hs_high_ell <- hs_zones %>% filter(pct_ell > quantile(pct_ell, 0.9, na.rm = T))

tmap_man_bx_background() +
  tm_shape(hs_high_ell) + tm_borders() + tm_dots()



tracts_with_high_ell <- st_join(nyc_tracts, schools_with_high_ell) %>%
  filter(!is.na(pct_ell))

tracts_with_high_ell

tmap_man_bx_background() +
  tm_shape(nyc_tracts) + tm_borders(alpha = 0.1) +
  tm_shape(tracts_with_high_ell) + tm_fill(col = "blue") + tm_dots()

zips_with_high_ell <- st_join(nyc_area_zips, schools_with_high_ell) %>% 
  filter(!is.na(pct_ell))











nyc_hs_zones



tmap_man_bx_background() + tm_shape(schools_with_high_ell) +
  tm_shape()
+ tm_dots(size = "pct_ell")

 -> test_1

test_1 %<>% filter(!is.na(pct_swd))

plot(st_geometry(test_1))

st_join(schools_with_high_ell, nyc_tracts, join = st_intersects) -> test_1

test_1

tmap_man_bx_background() + tm_shape(test_1) + tm_borders() + tm_fill(col = "red")


























# plot all schools in the 90th+ percentile for ELL enrollment
tmap_man_bx_zoom() +
  tm_shape(nyc_sds) + tm_borders() +
  tm_shape(nyc_area_zips) + tm_borders(alpha = 0.2) +
  tm_shape(sch_dem_snp %>% filter(pct_ell > quantile(sch_dem_snp$pct_ell, 0.9))) + 
  tm_symbols(col = "red", size = 0.1)




 
filter(sch_dem_snp, pct_ell > quantile(sch_dem_snp$pct_ell, .9)) %>% nrow




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


