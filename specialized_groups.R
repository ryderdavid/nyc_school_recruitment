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

names(sd_demo_snap_18_spatial)

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







# plot NY High Schools by % of ELL enrollment

# schools_demo_18 <- schools_demo_snapshot %>% filter(year == "2017-18")
# schools_demo_18



sch_dem_snp <- schools_demo_snapshot %>% filter(year == "2017-18") %>% 
  # filter_at(vars(grade_9, grade_10, grade_11, grade_12), all_vars(. > 0)) %>% # all school years
  dplyr::select(dbn, school_name, year, total_enrollment, num_ell, pct_ell, num_pov, pct_pov, num_swd, pct_swd)


# Filtering on just high schools - but maybe we want younger to tie closer to 
# sch_pts <- school_points %>% 
#   filter(sch_type == "High school")


sch_dem_snp_spdf <- merge(sch_pts, sch_dem_snp, 
      by.x = "ats_code", by.y = "dbn", all.x = F) %>% 
  dplyr::select(ats_code, school_name, 
                num_ell, pct_ell,
                num_pov, pct_pov,
                num_swd, pct_swd)


mean(sch_dem_snp_spdf$pct_swd)
mean(sch_dem_snp_spdf$pct_ell)

tmap_man_bx_zoom() +
  tm_shape(nyc_sds) + tm_borders() +
  tm_shape(sch_dem_snp_spdf %>% 
             filter(pct_swd > mean(sch_dem_snp_spdf$pct_swd))) + 
  tm_symbols(col = "red", size = "pct_swd")

tmap_mode("plot")
tmap_man_bx_zoom() +
  tm_shape(man_bx_tracts) + tm_borders(alpha = 0.2) + 
  tm_shape(nyc_sds) + tm_borders() +
  tm_shape(sch_dem_snp_spdf %>% 
             filter(pct_ell > mean(sch_dem_snp_spdf$pct_ell))) + 
  tm_symbols(col = "orange", size = "pct_ell")





nyc_above_avg_ell_spdf <- filter(sch_dem_snp_spdf, pct_ell > mean(sch_dem_snp_spdf$pct_ell)) %>% 
  dplyr::select(ats_code, school_name, num_ell, pct_ell)



 



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


