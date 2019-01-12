
# install.packages("devtools")
# install.packages("spdplyr")
# install.packages("tidycensus")
# install.packages("acs")
# install.packages("rstudioapi")


check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c('devtools', 'acs', 'tidycensus', 'tidyverse', 'tigris', 'sp', 
              'tmap', 'readxl', 'ggplot2', 'rgdal', 'spdplyr', 'RColorBrewer', 
              'tmaptools', 'viridis', 'viridisLite', 'RSocrata', 'grid', 'gridExtra', 'rstudioapi')

check.packages(packages)

# need local package gdal

library(devtools)
library(acs)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sp)
library(tmap)
library(readxl)
library(ggplot2)
library(rgdal)
library(spdplyr)
library(RColorBrewer)
library(viridis)
library(viridisLite)
library(tmaptools)
library(RSocrata)
library(grid)
library(gridExtra)
# to create grid side by side layouts of tmap plots per
# https://stackoverflow.com/questions/34344454/plot-2-tmap-objects-side-by-side
library(grid) 




set_sourcefile_wd <- function() {
  library(rstudioapi) # 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

set_sourcefile_wd()
setwd("data")


# Read in the data from each sheet
sheet_1 <- read_xlsx("historical_recruitment_data_1.xlsx", sheet = 1)
sheet_2 <- read_xlsx("historical_recruitment_data_1.xlsx", sheet = 2)
sheet_3 <- read_xlsx("historical_recruitment_data_1.xlsx", sheet = 3)


# The below three commands clean and standardize the colnames for each sheet's
# vars
str_to_lower(str_replace_all(names(sheet_1), 
                             "[^[:alnum:]]|[^[:ascii:]]", "_")) %>% 
  str_replace_all("[:punct:]+", "_") %>% 
  str_replace_all("^[:punct:]|[:punct:]$", "") -> names(sheet_1)

str_to_lower(str_replace_all(names(sheet_2), 
                             "[^[:alnum:]]|[^[:ascii:]]", "_")) %>% 
  str_replace_all("[:punct:]+", "_") %>% 
  str_replace_all("^[:punct:]|[:punct:]$", "") -> names(sheet_2)

str_to_lower(str_replace_all(names(sheet_3), 
                             "[^[:alnum:]]|[^[:ascii:]]", "_")) %>% 
  str_replace_all("[:punct:]+", "_") %>% 
  str_replace_all("^[:punct:]|[:punct:]$", "") -> names(sheet_3)


# Select just the cols we need - location and application pd. Also commonly
# format zip codes (sheet two was numeric, sheets 1 and 3 were strings with
# trailing nonsignificant 0)
sheet_1 %>% mutate(studentaddress_zip =
                     str_sub(as.character(studentaddress_zip),
                             1, 5)) -> sheet_1_trimmed

sheet_2 %>% mutate(studentaddress_zip = 
                     str_sub(as.character(studentaddress_zip),
                             1, 5)) -> sheet_2_trimmed

sheet_3 %>% mutate(studentaddress_zip =
                     str_sub(as.character(studentaddress_zip),
                             1, 5)) -> sheet_3_trimmed

# Combine the three sheets
bind_rows(sheet_1_trimmed, 
          sheet_2_trimmed, 
          sheet_3_trimmed) -> recruitment_data_man_bx_merged


# Strip extraneous year from enrollment period and rename to "year", rename
# studentaddress_zip to zip, then cast year as numeric
recruitment_data_clean <- recruitment_data_man_bx_merged %>% 
  mutate(enrollment_period = str_sub(enrollment_period, 1, 4)) %>% 
  rename(year = enrollment_period, zip = studentaddress_zip) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(studentaddress_coordinates)) %>% # to remove one pesky record in 2018...
  separate(studentaddress_coordinates, c("lat", "long"), sep = ",") %>% 
  mutate(long = as.numeric(long), lat = as.numeric(lat))

# One hand fix to correct a typo in data entry -- the above filter also gets rid
recruitment_data_clean[recruitment_data_clean$zip == "1.047", "zip"] <- "10473"

# get recruitment data clean into a spatial data frame
# need to refactor code to reference this SPDF rather than the DF?
recruitment_data_spdf <- recruitment_data_clean
coordinates(recruitment_data_spdf) <- ~long + lat
recruitment_data_spdf


# Summarize applications by zip and trim out non manhattan-bronx applications
man_bx_apps_by_zip <- recruitment_data_clean %>% 
  count(zip) %>% 
  filter(zip %in% seq(10000, 10299) | zip %in% seq(10400, 10499))


# recruitment data for manhattan and bx only
recruitment_data_man_bx <- recruitment_data_clean %>% 
  filter(zip %in% seq(10000, 10299) | zip %in% seq(10400, 10499))



options(tigris_use_cache = T)

# Download shapefiles for NYC area ZCTAs to render as background layer
nyc_area_zips <- zctas(cb = T, starts_with = c('070','076','073','100','101',
                                               '102','103','104','105','106',
                                               '107','108','109','110','111',
                                               '112','113','114'))

##### SET A REFERENCE VARIABLE HERE: CRS FOR ALL LAYERED PLOTS ##### use this
# CRS to transform projeections of any other layer being used when necessary
NYC_CRS <- proj4string(nyc_area_zips)

# Collect just the ZCTAs we're interested in - Man/Bx
man_bx_zips <- zctas(state = "NY", cb = T, 
                     starts_with = c("100", "101", "102", "104"))


# Merge shapefile with application data tabulation of zip codes, removing from
# shapefile any ZCTA polygon with "NA" -- AKA 0 applications
man_bx_merge <- merge(man_bx_zips, 
                      man_bx_apps_by_zip, 
                      by.x = "GEOID10", 
                      by.y = "zip") %>% filter(!is.na(n))



# Create a single point shapefile for Samuel Gompers HS
sam_gompers_hs <- data.frame(name = "Samuel Gompers HS", lat = 40.811227, long = -73.907361)
coordinates(sam_gompers_hs) <- ~long + lat

require(raster)
projection(sam_gompers_hs) = as.character(proj4string(man_bx_merge)) # update this to method used elsewhere
dir.create("sghs_coords")
shapefile(sam_gompers_hs, "sghs_coords/sghs_coords.shp")



## Create a "fixed" shapefile of NYC School Districts
# Download School District Shapefile from:
# https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nysd_18d.zip
nyc_sds <- readOGR(dsn = "nysd_18d", layer = "nysd") 
nyc_sds <- spTransform(nyc_sds, CRS(NYC_CRS)) # set NYC SD proj to common proj

# Perform a little surgery on NYC School District 10. SD10 is mostly in the
# Bronx, but a tiny bit of it (Marble Hill) is in Manhattan but on the Bronx
# side of the river. The shapefile divides SD into manhattan and bx sides, this
# script merges those into a borough-agnostic polygon. 

# isolate sd10's data, sum the area, drop the length. Note this will add
# problematic border length data on SD10, since we're erasing internal
# perimeter. Don't use the length. 
sd10_data <- nyc_sds@data %>% filter(SchoolDist == 10) %>%
  summarise_at(c("Shape_Area", "Shape_Leng"), sum) %>% 
  mutate(SchoolDist = as.integer(10))

# aggregate both polygons for SD10 into a single poly.
sd10_one_poly <- nyc_sds %>% filter(SchoolDist == 10) %>% aggregate()

# combine the above polygons and data into a single SPDF
sd10_spdf <- SpatialPolygonsDataFrame(sd10_one_poly, sd10_data)

# bind together shapefile of polys other than SD10 with the new merged SD10 SPDF
nyc_sds_fixed <- bind(sd10_spdf, filter(nyc_sds, SchoolDist != 10))



# Plot all three years' (2017 - 19) application data
tmap_mode('plot')

## MAN_BX BACKGROUND MAP AS FUNCTION EVERY TIME
tmap_man_bx_background <- function() {
  return(
    tm_shape(nyc_area_zips, 
             ylim = c(40.681061, 40.930),
             xlim = c(-74.041447, -73.78)) +
      tm_fill(col = "grey90")
    )
}

tmap_man_bx_zoom <- function() {
  return(
    tm_shape(nyc_area_zips, 
             ylim = c(40.77, 40.918),
             xlim = c(-73.98, -73.78)) +
      tm_fill(col = "grey90")
  )
}



# Turned plotting by registration year into a function
plot_reg_by_year <- function(yr, rec_data = recruitment_data_clean, p = "Oranges") {
  
  reg.by.zip <- rec_data %>% 
    dplyr::select(application_id, zip, lat, long, year, registration_completed_date) %>% 
    mutate(registered = ifelse(!is.na(registration_completed_date), 1, 0)) %>% 
    filter(registered == 1) %>% count(zip, year) %>% 
    transmute(zip = zip, year = year, registrations = n)
  
  # Plot selected year registrations
  reg.filtered.by.year <- filter(reg.by.zip, year == as.numeric(yr))
  # add spatial data
  reg.filtered.by.year <- merge(man_bx_zips, reg.filtered.by.year, 
                        by.x = "GEOID10", by.y = "zip")
  
  regplot.yr <- tmap_man_bx_zoom() +
    tm_shape(reg.filtered.by.year) + 
    tm_borders(lw = 1.5, alpha = .3) + 
    tm_fill(col = "registrations", 
            title = "Registrations", 
            colorNA = NULL, 
            palette = p, 
            style = "pretty") +
    tm_text(text = "GEOID10", 
            size = "registrations", 
            style = "pretty", 
            size.lim = c(10, 20),
            shadow = T, 
            legend.size.show = F, 
            fontface = "bold") +
    tm_layout(main.title = paste("Distribution of", 
                                 format(sum(reg.filtered.by.year$registrations, 
                                            na.rm = T), big.mark = ","),
                                 paste("registrations in Manhattan",
                                       "and the Bronx,\n",
                                       as.character(yr),
                                       "School Year,",
                                       "by Zip Code Tabulation Area")),
              main.title.position = "center",
              legend.position = c("left", "top"),
              main.title.size = 1.2)
  
  return(regplot.yr)
  
}



# 
# # Explore: Trying to 
# recruitment_data_clean %>% 
#   count(zip, year) %>% 
#   spread(year, n, fill = 0) %>% 
#   mutate(dist_17_18 = `2018` - `2017`, 
#          pct_chg_17_18 = `2018` - `2017` / `2017`, 
#          dist_18_19 = `2019` - `2018`, 
#          pct_chg_18_19 = `2019` - `2018` / `2018`)
# 
# 
# recruitment_data_clean[recruitment_data_clean$pct_chg]







### Question: which school district are most applications coming from?
### note: http://www.guru-gis.net/count-points-in-polygons/

plot_sd_chloropleth <- function(year = seq(1950,2050), palette = "YlOrBr", regonly = F) {
  
  yr <- year
  p <-  palette
  regonly <- regonly
  
  points <- recruitment_data_clean %>% 
    dplyr::select(application_id, year, long, lat, 
                  registration_completed_date, submission_date, year) %>% 
    mutate(registered = ifelse(!is.na(registration_completed_date), 1, 0)) %>% 
    dplyr::select(-registration_completed_date)
  
  # filter points by passed year
  points <- points %>% filter(year %in% yr)
  
  
  coordinates(points) <- ~long + lat # move coords into SPDF slot
  proj4string(points) <- CRS(NYC_CRS) # set common projection
  
  if(regonly == T) {
    points <- points %>% filter(registered == 1)
  }
  
  
  # create a label that changes format for map plotting based on years selected
  if(length(yr) == 1) {
    yrlabel <- paste(as.character(yr), " School Year", sep = "")
  } else {
    yrlabel <- paste(min(points$year), "-", max(points$year), " School Years", sep = "")
  }
  
  # requires nyc sds fixed, check how many points plot into nyc sds polys
  res <- over(points, nyc_sds_fixed)
  
  # tabulate points per school district
  points_per_sd <- as.tibble(table(res$SchoolDist)) %>% 
    rename(SchoolDist = Var1) %>% 
    mutate(SchoolDist = as.integer(SchoolDist))
  
  
  # to the SPDF of school districts, add the number of points taking place
  # in each SD
  points_per_sd_spdf <- merge(nyc_sds_fixed, points_per_sd,
                                    by.x = "SchoolDist", by.y = "SchoolDist")
  
  
  # create a pretty label variable combining SD number and number of applications
  points_per_sd_spdf <- points_per_sd_spdf %>% 
    mutate(label = paste("SD", as.character(SchoolDist), ":\n", 
                         as.character(n), sep = ""))
  
  # applications or registrations changes title and legend name
  if(regonly == T) {
    ar <- "registrations"
    ar_title <- "Registrations"
  } else {
    ar <- "applications"
    ar_title <- "Applications"
  }
  
  # plot points per SD
  tmap_mode("plot")
  
  tmap_man_bx_zoom() +
    tm_shape(points_per_sd_spdf) +
    tm_borders(alpha = 0.3, lw = 1.5) + 
    tm_fill(col = "n", title = ar, colorNA = NULL, palette = p) + 
    tm_text(text = "label", fontface = "bold", style = "pretty", 
            size = "n", legend.size.show = F, shadow = TRUE) + 
    tm_layout(main.title = paste(ar_title, "per school district,", "\n", yrlabel), 
              main.title.position = ("center"), fontface = "bold", legend.position = c("left", "top"))
  
}


### Question: where are candidate students best converting from applications to registrations?
### note: http://www.guru-gis.net/count-points-in-polygons/

plot_conversions_by_sd <- function(year = seq(1950,2050), palette = "YlOrBr") {
  
  yr <- year
  p <-  palette
  
  yr <- 2017
  p <- "YlOrBr"
  
  
  app_points <- recruitment_data_clean %>% 
    dplyr::select(application_id, year, long, lat, 
                  registration_completed_date, submission_date, year) %>% 
    mutate(registered = ifelse(!is.na(registration_completed_date), 1, 0)) %>% 
    dplyr::select(-registration_completed_date)
  
  # filter points by passed year
  app_points <- app_points %>% filter(year %in% yr)
  
  
  coordinates(app_points) <- ~long + lat # move coords into SPDF slot
  proj4string(app_points) <- CRS(NYC_CRS) # set common projection
  
  
  # create a reg_points SPDF that only contains successful registrations
  reg_points <- app_points %>% filter(registered == 1)
  
  coordinates(reg_points) <- ~long + lat # move coords into SPDF slot
  proj4string(reg_points) <- CRS(NYC_CRS) # set common projection
  
  # # create a label that changes format for map plotting based on years selected
  # if(length(yr) == 1) {
  #   yrlabel <- paste(as.character(yr), " School Year", sep = "")
  # } else {
  #   yrlabel <- paste(min(points$year), "-", max(points$year), " School Years", sep = "")
  # }
  
  # calc how many applications and registrations plot into nyc sds polys
  apps_over_sds <- over(app_points, nyc_sds_fixed)
  regs_over_sds <- over(reg_points, nyc_sds_fixed)
  
  # tabulate applications and registrations per school district
  total_apps_per_sd <- as.tibble(table(apps_over_sds$SchoolDist)) %>% 
    rename(SchoolDist = Var1, apps = n) %>% 
    mutate(SchoolDist = as.integer(SchoolDist))
  
  total_regs_per_sd <- as.tibble(table(regs_over_sds$SchoolDist)) %>% 
    rename(SchoolDist = Var1, regs = n) %>% 
    mutate(SchoolDist = as.integer(SchoolDist))
  
  # merge both attributes and get apps -> regs conversion rate
  conversions_per_sd <- merge(total_apps_per_sd, 
                              total_regs_per_sd, by = "SchoolDist") %>% 
    mutate(rate = regs / apps)
  
  # tidy the dataframe for plotting (one observation per line)
  conversions_for_plotting <- conversions_per_sd %>% filter(apps > 0) %>% gather("type", "count", 2:3)
  
  ggplot(data = conversions_for_plotting, 
         aes(x = as.factor(SchoolDist), y = count, fill = type)) + 
    geom_bar(stat = "identity") + 
    coord_flip()
  
  

  # merge apps and regs per sd with nyc sds shapefile
  apps_per_sd_spdf <- merge(nyc_sds_fixed, total_apps_per_sd,
                              by.x = "SchoolDist", by.y = "SchoolDist")
  
  regs_per_sd_spdf <- merge(nyc_sds_fixed, total_regs_per_sd,
                            by.x = "SchoolDist", by.y = "SchoolDist")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # # create a pretty label variable combining SD number and number of applications
  # points_per_sd_spdf <- points_per_sd_spdf %>% 
  #   mutate(label = paste("SD", as.character(SchoolDist), ":\n", 
  #                        as.character(n), sep = ""))
  
  # applications or registrations changes title and legend name
  if(regonly == T) {
    ar <- "registrations"
    ar_title <- "Registrations"
  } else {
    ar <- "applications"
    ar_title <- "Applications"
  }
  
  # plot points per SD
  tmap_mode("plot")
  
  tmap_man_bx_zoom() +
    tm_shape(points_per_sd_spdf) +
    tm_borders(alpha = 0.3, lw = 1.5) + 
    tm_fill(col = "n", title = ar, colorNA = NULL, palette = p) + 
    tm_text(text = "label", fontface = "bold", style = "pretty", 
            size = "n", legend.size.show = F, shadow = TRUE) + 
    tm_layout(main.title = paste(ar_title, "per school district,", "\n", yrlabel), 
              main.title.position = ("center"), fontface = "bold", legend.position = c("left", "top"))
  
}


## Plot Applications by Zip Code:
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




# demonstrate application to registration comparisons











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





