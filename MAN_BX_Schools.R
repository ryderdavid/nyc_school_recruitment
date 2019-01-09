
# install.packages("devtools")
# install.packages("spdplyr")
# install.packages("tidycensus")
# install.packages("acs")
# install.packages("rstudioapi")

library(devtools)
library(acs)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sp)
library(tmap)
library(readxl)
library(ggplot2)
library(sp)
library(rgdal)
library(spdplyr)
library(RColorBrewer)
library(viridis)
library(viridisLite)
library(tmaptools)
library(RSocrata)
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

# Collect just the ZCTAs we're interested in - Man/Bx
man_bx_zips <- zctas(state = "NY", cb = T, 
                     starts_with = c("100", "101", "102", "104"))


# Merge shapefile with application data tabulation of zip codes, removing from
# shapefile any ZCTA polygon with "NA" -- AKA 0 applications
man_bx_merge <- merge(man_bx_zips, 
                      man_bx_apps_by_zip, 
                      by.x = "GEOID10", 
                      by.y = "zip") %>% filter(!is.na(n))

man_bx_merge

# Create a single point shapefile for Samuel Gompers HS
sam_gompers_hs <- data.frame(name = "Samuel Gompers HS", lat = 40.811227, long = -73.907361)

coordinates(sam_gompers_hs) <- ~long + lat

require(raster)
projection(sam_gompers_hs) = as.character(proj4string(man_bx_merge))
dir.create("sghs_coords")
shapefile(sam_gompers_hs, "sghs_coords/sghs_coords.shp")



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
  
  regplot.yr <- tm_shape(nyc_area_zips, ylim = c(40.681061, 40.930), 
                           xlim = c(-74.041447, -73.78)) + 
    tm_fill(col = "grey90") +
    tm_shape(reg.filtered.by.year) + 
    tm_borders(lw = 1.5, alpha = .2) + 
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


# Plot by year
plot_reg_by_year(2017)
plot_reg_by_year(2018, p = "Greens")
plot_reg_by_year(2019)


















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


















nyc_sds <- readOGR(dsn = "nysd_18d", layer = "nysd")

nyc_sds


tm_shape(nyc_area_zips, 
         ylim = c(40.681061, 40.930),
         xlim = c(-74.041447, -73.78)) +
  tm_fill(col = "grey90") + 
  tm_shape(nyc_sds) + tm_borders()


tmap_man_bx_background() + 
  tm_shape(nyc_sds) + tm_borders() + tm_text(text = "SchoolDist")












# trying to plot points over nyc_sds

proj4string(recruitment_data_spdf) <- proj4string(nyc_sds)
plot(nyc_sds)
plot(recruitment_data_spdf, add = T, col = "red")

plot(recruitment_data_spdf)

recruitment_data_clean %>% filter()




plot(recruitment_data_spdf, col = "red")







