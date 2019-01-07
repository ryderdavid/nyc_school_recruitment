
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



set_sourcefile_wd <- function() {
  library(rstudioapi) # 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

set_sourcefile_wd()
setwd("data")


# Read in the data from each sheet
sheet_1 <- read_xlsx("historical_recruitment_data.xlsx", sheet = 1)
sheet_2 <- read_xlsx("historical_recruitment_data.xlsx", sheet = 2)
sheet_3 <- read_xlsx("historical_recruitment_data.xlsx", sheet = 3)


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


# Select just the cols we need - location and application pd
sheet_1 %>% select(application_id, 
                   enrollment_period, 
                   studentaddress_zip, 
                   studentaddress_coordinates) -> sheet_1_trimmed
sheet_2 %>% select(application_id, 
                   enrollment_period, 
                   studentaddress_zip, 
                   studentaddress_coordinates) -> sheet_2_trimmed
sheet_3 %>% select(application_id, 
                   enrollment_period, 
                   studentaddress_zip, 
                   studentaddress_coordinates) -> sheet_3_trimmed

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
  separate(studentaddress_coordinates, c("lat", "long"), sep = ",")


# Summarize applications by zip and trim out non manhattan-bronx applications
man_bx_apps_by_zip <- recruitment_data_clean %>% 
  count(zip) %>% 
  filter(zip %in% seq(10000, 10299) | zip %in% seq(10400, 10499))


# neighborhoods <- readOGR(dsn = "nynta_18d", layer = "nynta")
# 
# man_bx_ntas <- neighborhoods[neighborhoods$BoroName %in% 
#                                c("Manhattan", "Bronx"), ]


# tm_shape(man_bx_ntas) + 
#   tm_fill(col = "white") + 
#   tm_borders() + 
#   tm_layout(bg.color = "grey90")

options(tigris_use_cache = T)

# Download shapefiles for NYC area ZCTAs to render as background layer
nyc_area_zips <- zctas(cb = T, starts_with = c('070','076','073','100','101',
                                               '102','103','104','105','106',
                                               '107','108','109','110','111',
                                               '112','113','114'))

# Collect just the ZCTAs we're interested in - Man/Bx
man_bx_zips <- zctas(state = "NY", cb = T, 
                     starts_with = c("100", "101", "102", "104"))






# # Bus stops - might not be the right viz
# nyc_stops <- readOGR(dsn = "nyc_bus_stops_may2018", layer = "bus_stops_nyc_may2018")
# # man_bx_stops <- nyc_stops@data[nyc_stops$NAMELSAD == "Bronx County" | nyc_stops$NAMELSAD == "New York County", ]
# 
# nyc_stops %>% filter(NAMELSAD == "Bronx County" | NAMELSAD == "New York County") -> man_bx_stops
# 
# tm_shape(man_bx_ntas) + tm_borders() + tm_shape(nyc_stops) + tm_dots(col = "red")
# 
# # Bus routes
# dir()
# readOGR(dsn = "bus_routes_nyc_nov2018", layer = "bus_routes_nyc_nov2018") -> bus_routes_nyc
# 
# tm_shape(bus_routes_nyc) + tm_lines()


# Merge shapefile with application data tabulation of zip codes, removing from
# shapefile any ZCTA polygon with "NA" -- AKA 0 applications
man_bx_merge <- merge(man_bx_zips, 
                      man_bx_apps_by_zip, 
                      by.x = "GEOID10", 
                      by.y = "zip") %>% 
  filter(!is.na(n))


# Create a single point shapefile for Samuel Gompers HS
sam_gompers_hs <- data.frame(name = "Samuel Gompers HS", lat = 40.811227, long = -73.907361)

SpatialPointsDataFrame(coords = c(sam_gompers_hs$lat, sam_gompers_hs$long),
                       proj4string = CRS(as.character(proj4string(man_bx_merge))), 
                       data = sam_gompers_hs$name) -> sghs_coords

coordinates(sam_gompers_hs) <- ~long + lat

require(raster)
projection(sam_gompers_hs) = as.character(proj4string(man_bx_merge))
dir.create("sghs_coords")
shapefile(sam_gompers_hs, "sghs_coords/sghs_coords.shp")



# Plot all three years' (2017 - 19) application data
tmap_mode('plot')

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
                                     "and the Bronx,\n2017-2019 School Years")),
            main.title.position = "center",
            legend.position = c("left", "top"),
            main.title.size = 1.2) + 
  tm_credits("Source: HUM II Recruitment Data, 2017-2019", 
             position = c("right", "bottom")) + 
  tm_shape(sam_gompers_hs) + tm_symbols(size = .5, col = "#226bf8", style = "pretty", 
                                        border.lwd = 1.5, border.col = "white", shape = 23)








