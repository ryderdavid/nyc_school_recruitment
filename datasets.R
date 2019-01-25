check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# need local package gdal, units (udunits on arch), v8-3.14 (source on AUR), gcc-fortran, unixodbc

packages <- c('sf', 'raster', 'devtools', 'acs', 'tidycensus', 'tidyverse', 'tigris', 'sp', 
              'tmap', 'tmaptools', 'readxl', 'ggplot2', 'rgdal', 'spdplyr', 'RColorBrewer', 
              'viridis', 'viridisLite', 'rstudioapi', 'magrittr', 'getPass')

check.packages(packages)

set_sourcefile_wd <- function() {
  library(rstudioapi) # 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}


set_sourcefile_wd()
if(!dir.exists("data")) {
  
  dir.create("data")
  
}
setwd("data")


# API KEYS NEEDED FOR DATA:
# Save keys as plaintext files in /api_keys/ -- ignored by git

# ACS - American Communities Survey US Census - Census Key
# census_key <- read_file("../api_keys/census_key") %>% trim() %>% 
#   census_api_key(., install = T)


if(Sys.getenv("CENSUS_API_KEY") == "") {
  
  census_api_key(getPass("Enter U.S. census API key to install: "), install = T, overwrite = T)
  readRenviron("~/.Renviron")
  
}


##### SET A REFERENCE VARIABLE HERE: CRS FOR ALL LAYERED PLOTS ##### use this
# CRS to transform projeections of any other layer being used when necessary
# NYC_CRS <- proj4string(nyc_area_zips)
# NYC_CRS <- st_crs(nyc_area_zips)
wgs84_crs <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"




# Download shapefiles for NYC area ZCTAs to render as background layer
options(tigris_use_cache = T)
nyc_area_zips <- 
  zctas(cb = T, starts_with = c('070','071','072','073','074',
                                '075','076','100','101','102',
                                '103','104','105','106','107',
                                '108','109','110','111','112',
                                '113','114','115','116')) %>% 
  as("sf") %>%
  st_transform(crs = wgs84_crs)



  

# FIlter just the ZCTAs we're interested in - Man/Bx
man_bx_zips <- nyc_area_zips %>% filter(str_detect(GEOID10, "^100|^101|^102|^103|^104"))


# Create a "fixed" shapefile of NYC School Districts - merging all of SD 10
# which is split over Bronx / Manhattan boundary. School District Shapefile
# from:
# https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nysd_18d.zip

download.file("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nysd_18d.zip", 
  destfile = "nysd_18d.zip")
unzip("nysd_18d.zip", overwrite = T)


nyc_sds <- st_read("https://data.cityofnewyork.us/resource/cuae-wd7h.geojson") %>% 
  st_transform(crs = wgs84_crs) %>% 
  group_by(school_dist) %>% 
  summarise(shape_area = sum(as.numeric(shape_area)), 
            shape_leng = sum(as.numeric(shape_leng)),
            geometry = st_union(geometry))



districts_demo_snapshot <- 
  read_csv("https://data.cityofnewyork.us/resource/dndd-j759.csv")

names(districts_demo_snapshot) %<>% 
  str_to_lower() %>% 
  str_replace_all("\\s", "_") %>% 
  str_replace_all("#", "num") %>% 
  str_replace_all("%", "pct") %>% 
  str_replace_all("\\(|\\)", "") %>% 
  str_replace_all("\\&", "") %>% 
  str_replace_all("_{2,}", "_")

districts_demo_snapshot <- districts_demo_snapshot %>% 
  rename(num_swd = students_with_disabilities_1, 
         pct_swd = students_with_disabilities_2,
         num_ell = english_language_learners_1, 
         pct_ell = english_language_learners_2, 
         num_pov = poverty_1, pct_pov = poverty_2)

districts_demo_snapshot <- 
  inner_join(districts_demo_snapshot, nyc_sds, 
             by = c("administrative_district" = "SchoolDist")) %>% 
  st_as_sf()

districts_demo_snapshot_1718 <- districts_demo_snapshot %>% 
  filter(year == "2017-18")

districts_demo_snapshot


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

schools_demo_snapshot <- schools_demo_snapshot %>% 
  rename(num_swd = num_students_with_disabilities, pct_swd = pct_students_with_disabilities,
         num_ell = num_english_language_learners, pct_ell = pct_english_language_learners,
         num_pov = num_poverty, pct_pov = pct_poverty)

schools_demo_snapshot_1718 <- schools_demo_snapshot %>% filter(year == "2017-18")

# Shapefile of public school points - This is an ESRI shape file of school point
# locations based on the official address.  It includes some additional basic
# and pertinent information needed to link to other data sources. It also
# includes some basic school information such as Name, Address, Principal, and
# Principal’s contact information.

download.file(url = "https://data.cityofnewyork.us/download/jfju-ynrr/application%2Fzip", destfile = "school_points.zip")
unzip("school_points.zip", exdir = "school_points", overwrite = T)

school_points <- st_read("school_points/Public_Schools_Points_2011-2012A.shp")

names(school_points) %<>% 
  str_to_lower()


# clean up trailing whitespace in ats_code
school_points$ats_code %<>% str_trim()

school_points <- st_transform(school_points, crs = wgs84_crs)

# add point geometry to school demographic data
schools_demo_snapshot <- inner_join(schools_demo_snapshot, school_points, by = c("dbn" = "ats_code")) %>% st_as_sf()



# Get census tract shapefiles

# NYC tracts clipped to shoreline available here: 
# https://www1.nyc.gov/site/planning/data-maps/open-data/districts-download-metadata.page
nyc_tracts <- 
  st_read("http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nyct2010/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson")



nyc_tracts <- nyc_tracts %>% st_transform(crs = wgs84_crs)

# NYC School Zones - at https://data.cityofnewyork.us/Education/2017-2018-School-Zones/ghq4-ydq4
nyc_hs_zones <- read_sf("https://data.cityofnewyork.us/resource/9hw3-gi34.geojson") %>% 
  st_transform(., crs = wgs84_crs)

nyc_ms_zones <- read_sf("https://data.cityofnewyork.us/resource/jxpn-gg5q.geojson") %>% 
  st_transform(., crs = wgs84_crs)

nyc_es_zones <- read_sf("https://data.cityofnewyork.us/resource/xehh-f7pi.geojson") %>% 
  st_transform(., crs = wgs84_crs)

# NYC bus routes
bus_url <- "http://faculty.baruch.cuny.edu/geoportal/data/nyc_transit/may2018/bus_routes_nyc_may2018.zip"
if (dir.exists("bus_routes") == F) {
  dir.create("bus_routes")
  }

download.file(bus_url, "bus_routes/bus_routes_nyc_may2018.zip")
unzip("bus_routes/bus_routes_nyc_may2018.zip", overwrite = T)
nyc_bus_routes <- st_read("bus_routes_nyc_may2018.shp")
nyc_bus_routes <- st_transform(nyc_bus_routes, crs = wgs84_crs)

# NYC bus shelters
nyc_bus_shelters <- 
  st_read("https://data.cityofnewyork.us/api/geospatial/qafz-7myz?method=export&format=GeoJSON") %>% 
  st_transform(., crs=wgs84_crs)

nyc_bus_shelters$location <- str_to_title(nyc_bus_shelters$location, locale = "en")


# NYC Neighborhood Tabulation Areas (NTAs):
nyc_ntas <- st_read("https://data.cityofnewyork.us/resource/93vf-i5bz.geojson") %>% 
  st_transform(., crs = wgs84_crs)


manhattan_sf <- nyc_ntas %>% filter(boroname == "Manhattan") %>% st_union()
bronx_sf <- nyc_ntas %>% filter(boroname == "Bronx") %>% st_union()
brooklyn_sf <- nyc_ntas %>% filter(boroname == "Brooklyn") %>% st_union()
queens_sf <- nyc_ntas %>% filter(boroname == "Queens") %>% st_union()
staten_sf <- nyc_ntas %>% filter(boroname == "Staten Island") %>% st_union()

nyc_sf <- st_union(c(manhattan_sf, bronx_sf, queens_sf, brooklyn_sf, staten_sf))


# HUM II RECRUITMENT DATA ------------------------------------------------------
# This project requires the spreadsheet "historical_recruitment_data.xlsx" - HUM
# II recruitment data.

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
          sheet_3_trimmed) -> recruitment_data # used to be recruitment_data_man_bx_merged

# Clean up unneeded vars
rm(list = c("sheet_1", 
            "sheet_1_trimmed", 
            "sheet_2", 
            "sheet_2_trimmed", 
            "sheet_3", 
            "sheet_3_trimmed"))


# Strip extraneous year from enrollment period and rename to "year", rename
# studentaddress_zip to zip, then cast year as numeric
recruitment_data %<>% 
  mutate(enrollment_period = str_sub(enrollment_period, 1, 4)) %>% 
  rename(year = enrollment_period, zip = studentaddress_zip) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(studentaddress_coordinates)) %>% # to remove one pesky record in 2018...
  separate(studentaddress_coordinates, c("lat", "long"), sep = ",") %>% 
  mutate(long = as.numeric(long), lat = as.numeric(lat))

# One hand fix to correct a typo in data entry
recruitment_data[recruitment_data$zip == "1.047", "zip"] <- "10473"


# recruitment data for manhattan and bx only
recruitment_data_man_bx <- recruitment_data %>% 
  filter(zip %in% seq(10000, 10299) | zip %in% seq(10400, 10499))

# convert recruitment_data to simple features file
recruitment_data <- recruitment_data %>% st_as_sf(coords = c("long", "lat"), crs = wgs84_crs)

# SF of application points
application_points <- recruitment_data %>% 
  mutate(registered = ifelse(!is.na(registration_completed_date), T, F))

# SF of registration points - just those that filtered as registered
registration_points <- application_points %>% 
  dplyr::select(-registration_completed_date) %>% filter(registered == T) %>% 
  dplyr::select(-registered)

set_sourcefile_wd()
