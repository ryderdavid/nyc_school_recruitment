check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# need local package gdal, units (udunits on arch), v8-3.14 (source on AUR), gcc-fortran, unixodbc

packages <- c('sf', 'devtools', 'acs', 'tidycensus', 'tidyverse', 'tigris', 'sp', 
              'tmap', 'tmaptools', 'readxl', 'ggplot2', 'rgdal', 'spdplyr', 'RColorBrewer', 
              'viridis', 'viridisLite', 'rstudioapi', 'raster', 'magrittr')

check.packages(packages)

set_sourcefile_wd <- function() {
  library(rstudioapi) # 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

set_sourcefile_wd()
setwd("data")


# API KEYS NEEDED FOR DATA:
# Save keys as plaintext files in /api_keys/ -- ignored by git

# ACS - American Communities Survey US Census - Census Key
census_key <- read_file("../api_keys/census_key") %>% trim() %>% 
  census_api_key(., install = T)



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


# create SPDF of application points
application_points_spdf <- recruitment_data
coordinates(application_points_spdf) <- ~long + lat # move coords into SPDF slot
proj4string(application_points_spdf) <- CRS(NYC_CRS) # set common projection


# Filter Application points for just registration points
registration_points_spdf <- application_points_spdf %>% 
  mutate(registered = ifelse(!is.na(registration_completed_date), T, F)) %>% 
  dplyr::select(-registration_completed_date) %>% filter(registered == T) %>% 
  dplyr::select(-registered)



options(tigris_use_cache = T)

# Download shapefiles for NYC area ZCTAs to render as background layer
nyc_area_zips <- zctas(cb = T, starts_with = c('070','071','072','073','074',
                                               '075','076','100','101','102',
                                               '103','104','105','106','107',
                                               '108','109','110','111','112',
                                               '113','114','115','116'))

##### SET A REFERENCE VARIABLE HERE: CRS FOR ALL LAYERED PLOTS ##### use this
# CRS to transform projeections of any other layer being used when necessary
NYC_CRS <- proj4string(nyc_area_zips)

# FIlter just the ZCTAs we're interested in - Man/Bx
man_bx_zips <- nyc_area_zips %>% filter(str_detect(GEOID10, "^100|^101|^102|^103|^104"))


# Create a "fixed" shapefile of NYC School Districts - merging all of SD 10
# which is split over Bronx / Manhattan boundary. School District Shapefile
# from:
# https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nysd_18d.zip

download.file("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nysd_18d.zip", 
  destfile = "nysd_18d.zip")
unzip("nysd_18d.zip", overwrite = T)

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
nyc_sds <- bind(sd10_spdf, filter(nyc_sds, SchoolDist != 10))

rm(list = c("sd10_spdf", "sd10_one_poly", "sd10_data"))





districts_demo_snapshot <- read_csv("https://data.cityofnewyork.us/resource/dndd-j759.csv")


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


# Shapefile of public school points - This is an ESRI shape file of school point
# locations based on the official address.  It includes some additional basic
# and pertinent information needed to link to other data sources. It also
# includes some basic school information such as Name, Address, Principal, and
# Principal’s contact information.

download.file(url = "https://data.cityofnewyork.us/download/jfju-ynrr/application%2Fzip", destfile = "school_points.zip")
unzip("school_points.zip", exdir = "school_points", overwrite = T)

school_points <- readOGR(dsn = "school_points", layer = "Public_Schools_Points_2011-2012A")

names(school_points) %<>% 
  str_to_lower()

# clean up trailing whitespace in ats_code
school_points$ats_code %<>% str_trim()

school_points <- spTransform(school_points, CRS(NYC_CRS))







# Get census tract shapefiles

nyc_tracts <- tidycensus::get_acs(state = "NY",
                                  variables = "B01003_001",
                                  county = c("New York", "Bronx", "Kings", "Queens", "Richmond"), 
                                  geography = "tract",
                                  geometry = T)



# replace this with a filter once I figure out how to work sf files
man_bx_tracts <- tidycensus::get_acs(state = "NY",
                                     variables = "B01003_001",
                                     county = c("New York", "Bronx"), 
                                     geography = "tract", 
                                     geometry = T)

man_bx_tracts_spdf <- as(st_geometry(man_bx_tracts), "Spatial")
