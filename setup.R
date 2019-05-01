library(sf)
library(leaflet)
library(rmarkdown)
library(kableExtra)
library(magrittr)
library(tmaptools)
library(tmap)
library(ggplot2)
library(readxl)
library(tigris)
library(tidycensus)
library(acs)
library(tidyverse)
library(BAMMtools) # for getting jenks natural breaks




if(!dir.exists("data")) {
  dir.create("data/downloads", recursive = T)
} else if (!dir.exists("data/downloads")) {
  dir.create("data/downloads")
}

##### SET A REFERENCE VARIABLE HERE: CRS FOR ALL LAYERED PLOTS ##### use this
# CRS to transform projeections of any other layer being used when necessary

wgs84_crs <-  "+proj=longlat +datum=WGS84 +no_defs"



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


# get a simple block of NJ coastline to add to baselayers of maps
nj_land <- nyc_area_zips %>% filter(str_detect(GEOID10, "^07")) %>% st_union


# Filter just the ZCTAs we're interested in - Man/Bx
man_bx_zips <- nyc_area_zips %>% filter(str_detect(GEOID10, "^100|^101|^102|^103|^104"))


# Get NYCHA location geojson
nycha_locations <- 
  st_read("https://data.cityofnewyork.us/resource/5j2e-zhmb.geojson", stringsAsFactors = F) %>% 
  st_transform(crs = wgs84_crs)


# NYC's SD geojson file only pulls in data as characters, which leads sf to read
# them as factors which breaks all attempts at analysis. forcing NOT to factors
# brings everything in as characters, which lets us cast each attribute to
# numeric
nyc_sds <- st_read("https://data.cityofnewyork.us/resource/cuae-wd7h.geojson", stringsAsFactors = F) %>% 
  st_transform(crs = wgs84_crs) %>% 
  dplyr::select(-shape_area, -shape_leng) %>% 
  mutate(school_dist = as.integer(school_dist)) %>% 
  # mutate(school_dist = as.factor(school_dist)) %>% 
  group_by(school_dist) %>% 
  summarize()




districts_demo_snapshot <- 
  read_csv("https://data.cityofnewyork.us/resource/dndd-j759.csv") %>% 
  rename_all(
    funs(
      str_to_lower(.) %>% 
        str_replace_all(., "\\s", "_") %>% 
        str_replace_all(., "#", "num") %>% 
        str_replace_all(., "%", "pct") %>% 
        str_replace_all(., "\\(|\\)", "") %>% 
        str_replace_all(., "\\&", "") %>% 
        str_replace_all(., "_{2,}", "_")
    )
  ) %>% 
  mutate(administrative_district = as.integer(administrative_district)) %>% 
  # mutate(administrative_district = as.factor(administrative_district)) %>% 
  rename(num_swd = students_with_disabilities_1, 
         pct_swd = students_with_disabilities_2,
         num_ell = english_language_learners_1, 
         pct_ell = english_language_learners_2, 
         num_pov = poverty_1, 
         pct_pov = poverty_2) %>% 
  inner_join(nyc_sds, ., 
             by = c("school_dist" = "administrative_district"))

districts_demo_snapshot_1718 <- districts_demo_snapshot %>% 
  filter(year == "2017-18")


# Shapefile of public school points - This is an ESRI shape file of school point
# locations based on the official address.  It includes some additional basic
# and pertinent information needed to link to other data sources. It also
# includes some basic school information such as Name, Address, Principal, and
# Principal???s contact information.

download.file(url = "https://data.cityofnewyork.us/download/jfju-ynrr/application%2Fzip", destfile = "data/downloads/school_points.zip")
unzip("data/downloads/school_points.zip", exdir = "data/school_points", overwrite = T)

school_points <- st_read("data/school_points/Public_Schools_Points_2011-2012A.shp") %>% 
  rename_all(str_to_lower) %>% 
  # clean up trailing unicode characters in ats_code
  mutate(ats_code = str_sub(ats_code, 1, 6)) %>% 
  mutate(principal = str_to_title(principal, locale = "en")) %>% 
  st_transform(school_points, crs = wgs84_crs)

# School demographic data from NYC gov't. Will merge with above points data.
schools_demo_snapshot <- 
  read_csv("https://data.cityofnewyork.us/api/views/s52a-8aq6/rows.csv?accessType=DOWNLOAD&bom=true&format=true") %>% 
  rename_all(
    funs(
      str_to_lower(.) %>% 
        str_replace_all(., "\\s", "_") %>% 
        str_replace_all(., "#", "num") %>% 
        str_replace_all(., "%", "pct") %>% 
        str_replace_all(., "\\(|\\)", "") %>% 
        str_replace_all(., "\\&", "") %>% 
        str_replace_all(., "_{2,}", "_")
    )
  ) %>% 
  rename(num_swd = num_students_with_disabilities, 
         pct_swd = pct_students_with_disabilities,
         num_ell = num_english_language_learners, 
         pct_ell = pct_english_language_learners,
         num_pov = num_poverty, 
         pct_pov = pct_poverty) %>% 
  inner_join(school_points, ., by = c('ats_code' = 'dbn'))

# filter out old years of schools demo snapshot
schools_demo_snapshot_1718 <- schools_demo_snapshot %>% filter(year == "2017-18")




# The below sets up the necessary shapefile and function to clip any shapefile
# to the NYC shoreline (for example, the US Census Tract boundaries, which
# extend into the Hudson and East Rivers). Passing your shapefile and nyc_water
# to st_erase will clip them to the shoreline. Curiously, 
st_erase <- function(x, y) {
  st_difference(x, st_union(st_combine(y)))
}

bx_water <- tigris::area_water("NY", county = "Bronx", class = "sf")
bk_water <- tigris::area_water("NY", county = "Kings", class = "sf")
qn_water <- tigris::area_water("NY", county = "Queens", class = "sf")
st_water <- tigris::area_water("NY", county = "Richmond", class = "sf")
ny_water <- tigris::area_water("NY", county = "New York", year = 2017, class = "sf")

nyc_water <- 
  st_union(c(bx_water$geometry, 
             bk_water$geometry, 
             qn_water$geometry, 
             st_water$geometry, 
             ny_water$geometry)) %>%
  st_transform(crs = wgs84_crs)


# Get tracts (unclipped to shoreline)
trct <- 
  tigris::tracts(state = "NY", 
                 county = c('Bronx',
                            'Kings',
                            'New York',
                            'Queens',
                            'Richmond')) %>% 
  as("sf") %>%  # cast to sf
  st_transform(crs=wgs84_crs) %>% 
  
  # make easier to work with with county name
  mutate(countyname = recode(.$COUNTYFP, 
                             `005`="Bronx", 
                             `047`="Kings", 
                             `061`="New York", 
                             `081`="Queens", 
                             `085`="Richmond")) %>% 
  
  mutate(tractname = paste(NAMELSAD, ", ",
                           countyname,
                           " County",
                           sep = ""))

nyc_tracts <- st_erase(trct, nyc_water) # erase water from edges 


# NYC School Zones - at https://data.cityofnewyork.us/Education/2017-2018-School-Zones/ghq4-ydq4
nyc_hs_zones <- read_sf("https://data.cityofnewyork.us/resource/9hw3-gi34.geojson") %>% 
  st_transform(., crs = wgs84_crs)

nyc_ms_zones <- read_sf("https://data.cityofnewyork.us/resource/jxpn-gg5q.geojson") %>% 
  st_transform(., crs = wgs84_crs)

nyc_es_zones <- read_sf("https://data.cityofnewyork.us/resource/xehh-f7pi.geojson") %>% 
  st_transform(., crs = wgs84_crs)

# NYC bus routes
bus_url <- "http://faculty.baruch.cuny.edu/geoportal/data/nyc_transit/may2018/bus_routes_nyc_may2018.zip"


download.file(bus_url, "data/downloads/bus_routes_nyc_may2018.zip", cacheOK = T)
unzip("data/downloads/bus_routes_nyc_may2018.zip", exdir = "data/bus_routes", overwrite = T)
nyc_bus_routes <- st_read("data/bus_routes/bus_routes_nyc_may2018.shp")
nyc_bus_routes <- st_transform(nyc_bus_routes, crs = wgs84_crs)


# merge bus routes by route (directions currently their own shapes)
nyc_bus_routes %<>% group_by(route_id, route_shor, route_long, color) %>% 
  summarize() %>% ungroup()

# NYC bus shelters
nyc_bus_shelters <- 
  st_read("https://data.cityofnewyork.us/api/geospatial/qafz-7myz?method=export&format=GeoJSON") %>% 
  st_transform(., crs=wgs84_crs)

# nyc_bus_shelters$location <- str_to_title(nyc_bus_shelters$location, locale = "en")

nyc_bus_shelters %<>% mutate(location = str_to_title(nyc_bus_shelters$location, locale = "en"),
                             coords = paste(latitude, longitude, sep = ", "))


# NYC Neighborhood Tabulation Areas (NTAs):
nyc_ntas <- st_read("https://data.cityofnewyork.us/resource/93vf-i5bz.geojson") %>% 
  st_transform(., crs = wgs84_crs)


manhattan_sf <- nyc_ntas %>% filter(boroname == "Manhattan") %>% st_union()
bronx_sf <- nyc_ntas %>% filter(boroname == "Bronx") %>% st_union()
brooklyn_sf <- nyc_ntas %>% filter(boroname == "Brooklyn") %>% st_union()
queens_sf <- nyc_ntas %>% filter(boroname == "Queens") %>% st_union()
staten_sf <- nyc_ntas %>% filter(boroname == "Staten Island") %>% st_union()

nyc_sf <- st_union(c(manhattan_sf, bronx_sf, queens_sf, brooklyn_sf, staten_sf))

nyc_area_land <- st_union(nj_land, nyc_sf)

# set a bbox for common views of manhattan and bronx together
man_bx_bbox <- st_bbox(st_union(manhattan_sf, bronx_sf))






# RECRUITMENT DATA ------------------------------------------------------
# This project requires the spreadsheet "historical_recruitment_data.xlsx" 

# Read in the data from each sheet
sheet_19 <- read_xlsx("schooldata/historical_recruitment_data_0319.xlsx")
sheet_18 <- read_xlsx("schooldata/historical_recruitment_data.xlsx", sheet = 2)
sheet_17 <- read_xlsx("schooldata/historical_recruitment_data.xlsx", sheet = 3)

# The below three commands clean and standardize the colnames for each sheet's
# vars

names(sheet_17) <- 
  str_to_lower(str_replace_all(names(sheet_17), 
                               "[^[:alnum:]]|[^[:ascii:]]", "_")) %>% 
  str_replace_all("[:punct:]+", "_") %>% 
  str_replace_all("^[:punct:]|[:punct:]$", "")

names(sheet_18) <- 
  str_to_lower(str_replace_all(names(sheet_18), 
                               "[^[:alnum:]]|[^[:ascii:]]", "_")) %>% 
  str_replace_all("[:punct:]+", "_") %>% 
  str_replace_all("^[:punct:]|[:punct:]$", "")

names(sheet_19) <- 
  str_to_lower(str_replace_all(names(sheet_19), 
                               "[^[:alnum:]]|[^[:ascii:]]", "_")) %>% 
  str_replace_all("[:punct:]+", "_") %>% 
  str_replace_all("^[:punct:]|[:punct:]$", "")


# Select just the cols we need - location and application pd. Also commonly
# format zip codes (sheet two was numeric, sheets 1 and 3 were strings with
# trailing nonsignificant 0)
sheet_19_trimmed <- 
  sheet_19 %>% mutate(studentaddress_zip =
                        str_sub(as.character(studentaddress_zip), 1, 5))
sheet_18_trimmed <- 
  sheet_18 %>% mutate(studentaddress_zip = 
                        str_sub(as.character(studentaddress_zip), 1, 5))

sheet_17_trimmed <- 
  sheet_17 %>% mutate(studentaddress_zip =
                        str_sub(as.character(studentaddress_zip), 1, 5))

# Combine the three sheets

recruitment_data <- 
  bind_rows(sheet_17_trimmed, 
            sheet_18_trimmed, 
            sheet_19_trimmed) # used to be recruitment_data_man_bx_merged

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
  rename(year = enrollment_period) %>% 
  filter(!is.na(studentaddress_coordinates)) %>% # to remove one pesky record in 2018...
  separate(studentaddress_coordinates, c("lat", "long"), sep = ",") %>% 
  mutate(long = as.numeric(long), lat = as.numeric(lat)) %>% 
  
  # convert to sf and intersect with tracts, zips, and ntas shapefiles
  st_as_sf(coords = c("long", "lat"), 
           crs    = wgs84_crs) %>% 
  st_intersection(., nyc_area_zips) %>% 
  st_intersection(., nyc_ntas) %>% 
  st_intersection(., nyc_tracts) %>% 
  st_intersection(., nyc_sds) %>% 
  
  # rename and reclass key geography variables
  mutate(year        = as.integer(year),
         district    = as.integer(district),
         zip         = as.integer(GEOID10),
         tractname   = as.factor(paste(NAMELSAD, boroname, sep = ", "))) %>% 
  
  # add a logical variable for whether applicant was registered or not
  mutate(registered = ifelse(!is.na(registration_completed_date), T, F))

# recruitment data for manhattan and bx only
recruitment_data_man_bx <- recruitment_data %>% 
  filter(boroname %in% c("Bronx", "Manhattan"))

# SF of application points
application_points <- recruitment_data # %>% 

# SF of registration points - just those that filtered as registered
registration_points <- application_points %>% 
  dplyr::select(-registration_completed_date) %>% filter(registered == T) %>% 
  dplyr::select(-registered)



# create a tidy set of application totals of this with all years
recruitment_by_tract_tidy <-
  recruitment_data %>%
  st_drop_geometry() %>%
  count(GEOID, year, registered) %>%
  complete(GEOID, year, registered, fill = list(n = 0)) %>%
  mutate(pct_total = n / sum(n) * 100) %>%
  group_by(year) %>%
  mutate(pct_yr = n / sum(n) * 100) %>%
  ungroup() %>%
  inner_join(nyc_tracts, ., by = c("GEOID" = "GEOID"))


recruitment_by_sd_tidy <-
  recruitment_data %>%
  st_drop_geometry %>%
  count(school_dist, year, registered) %>%
  complete(school_dist, year, registered, fill = list(n = 0)) %>%
  mutate(pct_total = n / sum(n)) %>%
  group_by(year) %>%
  mutate(pct_yr = n / sum(n) * 100) %>%
  ungroup() %>%
  inner_join(nyc_sds, ., by = c("school_dist" = "school_dist"))

applications_by_tract_yearly_totals <-
  recruitment_by_tract_tidy %>%
  group_by(GEOID, tractname, year) %>%
  summarize(n = sum(n)) %>%
  group_by(year) %>%
  mutate(pct_yr = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(pct_total = n / sum(n) * 100) %>%
  filter(n > 0)

apps_jenks_breaks <- applications_by_tract_yearly_totals$n %>% getJenksBreaks(6)

registrations_by_tract_yearly_totals <-
  recruitment_by_tract_tidy %>%
  filter(registered == T, n > 0)

regs_jenks_breaks <- getJenksBreaks(registrations_by_tract_yearly_totals$n, 6)

