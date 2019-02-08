## Setup datasets -------------------------------------

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# need local package gdal, units (udunits on arch), v8-3.14 (source on AUR), gcc-fortran, unixodbc

packages <- c('here', 'sf', 'raster', 'devtools', 'acs', 'tidycensus', 'tidyverse', 'tigris', 'sp',
              'tmap', 'tmaptools', 'readxl', 'ggplot2', 'rgdal', 'spdplyr', 'RColorBrewer', 
              'viridis', 'viridisLite', 'rstudioapi', 'magrittr', 'getPass', "kableExtra", 'rmarkdown', 'revgeo')

check.packages(packages)


if(!dir.exists("data")) {
  dir.create("data/downloads", recursive = T)
} else if (!dir.exists("data/downloads")) {
  dir.create("data/downloads")
}


if(Sys.getenv("CENSUS_API_KEY") == "") {
  
  census_api_key(getPass("Enter U.S. census API key to install: "), install = T, overwrite = T)
  readRenviron("~/.Renviron")
  
}


##### SET A REFERENCE VARIABLE HERE: CRS FOR ALL LAYERED PLOTS ##### use this
# CRS to transform projeections of any other layer being used when necessary

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


# get a simple block of NJ coastline to add to baselayers of maps
nj_land <- nyc_area_zips %>% filter(str_detect(GEOID10, "^07")) %>% st_union


# Filter just the ZCTAs we're interested in - Man/Bx
man_bx_zips <- nyc_area_zips %>% filter(str_detect(GEOID10, "^100|^101|^102|^103|^104"))


# Create a "fixed" shapefile of NYC School Districts - merging all of SD 10
# which is split over Bronx / Manhattan boundary. School District Shapefile
# from:
# https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nysd_18d.zip
# 
# download.file("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nysd_18d.zip", 
#   destfile = "data/nysd_18d.zip")
# unzip("nysd_18d.zip", exdir = "data/nysd_18d", overwrite = T)

# NYC's SD geojson file only pulls in data as characters, which leads sf to read
# them as factors which breaks all attempts at analysis. forcing NOT to factors
# brings everything in as characters, which lets us cast each attribute to
# numeric
nyc_sds <- st_read("https://data.cityofnewyork.us/resource/cuae-wd7h.geojson", 
                   stringsAsFactors = F) %>% 
  st_transform(crs = wgs84_crs) %>% 
  mutate(school_dist = as.numeric(school_dist), 
         shape_area = as.numeric(shape_area),
         shape_leng = as.numeric(shape_leng)) %>% 
  group_by(school_dist) %>% 
  summarize()




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
         num_pov = poverty_1, 
         pct_pov = poverty_2)



districts_demo_snapshot_1718 <- districts_demo_snapshot %>% 
  filter(year == "2017-18")

districts_demo_snapshot_1718 <- 
  inner_join(nyc_sds, districts_demo_snapshot, 
             by = c("school_dist" = "administrative_district"))





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

download.file(url = "https://data.cityofnewyork.us/download/jfju-ynrr/application%2Fzip", destfile = "data/downloads/school_points.zip")
unzip("data/downloads/school_points.zip", exdir = "data/school_points", overwrite = T)

school_points <- st_read("data/school_points/Public_Schools_Points_2011-2012A.shp")

names(school_points) %<>% 
  str_to_lower()


# clean up trailing unicode characters in ats_code
school_points$ats_code %<>% str_sub(1,6)

school_points <- st_transform(school_points, crs = wgs84_crs)

# add point geometry to school demographic data
schools_demo_snapshot <- inner_join(schools_demo_snapshot, school_points, by = c("dbn" = "ats_code")) %>% st_as_sf()

# filter out old years of schools demo snapshot
schools_demo_snapshot_1718 <- schools_demo_snapshot %>% filter(year == "2017-18")


# Get census tract shapefiles

# NYC tracts clipped to shoreline available here: 
# https://www1.nyc.gov/site/planning/data-maps/open-data/districts-download-metadata.page
nyc_tracts <- 
  st_read("http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nyct2010/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson")


nyc_tracts <- nyc_tracts %>% st_transform(crs = wgs84_crs)


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
             qn_water$gMeeometry, 
             st_water$geometry, 
             ny_water$geometry)) %>%
  st_transform(crs = wgs84_crs)




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

# set a bbox for common views of manhattan and bronx together
man_bx_bbox <- st_bbox(st_union(manhattan_sf, bronx_sf))







# HUM II RECRUITMENT DATA ------------------------------------------------------
# This project requires the spreadsheet "historical_recruitment_data.xlsx" - HUM
# II recruitment data.

# Read in the data from each sheet
sheet_1 <- read_xlsx("data/historical_recruitment_data.xlsx", sheet = 1)
sheet_2 <- read_xlsx("data/historical_recruitment_data.xlsx", sheet = 2)
sheet_3 <- read_xlsx("data/historical_recruitment_data.xlsx", sheet = 3)

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




# GET 2017 5YR ACS HOUSEHOLD LANGUAGE SPOKEN BY LIMITED ENGLISH SPEAKING STATUS
options(tigris_use_cache = TRUE)
lep_hh_by_tract_C16002 <- 
  get_acs(geography = "tract", year = 2017, state = "NY", 
          county = c("New York", "Bronx", "Richmond", "Queens", "Kings"),
          output = "wide",
          geometry = T,
          variables = c(total_hh = "C16002_001E", 
                        spanish_lep = "C16002_004E",
                        indeur_lep = "C16002_007E",
                        asnpacisl_lep = "C16002_010E",
                        otherlang_lep = "C16002_013E",
                        total_pop = "B01003_001E")) %>% 
  st_transform(crs = wgs84_crs) %>% 
  st_erase(., nyc_water)

# get County name from longform tract name in NAME, get boro name from county names
# Don't need this anymore since adding boro
lep_hh_by_tract_C16002 %<>%
  mutate(county_name = str_extract(NAME, "((?<=\\d, )[:graph:]+).+(?=\\sCounty)")) 

# break out the data from the sf object and summarize across rows for each
# language's limited english proficiency respondents. Make sure to not introduce
# NaN's by dividing by zero!
total_lep_by_tract <- lep_hh_by_tract_C16002 %>% as_tibble() %>% 
  
  # Filter out first percentile by pop tracts (often parks and cemeteries)
  filter(total_pop >= quantile(total_pop, 0.1)) %>% 
  
  # get a variable summing LEP counts of each language group as total_lep
  transmute(total_lep = select(., spanish_lep, indeur_lep, 
                               asnpacisl_lep, otherlang_lep) %>% rowSums(),
            
            # and one for percentage. If total_lep is 0, hard enter 0% for pct
            pct_lep = ifelse(total_lep == 0, 0, 100 * total_lep / total_hh), 
            
            #keep GEOID variable
            GEOID = GEOID)

# merge the data back into the geometry
lep_hh_by_tract_C16002 <- inner_join(lep_hh_by_tract_C16002, 
                                     total_lep_by_tract, 
                                     by = c("GEOID" = "GEOID"))

# prep for plotting: add school districts for each tract.
lep_hh_by_tract_C16002 <- st_join(lep_hh_by_tract_C16002, nyc_sds)

# filter to just Manhattan and Bronx
lep_hh_by_tract_C16002_manbx <- filter(lep_hh_by_tract_C16002,
                                       county_name %in% c("New York", "Bronx"))


## Work section -------------------------------------------------


pct <- 0.75
high_lep_hh_manbx <- 
  lep_hh_by_tract_C16002_manbx %>% 
  filter(pct_lep >= quantile(.$pct_lep, pct))



high_lep_bus_shelters_manbx <- st_intersection(nyc_bus_shelters, high_lep_hh_manbx)

# routes_through_tracts <- st_intersects(nyc_bus_routes, high_lep_hh_manbx)
routes_through_tracts <- st_intersects(nyc_bus_routes, lep_hh_by_tract_C16002)

st_intersects_tally_attr <-
  function(x = NULL,
           y = NULL,
           attribute = NULL) {
    require(sf)
    
    # a <- attribute
    a <- attribute
    
    
    # a list of x's length with each record containing a vector of indices of y's
    # records that intersect with that record in x
    xy <- st_intersects(x, y)
    
    
    attr_n <- c() # empty vector to fill
    for (r in 1:length(xy)) {
      n <- 0
      
      if (length(xy[[r]]) == 0) {
        n <- 0
        
      } else {
        for (i in 1:length(xy[[r]])) {
          n <- n + st_drop_geometry(y)[i, a]
          
        }
        
      }
      
      attr_n <- c(attr_n, n)
      
    }
    
    
    return(x %>% bind_cols((!!a) := attr_n))
    
  }



# Create a subset of buses that just run in Manhattan and the Bronx
manbx_bus_sgbp <- st_intersects(nyc_bus_routes, c(manhattan_sf, bronx_sf))
manbx_bus_routes <- nyc_bus_routes[lengths(manbx_bus_sgbp) > 0, ]
rm(manbx_bus_sgbp)  # get rid of helper variable

# Tally the value "total_lep" for its intersects with bus routes passing through
# tracts with the attribute
manbx_bus_routes_with_lep <- st_intersects_tally_attr(manbx_bus_routes,
                                                      lep_hh_by_tract_C16002_manbx,
                                                      "total_lep")



manbx_bus_routes_with_lep %>% top_n(25, total_lep) %>% arrange(desc(total_lep))


























nrow(nyc_bus_routes)

length(st_intersects_tally_attribute(nyc_bus_routes, lep_hh_by_tract_C16002, "total_lep"))

routes <- st_intersects()





































nyc_bus_routes_with_lep_hhs <- nyc_bus_routes %>% 
  mutate(total_lep_passthrough = st_intersects_tally_attribute(., lep_hh_by_tract_C16002, "total_lep"))





manbx_bus_routes_with_lep_hhs <- nyc_bus_routes %>% 
  bind_cols(total_lep_passthrough = st_intersects_tally_attribute(nyc_bus_routes, 
                                                                  lep_hh_by_tract_C16002,
                                                                  
                                                                  manbx_bus_routes_with_lep_hhs <- nyc_bus_routes %>% 
                                                                    bind_cols(total_lep_passthrough = st_intersects_tally_attribute(nyc_bus_routes, 
                                                                                                                                    lep_hh_by_tract_C16002,
                                                                                                                                    
                                                                                                                                    manbx_bus_routes_with_lep_hhs <- nyc_bus_routes %>% 
                                                                                                                                      bind_cols(total_lep_passthrough = st_intersects_tally_attribute(nyc_bus_routes, 
                                                                                                                                                                                                      lep_hh_by_tract_C16002,
                                                                                                                                                                                                      
                                                                                                                                                                                                      manbx_bus_routes_with_lep_hhs <- nyc_bus_routes %>% 
                                                                                                                                                                                                        bind_cols(total_lep_passthrough = st_intersects_tally_attribute(nyc_bus_routes, 
                                                                                                                                                                                                                                                                        lep_hh_by_tract_C16002,
                                                                                                                                                                                                                                                                        



tmap_mode("view")
tm_basemap(leaflet::providers$CartoDB.PositronNoLabels) + 
  tm_shape(nyc_sds, point.per = "feature", 
           bbox = st_bbox(filter(high_lep_bus_shelters_manbx, boro_name == "Bronx"))) + 
  tm_borders(alpha = 0.8) + 
  tm_text("school_dist") +
  tm_shape(high_lep_hh_manbx) + 
  tm_borders(alpha = 0.2) + 
  tm_fill(col = "pct_lep", 
          palette = "Reds",
          title = "% LEP Households",
          id = "NAME",
          showNA = F,
          popup.vars = c("Tract ID"="GEOID", "Name"="NAME", 
                         "School District"="school_dist",
                         "Total Households"="total_hh",
                         "% LEP Households"="pct_lep")) +
  tm_shape(high_lep_bus_shelters_manbx) +
  tm_dots(col = "#42f4e2",
          id = "shelter_id",
          popup.vars = c("Location" = "location",
                         "Coordinates" = "coords",
                         "Nearby LEP Households" = "total_lep",
                         "% LEP households in Tract" = "pct_lep")) + 
  # tm_shape(high_lep_routes) + tm_lines(col = "route_id", lwd = 3, palette = "Set2")