library(tidyverse)
library(magrittr)
library(readxl)
library(purrr)
library(lubridate)
library(sf)
library(tigris)


# define a few common variables:
wgs84 <- '+init=epsg:4326'


# Import dataset, clean names, and select desired columns
schooldata_path <- 'schooldata/historical_recruitment_data.xlsx'
sheets <- excel_sheets(schooldata_path)


school_data_raw <- 
  map_df(sheets, ~ read_excel(schooldata_path, sheet = .x, col_types = 'text')) %>% 
  rename_all(str_to_lower) %>% 
  rename_all(str_replace_all, '\\s+', '_') %>% 
  rename_all(str_replace_all, '[^[:ascii:]]|(?!\\_)[[:punct:]]', '') %>% 
  rename_all(str_to_lower)


school_data_trimmed <-
  school_data_raw %>% 
    transmute(
      application_id,
      school_year = as.integer(str_extract(enrollment_period, '\\d{4}(?=-)')),
      grade_applying_to = as.integer(grade_applying_to),
      application_status,
      registered_bool = ifelse(!is.na(accepted_date), TRUE, FALSE),
      student_gender,
      submission_date = mdy_hms(submission_date, tz = 'America/New_York'),
      lat = as.numeric(str_extract(studentaddress_coordinates, '[:graph:]+(?=,)')),
      long = as.numeric(str_extract(studentaddress_coordinates, '(?<=,)[:graph:]+'))
      ) %>% 
  filter(!is.na(long) & !is.na(lat)) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = wgs84)

###############################################################################


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
ny_water <- tigris::area_water("NY", county = "New York", class = "sf")

nyc_water <- 
  st_union(c(bx_water$geometry, 
             bk_water$geometry, 
             qn_water$geometry, 
             st_water$geometry, 
             ny_water$geometry)) %>%
  st_transform(crs = wgs84)



# Get tracts (unclipped to shoreline)
nyc_tracts <- 
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
                           sep = "")) %>% 
  
  # erase water from land shapes
  st_erase(., nyc_water)

###############################################################################


# NYC's School Districts geojson file only pulls in data as characters, which
# leads sf to read them as factors which breaks all attempts at analysis.
# forcing NOT to factors brings everything in as characters, which lets us cast
# each attribute to numeric
nyc_sds <- st_read("https://data.cityofnewyork.us/resource/cuae-wd7h.geojson", 
                   stringsAsFactors = F) %>% 
  st_transform(crs = wgs84_crs) %>% 
  select(-shape_area, -shape_leng) %>% 
  mutate(school_dist = as.integer(school_dist)) %>% 
  group_by(school_dist) %>% 
  summarize()


