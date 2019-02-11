recruitment_data %>% glimpse





rr <- recruitment_data %>% st_drop_geometry() %>% count(school_dist, year, registered)



inner_join(rr, nyc_sds, by = c("school_dist" = "school_dist"))

inner_join(nyc_sds, rr, by = c("school_dist" = "school_dist"))



count_by_geography <- function(data = recruitment_data, 
                               geography = "tract", 
                               year.min = 0, 
                               year.max = 10000, 
                               regonly = F) {
  
  d <- data
  yrs <- seq(year.min, year.max)
  r <- regonly
  
  if (geography == "district") {
    geog <- "school_dist"
    mergegeom <- nyc_sds
  } else if (geography == "nta") {
    geog <- "ntacode" 
    mergegeom <- nyc_ntas
  } else if (geography == "tract") {
    geog <- "GEOID"
    mergegeom <- nyc_tracts
  } else if (geogrphy == "zip") {
    geog <- "GEOID10"
    mergegeom <- nyc_area_zips
  }
  
  
  # d <- recruitment_data
  # yrs <- seq(0,10000)
  # r <- F
  # 
  # geog <- "GEOID"
  # mergegeom <- nyc_tracts
  
  if (r == T) { 
    counts %<>% 
      filter(registered == T)
    }
  
  counts <- d %>% 
    st_drop_geometry() %>% 
    filter(year %in% yrs) %>% 
    count(!!as.name(geog), year, registered) %>% 
    complete(!!as.name(geog), year, registered, fill = list(n = 0))
  
  counts <- inner_join(mergegeom, counts, by = c("GEOID" = "GEOID") # having trouble passing strings here
  
  return(counts)
  
}


count_by_geography(geography = "district", year.min = 2017, year.max = 2017, regonly = T)
