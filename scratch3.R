applications_by_tract_tidy <- 
  recruitment_data %>% 
  st_drop_geometry() %>% 
  count(GEOID, year, registered) %>% 
  complete(GEOID, year, registered, fill = list(n = 0)) %>% 
  inner_join(nyc_tracts, ., by = c("GEOID" = "GEOID"))

registrations_by_tract_tidy <- 
  applications_by_tract_tidy %>% 
  filter(registered == T)





tm_tiles(providers$CartoDB.PositronNoLabels) + 
  tm_shape(nyc_sds, point.per = "feature") +
  tm_borders(alpha = 0.7) +
  tm_text(text = "school_dist") +
  tm_shape(applications_by_tract_yearly_totals %>% 
             filter(year == 2017)) + 
  tm_borders(alpha = 0.2) +
  tm_fill(col = "n",
          id="tractname",
          breaks = apps_jenks_breaks,
          title = "2017 Apps",
          popup.vars = c("Applications"="n",
                         "% of Year's Applications"="pct_yr")) +
  tm_view(set.view = c(-73.86652, 40.85263, 11),
          set.zoom.limits = c(11,14)) + 
  tm_layout(outer.margins = 20)    




pal <- colorBin("YlOrBr", domain = recruitment_by_sd_tidy %>% 
                  filter(year == 2017))

leaflet() %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  setView(lng = -73.86652, lat = 40.85263, zoom = 11) %>% 
  addPolygons(data = recruitment_by_tract_tidy)
  









































recruitment_data %>% glimpse





rr <- recruitment_data %>% st_drop_geometry() %>% count(school_dist, year, registered)



inner_join(rr, nyc_sds, by = c("school_dist" = "school_dist"))

inner_join(nyc_sds, rr, by = c("school_dist" = "school_dist"))


# produces counts of points in recruitment data (number of records) that fall in
# each polygon in the provided eography file. It can also limit by a span of
# years, and filter to just regstered applicants.
apps_by_geography <- function(data = recruitment_data, 
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
  
  # for testing
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
  

  counts <- inner_join(mergegeom, counts, by=setNames(nm=geog, geog)) # having trouble passing strings here
  # counts
  
  return(counts)
  
}


apps_by_geography(geography = "district", year.min = 2017, year.max = 2017, regonly = T)
