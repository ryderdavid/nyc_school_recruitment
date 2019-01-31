vars <- tidycensus::load_variables(2017, "acs5", cache = T)

vars %>% filter(str_detect(label, "total population"))

tidycensus::get_acs()





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
                        otherlang_lep = "C16002_013E")) %>% 
  st_transform(crs = wgs84_crs) %>% 
  st_erase(., nyc_water)




lep_hh_with_pop <- 
  inner_join(lep_hh_by_tract_C16002, 
             tract_populations, by = c("GEOID" = "GEOID"))

low_pop_tracts <- lep_hh_with_pop %>% filter(total_pop <= quantile(total_pop, 0.01)) 
high_lep_tracts <- lep_hh_with_pop %>% filter(pct_lep >= quantile(pct_lep, 0.75))

tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) + 
tm_shape(high_lep_tracts) + tm_fill(col = "red", alpha = 0.5) + 
  tm_shape(low_pop_tracts) + tm_fill(col = "green", alpha = 0.5)





# Get tract populations from ACS data
tract_populations_B01003 <- 
  get_acs(geography = "tract", year = 2017, state = "NY", 
          county = c("New York", "Bronx", "Richmond", "Queens", "Kings"),
          output = "wide",
          geometry = F,
          table = "B01003") %>% select(GEOID, B01003_001E) %>% 
  rename(total_pop = B01003_001E)

lep_hh




 















