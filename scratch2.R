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
                        otherlang_lep = "C16002_013E")) %>% 
  st_transform(crs = wgs84_crs) %>% 
  st_erase(., nyc_water)

# break out the data from the sf object and summarize across rows for each
# language's limited english proficiency respondents. Make sure to not introduce
# NaN's by dividing by zero!
lep_hh_by_tract_C16002 %>% as_tibble() %>% 
  filter(total_hh != 0) %>% # remove zero occupancy tracts like airports and parks
  transmute(total_lep = select(., spanish_lep, indeur_lep, 
                               asnpacisl_lep, otherlang_lep) %>% rowSums(),
            pct_lep = ifelse(total_lep == 0, 0, 100 * total_lep / total_hh), 
            GEOID = GEOID) -> total_lep_by_tract


# merge the data back into the geometry
lep_hh_by_tract_C16002 <- inner_join(lep_hh_by_tract_C16002, 
                                     total_lep_by_tract, 
                                     by = c("GEOID" = "GEOID"))

# prep for plotting: add school districts for each tract, remove NA records,
lep_hh_by_tract_C16002 <- st_join(lep_hh_by_tract_C16002, nyc_sds) # %>% 
# filter(!is.na(pct_lep))


tmap_mode("view")
tm_basemap(leaflet::providers$CartoDB.PositronNoLabels) + 
  tm_shape(nyc_sds, point.per = "feature", bbox = man_bx_bbox) + 
  tm_borders(alpha = 0.8) + 
  tm_text("school_dist") +
  tm_shape(lep_hh_by_tract_C16002) + 
  tm_borders(alpha = 0.2) + 
  tm_fill(col = "pct_lep", 
          palette = "Reds",
          title = "% LEP Households",
          id = "NAME",
          showNA = F,
          popup.vars = c("Tract ID"="GEOID", "Name"="NAME", 
                         "School District"="school_dist",
                         "Total Households"="total_hh",
                         "% LEP Households"="pct_lep"))



lep_hh_pctile <- quantile(lep_hh_by_tract_C16002, 0.9)

lep_hh_pctile_slice <- lep_hh_by_tract_C16002 %>% filter(pct_lep >= lep_hh_pctile)

bus_routes_in_lep_hhs <- nyc_bus_routes[lep_hh_pctile_slice, ]

qtm(bus_routes_in_lep_hhs)

qtm(nyc_bus_routes)


tmap_mode("view")
tm_basemap(leaflet::providers$CartoDB.PositronNoLabels) + 
  tm_shape(nyc_sds, point.per = "feature", bbox = man_bx_bbox) + 
  tm_borders(alpha = 0.8) + 
  tm_text("school_dist") +
  tm_shape(lep_hh_by_tract_C16002 %>% 
             filter(!is.na(pct_lep)) %>% 
             filter(pct_lep >= lep_hh_pctile)
  ) + 
  tm_borders(alpha = 0.2) + 
  tm_fill(col = "pct_lep", 
          palette = "Reds",
          title = "% LEP Households",
          id = "NAME",
          showNA = F,
          popup.vars = c("Tract ID"="GEOID", "Name"="NAME", 
                         "School District"="school_dist",
                         "Total Households"="total_hh",
                         "% LEP Households"="pct_lep"))