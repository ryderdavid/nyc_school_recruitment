
acs5_vars_17 <- load_variables(2017, "acs5", cache = T)

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
  transmute(total_lep = select(., spanish_lep, indeur_lep, 
                               asnpacisl_lep, otherlang_lep) %>% rowSums(),
            pct_lep = ifelse(total_lep == 0, NA, 100 * total_lep / total_hh), 
            GEOID = GEOID) -> total_lep_by_tract


# merge the data back into the geometry
lep_hh_by_tract_C16002 <- left_join(lep_hh_by_tract_C16002, 
                                    total_lep_by_tract, 
                                    by = c("GEOID" = "GEOID"))

# prep for plotting: add school districts for each tract, remove NA records,
lep_hh_tracts_ready <- st_intersection(lep_hh_by_tract_C16002, nyc_sds) %>% 
  filter(!is.na(pct_lep))
  

lep_hh_tracts_ready %>% rename(`Percent LEP` = pct_lep, `Total households` = total_hh)

tmap_mode("view")
tm_shape(nyc_sds, point.per = "feature", bbox = man_bx_bbox) + 
  tm_borders() + 
  tm_text("school_dist") +
tm_shape(lep_hh_tracts_ready) + 
  tm_borders(alpha = 0.2) + 
  tm_fill(col = "pct_lep", 
          title = "% LEP Households",
          id = "NAME",
          popup.vars = c("Tract ID"="GEOID", "Name"="NAME", 
                         "School District"="school_dist",
                         "Total Households"="total_hh",
                         "% LEP Households"="pct_lep"))





lep_tracts <- lep_hh_by_tract_C16002 %>% 
  





lep_percentile <- quantile(lep_tracts_clipped$pct_lep, .80)







# Hidden: Plot mode available for printed version of report.
tmap_mode("plot")
tm_shape(nyc_area_zips, bbox = c(manhattan_sf, bronx_sf)) + tm_fill(col = "grey90") +
tm_shape(lep_tracts) +
  tm_fill("pct_lep", palette = "Reds", style = "pretty",
          title = "% LEP households\nby census Tract") +
  tm_borders(alpha = 0.1) +
  tm_credits("Source: 2013-2017 ACS Five-Year Estimates",
             bg.color = "white", bg.alpha = 0.7, position = c("left", "bottom")) +
  tm_layout(legend.position = c("left", "top"),
            legend.height = .4,
            legend.width = .8,
            legend.title.size = 1,
            legend.title.fontface = "bold")



