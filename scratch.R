qtm(nyc_hs_zones)
qtm(nyc_ms_zones)


nyc_demo <- tidycensus::get_acs(geography = "tract", year = 2017, state = "NY",
                                county = c("New York", "Bronx", "Richmond", "Queens", "Kings"),
                                geometry = TRUE, 
                                table = "C16002")


acs5_vars_17 <- load_variables(2017, "acs5", cache = T)

# GET 2017 5YR ACS HOUSEHOLD LANGUAGE SPOKEN BY LIMITED ENGLISH SPEAKING STATUS
lep_hh_by_tract_C16002 <- 
  get_acs(geography = "tract", year = 2017, state = "NY", 
          county = c("New York", "Bronx", "Richmond", "Queens", "Kings"),
          output = "wide",
          geometry = T,
          variables = c(total_hh = "C16002_001E", 
                        spanish_lep = "C16002_004E",
                        indeur_lep = "C16002_007E",
                        asnpacisl_lep = "C16002_010E",
                        otherlang_lep = "C16002_013E"))

# break out the data from the sf object and summarize across rows for each
# language's limited english proficiency respondents. Make sure to not introduce
# NaN's by dividing by zero!
lep_hh_by_tract_C16002 %>% as_tibble() %>% 
  transmute(total_lep = select(., spanish_lep, indeur_lep, asnpacisl_lep, otherlang_lep) %>% rowSums(),
            pct_lep = ifelse(total_lep == 0, NA, 100 * total_lep / total_hh), GEOID = GEOID) -> total_lep_by_tract


# merge the data back into the geometry
lep_hh_by_tract_C16002 <- left_join(lep_hh_by_tract_C16002, 
                                    total_lep_by_tract, 
                                    by = c("GEOID" = "GEOID"))

lep_tracts <- lep_hh_by_tract_C16002 %>% 
  filter(!is.na(pct_lep))


lep_tracts_clipped <- st_erase(lep_tracts, nyc_water)


lep_percentile <- quantile(lep_tracts_clipped$pct_lep, .85)


tmap_mode("plot")
tm_shape(nyc_area_zips, bbox = c(manhattan_sf, bronx_sf)) + tm_fill(col = "grey90") +
tm_shape(lep_tracts_clipped %>% filter(pct_lep >= lep_percentile)) + tm_fill("pct_lep", palette = "Reds", title = "% LEP") + tm_borders(alpha = 0.3) + 
  tm_layout(legend.position = c("left", "top"), 
            legend.text.size = 0.6,
            legend.title.fontface = "bold",
            legend.title.size = 0.6,
            main.title = "% Limited English Proficiency Households by Census Tract,\n2017 ACS 5-Year Survey",
            main.title.size = 0.6,
            main.title.fontface = "bold",
            main.title.position = "center")




tm_shape(lep_hh_by_tract_C16002) + tm_fill(col = "pct_lep", palette = "Reds", showNA = F)
