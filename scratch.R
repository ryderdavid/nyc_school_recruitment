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

# break out the 
lep_hh_by_tract_C16002 %>% as_tibble() %>% 
  transmute(total_lep = select(., spanish_lep, indeur_lep, asnpacisl_lep, otherlang_lep) %>% rowSums(),
            pct_lep = total_lep / total_hh, GEOID = GEOID) -> total_lep_by_tract



lep_hh_by_tract_C16002 <- left_join(lep_hh_by_tract_C16002, 
                                    total_lep_by_tract, 
                                    by = c("GEOID" = "GEOID"))


qtm(lep_hh_by_tract_C16002, fill = "pct_lep")
