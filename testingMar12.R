library(tidyverse)

nycha <- read_csv("https://data.cityofnewyork.us/api/views/evjd-dqpz/rows.csv")

nycha %<>% 
  rename_all(funs(str_replace_all(., "\\s", "_"))) %>% 
  rename_all(funs(str_to_lower(.)))

nycha %<>% 
  mutate(geocode_loc = paste(development, 
                             "NYCHA",
                             borough,
                             sep = ", "))

nycha
  
nycha_locations


tmap_mode("view")


nycha_locations <- inner_join(nycha_locations, nycha, by = c("tds_num" = "tds#"))

nycha_locations %>% revgeo


recruitment_data %>% 
  pull(year) %>% 
  unique() %>% 
  sort()


for (i in sort(unique(recruitment_data$year))) {
  print()
}



recruitment_data %>% 
  filter(year == 2017) %>% 
  count(., registered)



recruitment_data %>% 
  filter(year %in% c(2017, 2018)) %>% 
  st_drop_geometry() %>% 
  count(year, registered) %>% 
  spread(registered, n) %>% 
  transmute(Year = year,
            Applied = `TRUE` + `FALSE`,
            Registered = `TRUE`,
            `Registration Rate` = Registered / Applied,
            `To Reach 1200` = 1200 / `Registration Rate`) %>% 
  kable()




  
  
