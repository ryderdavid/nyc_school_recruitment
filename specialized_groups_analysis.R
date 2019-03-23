set_sourcefile_wd <- function() {
  library(rstudioapi) # 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

set_sourcefile_wd()
  
source("functions.R")




# Get school with high ELL enrollment


# Plot school districts with high ELL enrollment
tmap_mode("plot")
tm_shape(nyc_area_zips, bbox = c(manhattan_sf, bronx_sf)) +
  tm_fill(col = "grey90") + 
  tm_shape(districts_demo_snapshot_1718, point.per = "feature") + 
    tm_borders() + 
    tm_fill(col = "pct_ell", title = "% ELL Enrollment, 2017-18", palette = "BuGn") +
    tm_text("school_dist") +
    tm_layout(legend.position = c("left", "top"),
              main.title = "Percentage of ELL enrollment by district, 2017-18",
              main.title.position = "center")

# Plot schools in 95th percentile for ELL enrollment in Manhattan and The Bronx:
high_ell_schools_man_bx <- schools_demo_snapshot_1718 %>% 
  filter(pct_ell > quantile(.$pct_ell, 0.95)) %>% 
  filter(boro %in% c("X", "M")) %>%
  .[, c("dbn", "school_name", "total_enrollment", "pct_ell")]

tm_shape(nyc_area_zips, bbox = c(manhattan_sf, bronx_sf)) + 
  tm_fill(col = "grey90") +
  tm_shape(nyc_sds) + tm_borders() + tm_fill(alpha = 0) +
  tm_shape(high_ell_schools_man_bx) + 
  tm_symbols(col = "red", 
             size = "pct_ell", 
             title.size = "% ELL enrollment",
             perceptual = T, 
             sizes.legend = c(40,60,80,100),
             legend.size.is.portrait = T) + 
  tm_layout(legend.position = c("left", "top"),
            main.title = paste("Schools in Manhattan and The Bronx\n",
                               "in 95th percentile for ELL enrollment"),
            main.title.position = "center")




# Get lists of of all bus stops in that area
high_ell_schools_man_bx <- schools_demo_snapshot_1718 %>% 
  filter(pct_ell > quantile(.$pct_ell, 0.95)) %>% 
  filter(boro %in% c("X", "M")) %>%
  .[, c("dbn", "school_name", "total_enrollment", "pct_ell")]

high_ell_schools_buffer <- 
  st_buffer(high_ell_schools_man_bx, dist = 500)

high_ell_bus_shelters <- 
  st_intersection(nyc_bus_shelters, high_ell_schools_buffer)
  

# plot bus shelters within 500m of high recruiting schools
tm_shape(nyc_area_zips, bbox = c(manhattan_sf, bronx_sf)) + 
  tm_fill(col = "grey90") + 
  tm_shape(high_ell_schools_buffer) + 
  tm_fill(col = "red", alpha = 0.2) + 
  tm_shape(nyc_sds) + tm_borders(alpha = 0.5) +
  tm_shape(high_ell_bus_shelters) + 
  tm_symbols(col = "black", 
             border.col = "white", size = 0.15) + 
  tm_layout(main.title = paste(nrow(high_ell_bus_shelters), 
                               "bus shelters within 500 meters of",
                               nrow(high_ell_schools_man_bx),
                               "\n95th-percentile ELL-recruiting schools"),
            main.title.position = "center")



#output list of stops to advertise on
high_ell_bus_shelters %>% st_set_geometry(NULL) %>% 
  dplyr::select(shelter_id, location, school_name) %>% 
  kable() %>% kable_styling()



