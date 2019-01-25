set_sourcefile_wd <- function() {
  library(rstudioapi) # 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

set_sourcefile_wd()

source("functions.R")


# % of ELL enrollment as percentage of total enrollment, by district:
tm_shape(nyc_area_zips, bbox = st_union(manhattan_sf, bronx_sf)) +
  tm_fill(col = "grey90") + 
  tm_shape(districts_demo_snapshot_1718, point.per = "feature") + 
  tm_borders() + 
  tm_fill(col = "pct_ell", title = "% ELL Enrollment, 2017-18", palette = "BuGn") +
  tm_text("administrative_district") +
  tm_layout(legend.position = c("left", "top"))




high_ell_schools_man_bx <- schools_demo_snapshot_1718 %>% 
  filter(pct_ell > quantile(.$pct_ell, 0.95)) %>% 
  filter(boro %in% c("X", "M")) %>% 
  .[, c("dbn", "school_name", "total_enrollment", "pct_ell")]


# plot location of all 95th percentile by ELL enrollment schools





# Plot location of all bus stops in that area





# plot all dense housing in vicinity of high ELL schools






#output list of stops to advertise on




# output bus lines to advertise on





high_ell_schools_buffer <- 
  st_buffer(high_ell_schools_man_bx$geometry, dist = 500) %>% 
  st_union()


high_ell_bus_shelters <- 
  st_intersection(nyc_bus_shelters, high_ell_schools_buffer)

high_ell_bus_routes <- 
  st_intersection(nyc_bus_routes, high_ell_schools_buffer)


# create a dataframe of high ELL shelters
high_ell_bus_shelters %>% st_set_geometry(NULL) %>% dplyr::select(shelter_id, location, longitude, latitude)



ntas_with_high_ell <- st_join(nyc_ntas, high_ell_schools_man_bx)
  filter(!is.na(pct_ell))

qtm(ntas_with_high_ell)

tmap_mode("plot")
tm_shape(nyc_area_zips, bbox = st_bbox(high_ell_schools_buffer)) + tm_borders(alpha = 0.2) +
  tm_fill(col = "grey90") +
tm_shape(high_ell_schools_buffer) + 
  tm_fill(col = "red", alpha = 0.3) +
tm_shape(high_ell_bus_shelters) + 
  tm_symbols(col = "black", size = 0.1) # +
  tm_layout(main.title = paste(nrow(high_ell_bus_shelters), "bus shelters are within 500m of Manhattan and Bronx Schools"))

  
tmap_mode("view")
tm_shape(high_ell_schools_buffer) + 
  tm_fill(col = "red", alpha = 0.3) +
tm_shape(high_ell_schools_man_bx) + 
  tm_symbols(col = "red", size = 0.05) +
tm_shape(high_ell_bus_shelters) + 
  tm_symbols(col = "black", size = 0.05) # +
tm_layout(main.title = paste(nrow(high_ell_bus_shelters), "bus shelters are within 500m of Manhattan and Bronx Schools"))
  
tmap_mode("plot")
tm_shape(nyc_area_zips, bbox = st_bbox(high_ell_schools_buffer)) + tm_fill(col = "grey90") +
tm_shape(high_ell_schools_buffer) + tm_fill(col = "red", alpha = 0.2) + tm_borders() +
  tm_shape(high_ell_bus_routes) + tm_lines(col = "red", lwd = 2)

