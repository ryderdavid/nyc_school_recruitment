tmap_mode("view")
tm_shape(nyc_bus_shelters) + tm_dots(col = "red", size = 0.001) + 
  tm_shape(nyc_bus_routes) + tm_lines()


nyc_bus_routes

