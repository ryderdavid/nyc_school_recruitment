
# # Bus stops - might not be the right viz
# nyc_stops <- readOGR(dsn = "nyc_bus_stops_may2018", layer = "bus_stops_nyc_may2018")
# # man_bx_stops <- nyc_stops@data[nyc_stops$NAMELSAD == "Bronx County" | nyc_stops$NAMELSAD == "New York County", ]
# 
# nyc_stops %>% filter(NAMELSAD == "Bronx County" | NAMELSAD == "New York County") -> man_bx_stops
# 
# tm_shape(man_bx_ntas) + tm_borders() + tm_shape(nyc_stops) + tm_dots(col = "red")
# 
# # Bus routes
# dir()
# readOGR(dsn = "bus_routes_nyc_nov2018", layer = "bus_routes_nyc_nov2018") -> bus_routes_nyc
# 
# tm_shape(bus_routes_nyc) + tm_lines()