
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








# Merge shapefile with application data tabulation of zip codes, removing from
# shapefile any ZCTA polygon with "NA" -- AKA 0 applications
man_bx_merge <- merge(man_bx_zips, 
                      recruitment_data_man_bx, 
                      by.x = "GEOID10", 
                      by.y = "zip") %>% filter(!is.na(n))



recruitment_data_man_bx

# Create a single point shapefile for Samuel Gompers HS
sam_gompers_hs <- data.frame(name = "Samuel Gompers HS", lat = 40.811227, long = -73.907361)
coordinates(sam_gompers_hs) <- ~long + lat
projection(sam_gompers_hs) = as.character(proj4string(man_bx_merge)) # update this to method used elsewhere
# dir.create("sghs_coords")
# shapefile(sam_gompers_hs, "sghs_coords/sghs_coords.shp")




## Plot Applications by Zip Code:
##### TURN INTO FUNCTION I CAN CALL
tm_shape(nyc_area_zips, ylim = c(40.681061, 40.930), 
         xlim = c(-74.041447, -73.78)) + 
  tm_fill(col = "grey90") +
  
  tm_shape(man_bx_zips) + tm_fill(col = "grey90") + tm_layout(bg.color = "grey75") +
  tm_shape(man_bx_merge) + 
  tm_borders(lw = 1.5, alpha = .2) + 
  tm_fill(col = "n", title = "Applications", palette = "YlOrBr", colorNA = NULL) + 
  tm_text(text = "GEOID10", size = "n", style = "pretty", size.lim = c(100, 400),
          shadow = T, legend.size.show = F, fontface = "bold") +
  tm_layout(main.title = paste("Distribution of", 
                               format(sum(recruitment_data_man_bx$n), big.mark = ","),
                               paste("applications in Manhattan",
                                     "and the Bronx,\n2017-2019 School Years, by Zip Code Tabulation Area")),
            main.title.position = "center",
            legend.position = c("left", "top"),
            main.title.size = 1.2) + 
  tm_credits("Source: HUM II Recruitment Data, 2017-2019", 
             position = c("right", "bottom")) + 
  tm_shape(sam_gompers_hs) + tm_symbols(size = .5, col = "#226bf8", style = "pretty", 
                                        border.lwd = 1.5, border.col = "white", shape = 23)



