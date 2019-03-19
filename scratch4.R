recruitment_by_tract_tidy <- 
  recruitment_data %>% 
  # st_drop_geometry() %>% 
  count(GEOID, year, registered) %>% 
  complete(GEOID, year, registered, fill = list(n = 0)) %>% 
  inner_join(nyc_tracts, ., by = c("GEOID" = "GEOID"))




registrations_by_tract_tidy <- 
  applications_by_tract_tidy %>% 
  filter(registered == T)





# tm_tiles(providers$CartoDB.PositronNoLabels) + 
#   tm_shape(nyc_sds, point.per = "feature") +
#   tm_borders(alpha = 0.7) +
#   tm_text(text = "school_dist") +
#   tm_shape(applications_by_tract_yearly_totals %>% 
#              filter(year == 2017)) + 
#   tm_borders(alpha = 0.2) +
#   tm_fill(col = "n",
#           id="tractname",
#           breaks = apps_jenks_breaks,
#           title = "2017 Apps",
#           popup.vars = c("Applications"="n",
#                          "% of Year's Applications"="pct_yr")) +
#   tm_view(set.view = c(-73.86652, 40.85263, 11),
#           set.zoom.limits = c(11,14)) + 
#   tm_layout(outer.margins = 20)    
# 
# 

library(devtools)
# devtools::install_github("r-spatial/leafsync")




recruitment_allyears <-
  recruitment_by_tract_tidy %>% 
  group_by(GEOID, NAME) %>% 
  summarise(n = sum(n)) %>% 
  mutate(apps_per_1000 = n / total_pop)

bins = getJenksBreaks(recruitment_allyears$n, 9)

pal <- colorBin("YlOrBr", domain = recruitment_allyears$n, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%s County<br/>%g application(s)",
  recruitment_allyears$NAME, 
  recruitment_allyears$n
) %>% lapply(htmltools::HTML)


manbx_basemap <- 
  leaflet() %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  setView(lng = -73.86652, lat = 40.85263, zoom = 11)



manbx_basemap %>% 
  addPolygons(data = recruitment_allyears,
              fillColor = ~pal(n),
              weight = 1,
              opacity = 0.3,
              color = "grey60",
              fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 5,
                color = "red",
                fillOpacity = 0.7,
                bringToFront = T),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              ))




manbx_basemap %>% 
  addPolygons(data = recruitment_allyears %>% 
                filter(year == 2017),
              fillColor = ~pal(n),
              weight = 1,
              opacity = 0.3,
              color = "grey60",
              fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 5,
                color = "red",
                fillOpacity = 0.7,
                bringToFront = T),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              ))




