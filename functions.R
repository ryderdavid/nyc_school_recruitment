
# install.packages("devtools")
# install.packages("spdplyr")
# install.packages("tidycensus")
# install.packages("acs")
# install.packages("rstudioapi")


check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# need local package gdal, units (udunits on arch), v8-3.14 (source on AUR), gcc-fortran, unixodbc

packages <- c('sf', 'devtools', 'acs', 'tidycensus', 'tidyverse', 'tigris', 'sp', 
              'tmap', 'tmaptools', 'readxl', 'ggplot2', 'rgdal', 'spdplyr', 'RColorBrewer', 
              'viridis', 'viridisLite', 'rstudioapi', 'raster')

check.packages(packages)

# 
# library(rJava)
# library(devtools)
# library(acs)
# library(tidycensus)
# library(tidyverse)
# library(tigris)
# library(sp)
# library(tmap)
# library(readxl)
# library(ggplot2)
# library(rgdal)
# library(spdplyr)
# library(RColorBrewer)
# library(viridis)
# library(viridisLite)
# library(tmaptools)
# library(RSocrata)
# library(grid)
# library(gridExtra)
# # to create grid side by side layouts of tmap plots per
# # https://stackoverflow.com/questions/34344454/plot-2-tmap-objects-side-by-side
# library(grid)

source("datasets.R")

set_sourcefile_wd <- function() {
  library(rstudioapi) # 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

set_sourcefile_wd()


## MAN_BX BACKGROUND MAP AS FUNCTION EVERY TIME
tmap_nyc_background <- function() {
  return(
    tm_shape(nyc_area_zips,
             xlim = c(-74.259104, -73.687704),
             ylim = c(40.490671, 40.924773)) + tm_fill(col = "grey90")
  )
}

tmap_man_bx_background <- function() {
  return(
    tm_shape(nyc_area_zips, 
             ylim = c(40.66, 40.92),
             xlim = c(-74.03, -73.76)) +
      tm_fill(col = "grey90")
  )
}

tmap_man_bx_zoom <- function() {
  return(
    tm_shape(nyc_area_zips, 
             ylim = c(40.77, 40.918),
             xlim = c(-73.98, -73.78)) +
      tm_fill(col = "grey90")
  )
}


# Turned plotting by registration year into a function
plot_reg_by_year <- function(yr, rec_data = recruitment_data, p = "Oranges") {
  
  reg.by.zip <- rec_data %>% 
    dplyr::select(application_id, zip, lat, long, year, registration_completed_date) %>% 
    mutate(registered = ifelse(!is.na(registration_completed_date), 1, 0)) %>% 
    filter(registered == 1) %>% count(zip, year) %>% 
    transmute(zip = zip, year = year, registrations = n)
  
  # Plot selected year registrations
  reg.filtered.by.year <- filter(reg.by.zip, year == as.numeric(yr))
  # add spatial data
  reg.filtered.by.year <- merge(man_bx_zips, reg.filtered.by.year, 
                        by.x = "GEOID10", by.y = "zip")
  
  regplot.yr <- tmap_man_bx_zoom() +
    tm_shape(reg.filtered.by.year) + 
    tm_borders(lw = 1.5, alpha = .3) + 
    tm_fill(col = "registrations", 
            title = "Registrations", 
            colorNA = NULL, 
            palette = p, 
            style = "pretty") +
    tm_text(text = "GEOID10", 
            size = "registrations", 
            style = "pretty", 
            size.lim = c(10, 20),
            shadow = T, 
            legend.size.show = F, 
            fontface = "bold") +
    tm_layout(main.title = paste("Distribution of", 
                                 format(sum(reg.filtered.by.year$registrations, 
                                            na.rm = T), big.mark = ","),
                                 paste("registrations in Manhattan",
                                       "and the Bronx,\n",
                                       as.character(yr),
                                       "School Year,",
                                       "by Zip Code Tabulation Area")),
              main.title.position = "center",
              legend.position = c("left", "top"),
              main.title.size = 1.2)
  
  return(regplot.yr)
  
}




### Question: which school district are most applications coming from?
### note: http://www.guru-gis.net/count-points-in-polygons/

plot_sd_choropleth <- function(year = seq(1950,2050), palette = "YlOrBr", regonly = F) {
  
  yr <- year
  p <-  palette
  regonly <- regonly
  
  # Controls here for bugtesting within the function
  # yr <- 2018
  # p <- "Greens"
  # regonly <- T
  
  points <- recruitment_data %>% 
    dplyr::select(application_id, year, long, lat, 
                  registration_completed_date, submission_date, year) %>% 
    mutate(registered = ifelse(!is.na(registration_completed_date), 1, 0)) %>% 
    dplyr::select(-registration_completed_date)
  
  # filter points by passed year
  points <- points %>% filter(year %in% yr)
  
  
  coordinates(points) <- ~long + lat # move coords into SPDF slot
  proj4string(points) <- CRS(NYC_CRS) # set common projection
  
  if(regonly == T) {
    points <- points %>% filter(registered == 1)
  }
  
  # create a label that changes format for map plotting based on years selected
  if(length(yr) == 1) {
    yrlabel <- paste(as.character(yr), " School Year", sep = "")
  } else {
    yrlabel <- paste(min(points$year), "-", max(points$year), " School Years", sep = "")
  }
  
  # requires nyc sds fixed, check how many points plot into nyc sds polys
  res <- over(points, nyc_sds)
  
  # tabulate points per school district
  points_per_sd <- as.tibble(table(res$SchoolDist)) %>% 
    rename(SchoolDist = Var1) %>% 
    mutate(SchoolDist = as.integer(SchoolDist))
  
  # to the SPDF of school districts, add the number of points taking place
  # in each SD
  points_per_sd_spdf <- merge(nyc_sds, points_per_sd,
                                    by.x = "SchoolDist", by.y = "SchoolDist")
  
  
  # create a pretty label variable combining SD number and number of applications
  points_per_sd_spdf %<>% 
    mutate(label = paste("SD", as.character(SchoolDist), ":\n", 
                         as.character(n), sep = "")) %>% filter(!is.na(n))
  
  points_per_sd_spdf
  
  # applications or registrations changes title and legend name
  if(regonly == T) {
    ar <- "registrations"
    ar_title <- "Registrations"
  } else {
    ar <- "applications"
    ar_title <- "Applications"
  }
  
  # plot points per SD
  tmap_mode("plot")
  
  tmap_man_bx_zoom() +
    tm_shape(points_per_sd_spdf) +
    tm_borders(alpha = 0.3, lw = 1.5) + 
    tm_fill(col = "n", title = ar, colorNA = NULL, palette = p,
            showNA = F) + 
    tm_text(text = "label", fontface = "bold", style = "pretty", 
            showNA = F) + 
    tm_layout(main.title = paste(ar_title, "per school district,", "\n", yrlabel), 
              main.title.position = ("center"), fontface = "bold", legend.position = c("left", "top"))
  
}


### Question: where are candidate students best converting from applications to registrations?
### note: http://www.guru-gis.net/count-points-in-polygons/

plot_conversions_by_sd <- function(year = seq(1950,2050), palette = "Greys", data = recruitment_data) {
  
  yr <- year
  p <-  palette
  # 
  # yr <- 2019
  # p <- "BuGn"
  # data = recruitment_data
  
  
  app_points <- data %>% 
    dplyr::select(application_id, year, long, lat, 
                  registration_completed_date, submission_date, year) %>% 
    mutate(registered = ifelse(!is.na(registration_completed_date), 1, 0)) %>% 
    dplyr::select(-registration_completed_date)
  
  # filter points by passed year
  app_points <- app_points %>% filter(year %in% yr)
  
  coordinates(app_points) <- ~long + lat # move coords into SPDF slot
  proj4string(app_points) <- CRS(NYC_CRS) # set common projection
  
  
  # create a reg_points SPDF that only contains successful registrations
  reg_points <- app_points %>% filter(registered == 1)
  
  reg_points
  app_points
  
  # coordinates(reg_points) <- ~long + lat # move coords into SPDF slot
  proj4string(reg_points) <- CRS(NYC_CRS) # set common projection
  
  # calc how many applications and registrations plot into nyc sds polys. Fun
  # over() introduces NAs where points plot outside the scope of the NYC sds I
  # have trimmed the plot to (say if someone has applied from Jersey or out of
  # state), so after checking which poly each point plots over, I filter out any
  # that have NAs (aka where they are outside the scope of the query.)
  apps_over_sds <- over(app_points, nyc_sds) %>% filter_all(any_vars(!is.na(.)))
  regs_over_sds <- over(reg_points, nyc_sds) %>% filter_all(any_vars(!is.na(.)))
  
  
  # tabulate applications and registrations per school district
  total_apps_per_sd <- apps_over_sds %>% count(SchoolDist) %>% rename(applications=n)
  total_regs_per_sd <- regs_over_sds %>% count(SchoolDist) %>% rename(registrations=n)
  
  
  # merge both attributes and get apps -> regs conversion rate
  conversions_per_sd <- merge(total_apps_per_sd, 
                              total_regs_per_sd, by = "SchoolDist") %>% 
    mutate(rate = registrations / applications)
  
  # merge conversions with spatial data on school districts, and remove SDs
  # where no applications or registrations were plotted
  conversions_per_sd_spdf <-  merge(nyc_sds, conversions_per_sd,
                                    by.x = "SchoolDist", 
                                    by.y = "SchoolDist") %>% 
    filter(!is.na(applications) & !is.na(registrations) & !is.na(rate))
  
  conversions_per_sd_long <- conversions_per_sd_spdf@data %>% gather(key="type", value="count", "applications", "registrations")
  
  # prep two special variables for plot presentation
  clr <- brewer.pal(5, p)[4] # sets the color of registrations
  yrlabel <- ifelse(length(yr) == 1, as.character(yr), paste(min(yr), "-", max(yr), sep = ""))
  
  app_to_reg_plot <- ggplot(data = conversions_per_sd_long, 
                            mapping = aes(x = as.factor(SchoolDist), y = count, fill = type)) + 
    geom_bar(stat = "identity") + 
    theme(axis.title.x = element_blank()) + 
    coord_flip() + 
    scale_fill_manual(values = c("grey70", clr)) + 
    theme(legend.position = "bottom",
          legend.spacing.x = unit(0.25, 'cm'),
          legend.title = element_blank()) + 
    ggtitle(paste('Applications and Registrations by School District, ', yrlabel, sep = "")) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    xlab("School District")
  
  return(app_to_reg_plot)
  
}  







plot_app_to_reg_by_year <- function(year = seq(1950,2050), palette = "Set1", data = recruitment_data, top.n = 10) {
  
  yr <- year
  p <-  palette

  
  app_points <- data %>% 
    dplyr::select(application_id, year, long, lat, 
                  registration_completed_date, submission_date, year) %>% 
    mutate(registered = ifelse(!is.na(registration_completed_date), 1, 0)) %>% 
    dplyr::select(-registration_completed_date)
  
  # filter points by passed year
  app_points <- app_points %>% filter(year %in% yr)
  
  coordinates(app_points) <- ~long + lat # move coords into SPDF slot
  proj4string(app_points) <- CRS(NYC_CRS) # set common projection
  
  app_points
  
  # create a reg_points SPDF that only contains successful registrations
  reg_points <- app_points %>% filter(registered == 1)
  
  reg_points
  app_points
  
  # coordinates(reg_points) <- ~long + lat # move coords into SPDF slot
  proj4string(reg_points) <- CRS(NYC_CRS) # set common projection
  
  # calc how many applications and registrations plot into nyc sds polys. Fun
  # raster::intersect() introduces NAs where points plot outside the scope of the NYC sds I
  # have trimmed the plot to (say if someone has applied from Jersey or out of
  # state), so after checking which poly each point plots over, I filter out any
  # that have NAs (aka where they are outside the scope of the query.)
  #
  # NOTE: THIS IS WIP FUNCTION - I STILL DON'T HAVE IT DOING WHAT I WANT IT TO. 
  apps_over_sds <- intersect(app_points, nyc_sds) %>% filter_all(any_vars(!is.na(.)))
  regs_over_sds <- intersect(reg_points, nyc_sds) %>% filter_all(any_vars(!is.na(.)))
  
  apps_over_sds@data %>% group_by(SchoolDist, registered, year) %>% 
    summarise(count = n()) -> sd_stats
  
  sd_stats$year <- as.factor(sd_stats$year)
  levels(sd_stats$year)
  
  
  sd_stats %>% 
    group_by(SchoolDist) %>% 
    summarise(count = sum(count)) %>% 
    top_n(top.n, count) %>% 
    pull(SchoolDist) -> top_dists

  
  sd_stats_top <- sd_stats %>% filter(SchoolDist %in% top_dists)
  
  data <- sd_stats_top
  
  yrlabel <- ifelse(length(yr) == 1, as.character(yr), paste(min(yr), "-", max(yr), sep = ""))
  
  # This plots color of each 
  ggplot(data, aes(x = as.factor(SchoolDist), y = count)) + 
    geom_bar(stat = "identity", fill = "grey70") + 
    theme(axis.title.x = element_blank()) + 
    coord_flip() +
    # scale_color_brewer(palette = "Greys") +
    geom_bar(data = filter(data, registered == 1), 
             aes(fill = year), position = position_stack(reverse = T), stat = "identity") +
    scale_fill_brewer(palette = p) + 
    theme(legend.position = "bottom",
          legend.spacing.x = unit(0.25, 'cm'),
          legend.title = element_blank()) + 
    ggtitle(paste("Recruitment activity in top ", top.n, " recruiting school districts, ", yrlabel, 
                  "\n(total applications for all years shown in grey)", sep = "")) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    xlab("School District")
  
}  






