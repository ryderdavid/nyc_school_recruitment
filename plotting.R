
# trim decimals from school district numbers and set as factors
recdata <- application_points %>% 
  mutate(district = as.integer(district)) %>% 
  mutate(district = as.factor(district)) %>% 
  count(district, year, registered) %>% 
  complete(district, year, registered, fill = list(n = 0)) %>% 
  mutate(`Applicant Status` = recode(as.character(.$registered), 
                             `TRUE` = "Registered", 
                             `FALSE` = "Not Registered"))
  

















plot.applications <- 
  function(data = application_points, 
           year.min = -Inf, 
           year.max = Inf, 
           geography = "tract") {
    
    # d <- data
    d <- application_points
    
    g <- geography  # tract, district, zip, or nta
    g <- "tract"
    g <- "district"
    
    
    # Set year for filtering from arguments
    if (year.min == -Inf) {  # use minimum in data set if no min specified
      yr.min <- min(d$year) 
    } else {
      yr.min <- year.min
    }
    
    if (year.max == Inf) {  # use maximum in data set if no max specified
      yr.max <- max(d$year) 
    } else { 
      yr.max <- year.max 
    }
    
    
    d.counts <- d %>%  
      count(!!as.name(g), year, registered) %>% 
      complete(!!as.name(g), year, registered, fill = list(n = 0)) %>% 
      mutate(`Applicant Status` = recode(as.character(.$registered), 
                                         `TRUE` = "Registered", 
                                         `FALSE` = "Not Registered"))
    
    
    ggplot(d.counts) + 
      geom_bar(aes(x = district, y = n, fill = `Applicant Status`),
               stat = "identity") +
      ggtitle("Applications and Registrations, by Applicant's School District",
              subtitle = "School Years beginning 2017 through 2019") + 
      theme(plot.title    = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom") +
      xlab("School District (NA = Out of Town Application)") +
      ylab("Count") -> p1
    
    
    
  }  # close out plot.applications function







# All years
ggplot(recdata) + 
  geom_bar(aes(x = district, y = n, fill = registered),
           stat = "identity") +
  ggtitle("Applications and Registrations, by Applicant's School District",
          subtitle = "School Years beginning 2017 through 2019") + 
  theme(plot.title    = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  xlab("School District (NA = Out of Town Application)") +
  ylab("Count") -> p1





# 2017
ggplot(recdata %>% filter(year == 2017)) + 
  geom_bar(aes(x = district, y = n, fill = registered),
           stat = "identity") +
  ggtitle("Applications and Registrations, by Applicant's School District",
          subtitle = "2017-2018 School Year") + 
  theme(plot.title    = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  xlab("School District (NA = Out of Town Application)") +
  ylab("Count") -> p2


# 2018
ggplot(recdata %>% filter(year == 2018)) + 
  geom_bar(aes(x = district, y = n, fill = registered),
           stat = "identity") +
  ggtitle("Applications and Registrations, by Applicant's School District",
          subtitle = "2018-2019 School Year") + 
  theme(plot.title    = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  xlab("School District (NA = Out of Town Application)") +
  ylab("Count") -> p3


# 2019
ggplot(recdata %>% filter(year == 2019)) + 
  geom_bar(aes(x = district, y = n, fill = registered),
           stat = "identity") +
  ggtitle("Applications and Registrations, by Applicant's School District",
          subtitle = "2019-2020 School Year") + 
  theme(plot.title    = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  xlab("School District (NA = Out of Town Application)") +
  ylab("Count") -> p4




















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
















application_points %>% view

















aa %>% complete(district, year, registered, fill = list(n = 0))


nyc_sds



tm_shape(apps.2017.sd, point.per = "feature") + 
  tm_bubbles(size = "n", col = "red") + 
tm_shape(regs.2017.sd, point.per = "feature") + 
  tm_bubbles(size = "n", col = "cyan",
             size.max = max(apps.2017.sd$n))



application_points %>% complete(district, year, registered, )
