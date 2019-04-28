library(rstudioapi)
library(knitr)

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
source("datasets.R")
source("functions.R")





mean(applications_by_nta$n)


st_intersection(nyc_ntas, above_avg_applications_by_nta) %>% plot()


st_intersects()

applications_by_nta <- st_join(nyc_ntas, recruitment_data, left = F) %>% count(ntacode, ntaname) %>% plot()
above_avg_applications_by_nta <- applications_by_nta %>% filter(n > mean(applications_by_nta$n))



brk_min <- min(above_avg_applications_by_nta$n)
brk_max <- max(above_avg_applications_by_nta$n)
tmap_mode("plot")
tmap_man_bx_zoom() +
  tm_shape(nyc_tracts) + tm_borders(alpha = 0.1) +
tm_shape(above_avg_applications_by_nta, point.per = "largest") +
  tm_borders() +
  
  tm_fill(col = "n", breaks = fivenum(above_avg_applications_by_nta$n)) +
  tm_text("ntacode", size = .7)


above_avg_applications_by_nta %>% top_n(5, n) -> 
