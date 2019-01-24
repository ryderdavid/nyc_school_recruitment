set_sourcefile_wd <- function() {
  library(rstudioapi) # 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

set_sourcefile_wd()

source("functions.R")

high_ell_schools <- schools_demo_snapshot %>% filter(pct_ell > quantile(.$pct_ell, 0.9))

st_buffer(high_ell_schools$pct_ell, dist = 1000)

tmap_mode("view")
tm_shape(high_ell_schools) + tm_dots(col = "red")
