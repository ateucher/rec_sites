library(sf)
library(reticulate)

bcdata <- import("bcdata")

dl <- bcdata$download('recreation-sites-subset-information-purposes-only', 
                      'andy.teucher@gmail.com')
rec_sites <- read_sf(dl) %>% 
  st_transform(4326)

file.remove("BC_rec_sites/rec_sites.geojson")
st_write(rec_sites, "BC_rec_sites/rec_sites.geojson")
