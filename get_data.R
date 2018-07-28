library(sf)
library(httr)

layer <- "WHSE_FOREST_TENURE.FTEN_REC_SITE_POINTS_SVW"
base_url <- sprintf("https://openmaps.gov.bc.ca/geo/pub/%s/ows", layer)
response <- httr::GET(base_url,
                      query = list(service = "WFS", 
                                   version = "2.0.0", 
                                   request = "GetFeature", 
                                   typeName = layer, 
                                   outputFormat = "json", 
                                   SRSNAME = "epsg:4326"))
stop_for_status(response)

rec_sites <- read_sf(content(response, as = "text"))
saveRDS(rec_sites, "rec_sites.rds")
