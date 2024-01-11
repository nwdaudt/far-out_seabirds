## 
## Create transect lines
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries & source ####
source("./scripts/source.R")

library(dplyr)
library(sf)
# library(mapview) # only used to visually verify

## Transect's start/end longitude and latitude ####

sf_FarOut_transects <- data.frame(
  lon = c(173.567, 173.674, 173.413, 173.520, 173.258, 173.378),
  lat = c(-34.643, -34.399, -34.554, -34.305, -34.461, -34.214),
  lon2 = c(173.674, 173.413, 173.520, 173.258, 173.378, 173.113),
  lat2 = c(-34.399, -34.554, -34.305, -34.461, -34.214, -34.362)
)

## Create sf linestrings (transects) ####
sf_FarOut_transects <- FUN_create_line_segments(sf_FarOut_transects)

# mapview::mapview(sf_FarOut_transects, zcol = "ID")

## Save it
# sf::write_sf(sf_FarOut_transects, 
#              "./data-spatial/transects/far-out_6-transects.gpkg")

## Split linestrings into equal parts ####

sf_FarOut_transects <- 
  FUN_split_linestrings(df = sf_FarOut_transects,
                        n = 2, 
                        dist = 3, # due to geometries approximation, there is usually a gap of a few meters
                        crs = 2193) # NZ Transverse Mercator

sf_FarOut_transects <- 
  sf_FarOut_transects %>%
  dplyr::rename(transect = ID,
                sub_transect = id) %>%
  # Swap the order "1 <> 3" ("inshore / offshore")
  dplyr::mutate(sub_transect = ifelse((transect %% 2 == 0) & sub_transect == 1, 
                                      yes = 3, no = 
                                        ifelse((transect %% 2 == 0) & sub_transect == 3, 
                                               yes = 1, no = sub_transect))) %>%
  dplyr::mutate(id = paste0(as.character(transect), ".", as.character(sub_transect))) %>%
  dplyr::mutate(id = as.numeric(id)) %>%
  dplyr::arrange(id) %>%
  dplyr::select(-c(lon, lat, lon2, lat2)) %>%
  dplyr::select(id, transect, sub_transect, len)

# mapview::mapview(sf_FarOut_transects, zcol = "transect")
# mapview::mapview(sf_FarOut_transects, zcol = "sub_transect")
# mapview::mapview(sf_FarOut_transects, zcol = "id")

## Save it

# sf::write_sf(sf_FarOut_transects, 
#              "./data-spatial/transects/far-out_segmented-transects.gpkg")
