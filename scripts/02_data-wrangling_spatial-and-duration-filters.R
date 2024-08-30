## 
## Spatial and duration filters
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Library ####
library(dplyr)
library(sf)
# library(mapview) # Only used to visually check

## Read data raw ####

df_long <- read.csv("./data-processed/raw-tidy/seabird-raw-tidy-long.csv")
df_wide_groups <- read.csv("./data-processed/raw-tidy/seabird-raw-tidy-wide-groups.csv")
df_wide_species <- read.csv("./data-processed/raw-tidy/seabird-raw-tidy-wide-species.csv")

## Read spatial data ####

transects <- sf::read_sf("./data-spatial/transects/far-out_6-transects.gpkg")
# mapview::mapview(transects)

transects_segmented <- sf::read_sf("./data-spatial/transects/far-out_segmented-transects.gpkg")
# mapview::mapview(transects_segmented)

transects_segmented_buffer <- 
  sf::st_buffer(transects_segmented, endCapStyle = "FLAT", dist = 1500) %>% 
  sf::st_transform(4326)
# mapview::mapview(transects_segmented_buffer)

## Make a convex polygon around the transects (+ add a buffer),
## to create a spatial filter of the data

transects_df <- as.data.frame(transects)

transects_poly <-
  sf::st_polygon(
    list(
      cbind(
      c(transects_df[6,2], transects_df[6,4], transects_df[1,2], transects_df[1,4], transects_df[6,2]),
      c(transects_df[6,3], transects_df[6,5], transects_df[1,3], transects_df[1,5], transects_df[6,3])))) %>% 
  sf::st_sfc(crs = 4326) %>% 
  sf::st_sf()

# mapview::mapview(transects_poly) ## Good

# Add a buffer
transects_poly_buffer <- sf::st_buffer(transects_poly, dist = 3000) # 3 km buffer

# mapview::mapview(transects_poly_buffer) + transects_poly ## Good!

rm("transects_df", "transects_poly")

## Spatial filter and Spatial join with transects #### 

dfs <- list(df_long = df_long, 
            df_wide_groups = df_wide_groups, 
            df_wide_species = df_wide_species)

for(i in 1:length(dfs)){
  
  # Get name
  name <- names(dfs[i])
  
  # Get data
  sf <- 
    dfs[[i]] %>% 
    dplyr::mutate(lon2 = lon, lat2 = lat) %>% 
    sf::st_as_sf(coords = c("lon2", "lat2"), crs = 4326)
  # mapview::mapview(sf)
  
  ### Spatial filter: get only records inside the transect polygon
  sf <- 
    sf::st_intersection(sf, transects_poly_buffer)
  # mapview::mapview(sf)
  
  ### Spatial join with transects
  sf <- 
    sf::st_join(sf,
                (transects_segmented_buffer %>% 
                   dplyr::select(id_transect = id))) %>% 
    dplyr::filter(!is.na(id_transect))
  # mapview::mapview(sf)
  
  # Transform it back to 'data.frame'
  df <- sf %>% sf::st_set_geometry(NULL)
  
  # Overwrite object in the Global Environment 
  .GlobalEnv[[name]] <- df
  
  rm("i", "name", "sf", "df")
}

## Save the buffer 'polygon' and remove it from the environment
# sf::write_sf(transects_poly_buffer,
#              "./data-spatial/transects/far-out_transects-3km-buffer-polygon.gpkg")

## Save the buffer 'transect_segmented' and remove it from the environment
# sf::write_sf(transects_segmented_buffer,
#              "./data-spatial/transects/far-out_segmented-transects-buffer.gpkg")

rm("transects_poly_buffer", "transects_segmented_buffer", "transects_segmented")

## `id_count_duration` filter ####

## EDA distance and duration of counts
# boxplot(df_wide_groups$id_count_duration)
# boxplot(df_wide_groups$id_dist_km)
# plot(df_wide_groups$id_count_duration, df_wide_groups$id_dist_km)

## Use quantiles?
quantile(df_wide_groups$id_count_duration)

# The 25--75% quantiles are too narrow, so I ARBITRARILY chose ----------------#
# 7-min as minimum and 13-min  as maximum -------------------------------------#

# Overwrite 'dfs' list after spatial filter
dfs <- list(df_long = df_long, 
            df_wide_groups = df_wide_groups, 
            df_wide_species = df_wide_species)

for(i in 1:length(dfs)){
  
  # Get name
  name <- names(dfs[i])
  
  # Get data
  df <- dfs[[i]]
  
  # Filter
  df <- 
    df %>% dplyr::filter(id_count_duration >= 7 & id_count_duration <= 13)
  
  .GlobalEnv[[name]] <- df
  
  # Save it
  write.csv(df, paste0("./data-processed/", name, ".csv"), row.names = FALSE)
  
  rm("i", "name", "df")
}

rm("dfs")

## Done -- happy analysis :)