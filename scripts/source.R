##
## Helper functions for 'far-out_seabirds' Project
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##


## Create line segments from a matrix/df ------------------------------------ ##
# Function adapted from Spacedman's answer in StackExchange GIS 
# https://gis.stackexchange.com/questions/312289/r-create-multiple-linestrings-from-multiple-coordinates

FUN_create_line_segments <- function(df){
  
  ## df = data.frame(lon,lat,lon2,lat2)
  
  require(sf)
  
  st_segment <- function(df){sf::st_linestring(t(matrix(unlist(df), 2, 2)))}
  
  df$geom <- 
    sf::st_sfc(sapply(
      1:nrow(df), 
      function(i){st_segment(df[i,])}, simplify = FALSE), 
      crs = 4326)
  
  df <- sf::st_sf(cbind("ID" = 1:nrow(df), df))
  
  # Note: the function returns a spatial feature object
  return(df)
}

## Split linestrings into segments ------------------------------------------ ##

FUN_split_linestrings <- function(df, n, dist, crs) {
  
  ## df = data.frame
  ## n = number of points to be used as 'blades' to split the line feature
  ## dist = buffer distance 
  ## crs = a *metric* reference system
  
  require(sf)
  require(lwgeom)
  require(dplyr)
  
  df_splited <- data.frame()
  
  y <- 1 / (n)
  pts <- seq(from = y, to = 1, by = y)
  
  for (i in 1:nrow(df)) {
    
    x <- df[i,]
    
    x <- sf::st_transform(x, crs = crs)
    
    blade_pts <- sf::st_buffer(
      sf::st_line_sample(x, sample = pts),
      dist = dist)
    
    line_split <- lwgeom::st_split(x, blade_pts)
    
    df_tmp <- sf::st_collection_extract(line_split, "LINESTRING")
    
    df_tmp <- 
      df_tmp %>% 
      dplyr::mutate(len = as.numeric(sf::st_length(.))) %>%
      dplyr::filter(len > 6) %>% 
      dplyr::mutate(id = 1:nrow(.))
    
    df_splited <- rbind(df_splited, df_tmp)
  }
  
  # Note: the function returns a spatial feature object
  return(df_splited)
}

