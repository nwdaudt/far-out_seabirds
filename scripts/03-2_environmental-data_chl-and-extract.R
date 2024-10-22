## 
## Environmental variables
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####

library(dplyr)
library(stringr)
library(sf)
library(raster)
library(ggplot2)

# library(mapview) ## Only used for checking spatial data

# library(terra)
# library(rerddap)
# library(rerddapXtracto)

## Read data ####

df_seabirds <- read.csv("./data-processed/df_wide_species.csv")

# df_seabirds$date_asdate <- as.Date(df_seabirds$date)
# 
# df_seabirds %>%
#   dplyr::group_by(voyage) %>%
#   dplyr::summarise(date_start = min(date_asdate),
#                    date_end = max(date_asdate))

## Read spatial data ####

nz_polygon <- sf::read_sf("./data-spatial/nz/nz-coastlines-and-islands-polygons-topo-150k.gpkg")

isobaths <- 
  sf::read_sf("./data-spatial/nz/nz_isobath_ne_north_island.gpkg") %>% 
  dplyr::filter(DEPTH %in% c(50, 200, 500, 1000)) %>% 
  sf::st_transform(4326) %>% 
  sf::st_crop(., xmin = 172.98, xmax = 173.8, ymin = -34.2, ymax = -35)
# mapview::mapview(isobaths)

# Will I need this ??
# transects_polygon <- sf::read_sf("./data-spatial/transects/far-out_transects-3km-buffer-polygon.gpkg")

sf_study_area_polygon <- 
  sf::st_read("./data-spatial/transects/far-out_transects-3km-buffer-polygon.gpkg")
# plot(sf_study_area_polygon)

df_study_area_polygon <- 
  sf_study_area_polygon[[1]][[1]][[1]] %>% 
  as.data.frame() %>% 
  dplyr::rename(longitude = V1,
                latitude = V2)
# plot(df_study_area_polygon)

## Plot ESA CCI 5-day Ocean Colour (CHL) ####

## Data source:
## Ocean Colour Climate Change Initiative dataset, v.6, European Space Agency, 
##   available online at http://www.esa-oceancolour-cci.org/

## Sathyendranath et al. (2019) *Sensors* (https://doi.org/10.3390/s19194285)

## I've downloaded the data from
## https://coastwatch.pfeg.noaa.gov/erddap/griddap/pmlEsaCCI60OceanColor5Day.graph

## Note -- the 5-day composite is centred in the image label
## i.e. days 19-20-*21*-22-23 of a month will be labelled as *yyyy-mm-21*

file_dirs <- 
  list.files(normalizePath("./data-spatial/environmental-data/chl/"), full.names = TRUE)

voyages <- c("01voyage", "02voyage", "03voyage", 
             "04voyage", "05voyage", "06voyage", 
             "07voyage", "08voyage", "09voyage", "10voyage")

dfs_r <- data.frame()

for(voyage in voyages){
  # print(voyage)
  
  voyage_name <- as.character(voyage)
  
  file_dirs_voyage <- file_dirs[grepl(pattern = voyage_name, x = file_dirs)]
  # file_dirs_voyage[1]
  
  for (i in 1:length(file_dirs_voyage)) {
    
    r <- raster::raster(file_dirs_voyage[i])
    # plot(log(r), main = paste(voyage_name, r@z))
    
    df_r <- as.data.frame(r, xy = TRUE)
    colnames(df_r) <- c("x", "y", "chl_a")
    
    df_r$raster_date <- as.character(r@z)
    df_r$voyage <- paste("Voyage", stringr::str_sub(voyage_name, end = 2))
    df_r$raster_number <- i
    
    dfs_r <- rbind(dfs_r, df_r)
    rm("i", "r", "df_r")
  }
  
  rm("voyage", "voyage_name", "file_dirs_voyage")
}

dfs_r$raster_name <- paste0(dfs_r$raster_date, " ", dfs_r$voyage, "(", dfs_r$raster_number, ")")

### ---- Regional, macro-scale

# chl_plot <-
#   ggplot() +
#   geom_raster(data = dfs_r,
#               aes(x = x, y = y, fill = log(chl_a))) +
#   scale_fill_viridis_c() +
#   geom_point(data = df_study_area_polygon,
#              aes(x = longitude, y = latitude),
#              size = 0.3) +
#   facet_wrap(~ raster_name, nrow = 3, dir = "v") +
#   # xlim(c(173, 174)) + ylim(c(-35, -34)) +
#   ylab("") + xlab("") +
#   theme_bw() +
#   theme(strip.text = element_text(size = 6))
# 
# ggsave(chl_plot,
#        filename = "./results/EDA/chl_plot.pdf",
#        height = 15, width = 35, units = "cm")

### ---- Zoomed in plot, including 500-m isobath and where the seabird counts were undertaken

## First, "match" seabird data with the 5-day composite CHL images by creating 'raster_name'/'raster_date'

df_seabirds <-
  df_seabirds %>% 
  dplyr::mutate(raster_name = dplyr::case_when(
    voyage == "01voyage" ~ "2019-11-17 Voyage 01(2)",
    voyage == "02voyage" & date %in% c("2020-01-27", "2020-01-28") ~ "2020-01-26 Voyage 02(2)",
    voyage == "02voyage" & date %in% c("2020-01-29", "2020-01-30") ~ "2020-01-31 Voyage 02(3)",
    voyage == "03voyage" & date %in% c("2021-01-10", "2021-01-11", "2021-01-12", "2021-01-13") ~ "2021-01-11 Voyage 03(2)",
    voyage == "03voyage" & date %in% c("2021-01-14", "2021-01-15") ~ "2021-01-16 Voyage 03(3)",
    voyage == "04voyage" & date %in% c("2021-07-14", "2021-07-15") ~ "2021-07-15 Voyage 04(2)",
    voyage == "05voyage" & date %in% c("2022-01-18") ~ "2022-01-16 Voyage 05(2)",
    voyage == "05voyage" & date %in% c("2022-01-19", "2022-01-20", "2022-01-21", "2022-01-22") ~ "2022-01-21 Voyage 05(3)",
    voyage == "06voyage" & date %in% c("2022-11-13", "2022-11-14") ~ "2022-11-12 Voyage 06(2)",
    voyage == "06voyage" & date %in% c("2022-11-15", "2022-11-16", "2022-11-17")  ~ "2022-11-17 Voyage 06(3)",
    voyage == "07voyage" ~ "2023-01-21 Voyage 07(2)",
    voyage == "08voyage" ~ "2023-05-26 Voyage 08(2)",
    voyage == "09voyage" & date %in% c("2023-11-15", "2023-11-16", "2023-11-17", "2023-11-18", "2023-11-19") ~ "2023-11-17 Voyage 09(2)", 
    voyage == "09voyage" & date %in% c("2023-11-20", "2023-11-21", "2023-11-22", "2023-11-23") ~ "2023-11-22 Voyage 09(3)",
    voyage == "10voyage" ~"2024-05-05 Voyage 10(2)")) %>% 
  dplyr::mutate(raster_date = stringr::str_sub(raster_name, start = 1, end = 10))

## Plot

chl_plot_zoomin <-
  ggplot() +
  geom_raster(data = dfs_r,
              aes(x = x, y = y, fill = log(chl_a))) + 
  scale_fill_viridis_c() + 
  geom_point(data = df_study_area_polygon, 
             aes(x = longitude, y = latitude),
             size = 0.3) +
  geom_sf(data = nz_polygon, color = "black", fill = "lightgrey") +
  # geom_sf(data = isobaths[isobaths$DEPTH %in% c(500),], 
  #         aes(linetype  = as.factor(DEPTH)), colour = "red", linewidth = 0.25) +
  # scale_linetype_manual(values = c("solid"), name = "Isobath") +
  geom_point(data = df_seabirds, 
             aes(x = lon, y = lat),
             size = 0.3, color = "white") +
  facet_wrap(~ raster_name, nrow = 3, dir = "v") + 
  xlim(c(172.6, 173.9)) + ylim(c(-35, -34)) +
  ylab("") + xlab("") + 
  theme_bw() + 
  theme(strip.text = element_text(size = 4.5),
        axis.text = element_text(size = 5.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.text = element_text(size = 5.5),
        legend.title = element_text(size = 5.5))

# ggsave(chl_plot_zoomin,
#        filename = "./results/EDA/chl_plot_zoom-in.pdf",
#        height = 11, width = 30, units = "cm")

# rm("chl_plot_zoomin")
# gc()

## Extract CHL data for each seabird count and create a boxplot ####

# Note: 'voyage' and 'file_dirs' were created in the previous code session
# Note 2: need to run the code "matching" seabird obs with raster name, too (L 131)

df_seabirds_chl <- data.frame()

for(voyage in voyages){
  
  # Get voyage name
  voyage_name <- as.character(voyage)
  
  # Filter .nc files from that voyage
  file_dirs_voyage <- file_dirs[grepl(pattern = voyage_name, x = file_dirs)]
  
  # Subset only the data from that voyage
  df_sample <- df_seabirds %>% dplyr::filter(voyage == voyage_name)
  
  # Get the [raster] dates for extracting CHL data, and loop through it
  raster_dates <- unique(df_sample$raster_date)
  
  for (raster_date in raster_dates) {
    
    # For each date
    r_date <- as.character(raster_date)
    
    # Subset 'sample' only from that particular [raster] date
    df_sample_date <- df_sample %>% dplyr::filter(raster_date == r_date)
    
    # Find the 'image_number' to open the right raster file
    image_number <- paste0("-", unique(stringr::str_sub(df_sample_date$raster_name, start = 22, end = 22)),"_")
    
    # Open raster
    r <- raster::raster(file_dirs_voyage[grepl(pattern = image_number, 
                                               x = file_dirs_voyage)])
    # plot(log(r), main = paste(voyage_name, r@z))
    
    # Create an 'sf' object to extract the CHL values
    sf_sample_date <- df_sample_date %>% sf::st_as_sf(coords = c("lon", "lat"))
    
    # Extract CHL
    df_sample_date <- 
      df_sample_date %>% 
      dplyr::mutate(chl_a = raster::extract(r, sf_sample_date)) %>% 
      dplyr::mutate(chl_a_log = log(chl_a))
    
    # rbind the data
    df_seabirds_chl <- rbind(df_seabirds_chl, df_sample_date)
    
    rm("raster_date", "r_date", "image_number", "r", "sf_sample_date", "df_sample_date")
  }
  
  rm("voyage", "voyage_name", "file_dirs_voyage", "df_sample", "raster_dates")
}


chl_SD <- 
  df_seabirds_chl %>% 
  dplyr::group_by(voyage) %>% 
  dplyr::summarise(chl_a_SD = round(sd(chl_a, na.rm = TRUE), digits = 2)) %>% 
  dplyr::pull(chl_a_SD) %>% 
  as.character()

## Voyage 01 had only one observation within the study area, so the SD value is 'NA'
## I'll take advantage of this to include this information in the figure
chl_SD[1] <- "SD ="

chl_violin_voyage <-
  ggplot(df_seabirds_chl,
         aes(x = voyage, y = chl_a, color = season, fill = season)) + 
  geom_point(position = "jitter", size = 0.2) +
  geom_violin(alpha = 0.5) +
  scale_fill_manual(values = c("summer" = "#4E79A7", "autumn" = "#F28E2B", 
                               "winter" = "#E15759", "spring" = "#76B7B2"),
                    name = "") +
  scale_color_manual(values = c("summer" = "#4E79A7", "autumn" = "#F28E2B", 
                               "winter" = "#E15759", "spring" = "#76B7B2"),
                    name = "") +
  xlab("") + ylab("Chlorophyll-a concentration [mg m^-3]") +
  annotate("text", x = 1:10, y = 0.7, label = chl_SD, size = 2.5) +
  theme_bw() + 
  theme(axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 8, 
                                   angle = 45, vjust = 1, hjust = 1))

ggsave(chl_violin_voyage,
       filename = "./results/EDA/chl_violin-by-voyage.pdf",
       height = 10, width = 12, units = "cm")

## Boxplot instead of violin
# chl_boxplot_voyage <-
#   ggplot(df_seabirds_chl,
#          aes(x = voyage, y = chl_a, fill = season)) + 
#   geom_boxplot() +
#   scale_fill_manual(values = c("summer" = "#4E79A7", "autumn" = "#F28E2B", 
#                                "winter" = "#E15759", "spring" = "#76B7B2"),
#                     name = "") +
#   xlab("") + ylab("Chlorophyll-a concentration [mg m^-3]") +
#   theme_bw() + 
#   theme(axis.text = element_text(size = 8),
#         axis.text.x = element_text(size = 8, 
#                                    angle = 45, vjust = 1, hjust = 1))
#
# ggsave(chl_boxplot_voyage,
#        filename = "./results/EDA/chl_boxplot-by-voyage.pdf",
#        height = 10, width = 12, units = "cm")

## Save 'df_wide_species_chl' ####

df_seabirds_chl <- df_seabirds_chl %>% dplyr::select(- c(raster_name, raster_date))

write.csv(df_seabirds_chl,
          file = "./data-processed/df_wide_species_chl.csv",
          row.names = FALSE)

## Read SST, extract and save ####

# df_seabirds_chl <- read.csv("./data-processed/df_wide_species_chl.csv")

## Data source:
## NOAA Coral Reef Watch Operational Daily Near-Real-Time Global 5-km Satellite Coral Bleaching Monitoring Products

## https://coralreefwatch.noaa.gov/satellite/docs/recommendations_crw_citation.php

## I've downloaded the data from
## https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.graph

dates_sst <- unique(df_seabirds_chl$date)

### Download the data (don't need to run again!)
# for(date_sst in dates_sst) {
#   
#   url_string <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST%5B(",
#                        as.character(date_sst), 
#                        "T12:00:00Z)%5D%5B(-32.375):(-37.325)%5D%5B(171.525):(176.175)%5D",
#                        "&.draw=surface&.vars=longitude%7Clatitude%7CCRW_SST&.colorBar=KT_thermal%7C%7C%7C15%7C23%7C&.bgColor=0xffccccff")
#   
#   download.file(url = url_string,
#                 destfile = paste0("./data-spatial/environmental-data/sst/crw-sst_", as.character(date_sst),".nc") )
#   
#   rm("date_sst", "url_string")
# }

file_dirs <- 
  list.files(normalizePath("./data-spatial/environmental-data/sst/"), full.names = TRUE)

### Extract the data
df_seabirds_chl_sst <- data.frame()

for(date_sst in dates_sst){
  
  r <- raster::raster(file_dirs[grepl(pattern = date_sst, x = file_dirs)])
  # plot(r)
  
  df_sample <- 
    dplyr::filter(df_seabirds_chl, date == as.character(date_sst)) 
  
  sf_sample <-
    sf::st_as_sf(df_sample, coords = c("lon", "lat"), crs = 4326)
  
  # Extract SST
  df_sample <- 
    df_sample %>% 
    dplyr::mutate(sst = raster::extract(r, sf_sample))
  
  df_seabirds_chl_sst <- rbind(df_seabirds_chl_sst, df_sample)
  
  rm("date_sst", "r", "df_sample", "sf_sample")
}

## Save 'df_seabirds_chl_sst' ####

write.csv(df_seabirds_chl_sst,
          file = "./data-processed/df_wide_species_chl_sst.csv",
          row.names = FALSE)
