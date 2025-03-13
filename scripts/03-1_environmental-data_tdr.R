## 
## TDR data
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##


## Libraries ####
library(dplyr)
library(ggplot2)
library(interp)
library(ggrepel)
library(sf)

## Read data ####

files <- dir(path = "./data-raw/TDR/", pattern = "*.csv", ignore.case = TRUE, full.names = TRUE)

data <- data.frame()

for (i in 1:length(files)) {
  
  tmp <- read.csv(files[i], skip = 11)
  
  colnames(tmp) <- c("x", "date_time_utc", "latitude", "longitude", "temperature_deg_c", "depth_m", "qc_flag")
  
  tmp <- 
    tmp %>% 
    dplyr::mutate(id_profile = i, .before = everything()) %>% 
    dplyr::mutate(latitude = mean(latitude),
                  longitude = mean(longitude))
  
  data <- rbind(data, tmp)
  
  rm("tmp", "i")
}

# length(unique(data$id_profile)) # Check -- all good

rm("files")

## Spatial filter ####

study_area_polygon <- sf::read_sf("./data-spatial/transects/far-out_transects-3km-buffer-polygon.gpkg")
# plot(study_area_polygon)

data_sf <- 
  data %>% 
  dplyr::mutate(lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

## Get 'ids' that overlap with study area polygon
ids <- sf::st_contains(study_area_polygon, data_sf, sparse = FALSE) %>% t()

## Filter them from the 
data <- 
  data %>% dplyr::filter(ids) # returns a 'warning' but it works

# unique(data_test$id_profile) # Check -- all good

rm("ids", "data_sf", "study_area_polygon")

## Specify in which voyage each profile was ####

data <- 
  data %>% 
  dplyr::mutate(date = as.Date(date_time_utc))

data <- 
  data %>%
  dplyr::mutate(voyage = dplyr::case_when(
    date <= "2019-11-17" ~ "Voyage 01 (Spring)",
    date >= "2020-01-27" & date <= "2020-02-05" ~ "Voyage 02 (Summer)",
    date >= "2021-01-10" & date <= "2021-01-23" ~ "Voyage 03 (Summer)",
    date >= "2021-07-14" & date <= "2021-07-15" ~ "Voyage 04 (Winter)", 
    date >= "2022-01-18" & date <= "2022-01-26" ~ "Voyage 05 (Summer)", 
    date >= "2022-11-12" & date <= "2022-11-20" ~ "Voyage 06 (Spring)", 
    date >= "2023-01-20" & date <= "2023-01-24" ~ "Voyage 07 (Summer)",
    date >= "2023-05-25" & date <= "2023-05-26" ~ "Voyage 08 (Autumn)", 
    date >= "2023-11-14" & date <= "2023-11-25" ~ "Voyage 09 (Spring)",
    date >= "2024-05-04" & date <= "2024-05-06" ~ "Voyage 10 (Autumn)"), 
    .before = everything())

# unique(data$voyage)
## >> Profiles available from Voyage 06 onwards

data <- 
  data %>% 
  # SV Manawanui could have been surveying the region in other voyages
  # which not necessarily match our survey dates -- so remove those profiles
  dplyr::filter(! is.na(voyage)) %>% 
  # Also, remove data-points with bad quality (`qc_flag` 3 and 4)
  dplyr::filter(qc_flag < 3) %>% 
  dplyr::group_by(voyage, id_profile) %>% 
  dplyr::mutate(id_profile_continuous = dplyr::cur_group_id(), .after = id_profile) %>% 
  dplyr::ungroup(.)

## Filter away Voyage 8 id_profile 61 -- there are only a few values, which is clearly wrong
data <- 
  data %>% 
  dplyr::filter(! c(voyage == "Voyage 08 (Autumn)" & id_profile == 61))

## Plot the TDR profiles #### 

## Pseudo-code
# - for each voyage
# - interpolate TDR values
# - matrix > df > reshape
# -- for each profile
# -- find MLD
# -- save in 'mld_plot'
# - save interpolated profiles in 'tdr_interp_plot'

voyages <- unique(data$voyage)

tdr_interp_plot <- data.frame()
mld_plot <- data.frame()

for (voyage in voyages) {
  # print(voyage)
  voyage_chr <- as.character(voyage)
  
  tmp <- data[data$voyage == voyage_chr, ]
  
  ## Interpolate values
  interp_tmp <- 
    interp::interp(x = tmp$id_profile, 
                   y = tmp$depth_m, 
                   z = tmp$temperature_deg_c, 
                   duplicate = "mean",
                   nx = length(unique(tmp$id_profile)),
                   ny = round(max(tmp$depth_m), digits = 0))
  
  # Get the interpolated matrix
  dt_tmp <- 
    t(data.frame(interp_tmp$z)[, -1]) %>% 
    as.data.frame()
  
  # Set colnames to be the number of profiles
  colnames(dt_tmp) <- 1:ncol(dt_tmp)
  # Create 'depth', based on the number of values used in the interpolation
  dt_tmp$depth <- seq(0, max(tmp$depth_m), length.out = nrow(dt_tmp))
  
  dt_long <-
    dt_tmp %>% 
    tidyr::pivot_longer(cols = !depth,
                        names_to = "profile",
                        values_to = "value") %>% 
    dplyr::mutate(profile = as.numeric(profile)) %>% 
    dplyr::mutate(voyage = voyage_chr, .before = everything())
  
  ## ----- Find MLD
  # By definition, MLD is the depth at which the temperature changes -0.2 
  # compared to the temperature at 10m depth 
  
  profiles <- unique(dt_long$profile)
  
  for(profile in profiles){
    # print(profile)
    profile_chr <- as.character(profile)
    
    tmp_profile <- dt_long %>% dplyr::filter(profile == as.numeric(profile_chr))
    
    tmp_profile <- tmp_profile[3:nrow(tmp_profile), ] ## to remove any (possible) noise in the first couple of measurements
    
    # Find the closest value of 'depth' to '10'
    depth_index <- which(abs(tmp_profile$depth - 10) == min(abs(tmp_profile$depth - 10)))
    # ...and find its temperature
    temp_at_10m_value <- tmp_profile[depth_index, ]$value
    
    # Then, find the temp_at_10m_value minus 0.2, and the index of its depth 
    temp_minus02_index <- 
      which(abs(tmp_profile$value - (temp_at_10m_value - 0.2)) == min(abs(tmp_profile$value - (temp_at_10m_value - 0.2)), na.rm = TRUE))
    # ...and its depth
    depth_MLD <- tmp_profile[temp_minus02_index, ]$depth
    
    mld_plot <- rbind(mld_plot,
                      (data.frame(voyage = voyage_chr,
                                  profile = profile_chr,
                                  depth_mld = depth_MLD)))
  }
  
  tdr_interp_plot <- rbind(tdr_interp_plot, dt_long)
  
  rm("voyage", "voyage_chr", "tmp", "interp_tmp", "dt_tmp", "dt_long", 
     "profile", "profiles", "profile_chr", "tmp_profile", 
     "depth_index", "temp_at_10m_value", "temp_minus02_index", "depth_MLD")
}
rm("voyages")

## Plot TDR profiles based on the interpolated dataset
tdr_profiles <-
  ggplot(tdr_interp_plot, 
         aes(x = profile, y = depth*-1)) + 
  geom_raster(aes(fill = value)) + 
  scale_fill_viridis_c(option = "inferno", "Temperature (°C)", 
                       na.value = "grey90") +
  stat_contour(aes(z = value), binwidth = 0.5, 
               color="white", linewidth = 0.35) +
  geom_line(data = mld_plot, 
            aes(x = as.numeric(profile), y = depth_mld*-1, group = voyage), 
            color = "cyan", linewidth = 1) +
  facet_wrap(~voyage, scales = "free_x") + 
  xlab("TDR profile") + ylab("Depth (m)") + 
  theme_bw() +
  theme(legend.position = c(0.8, 0.3),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        # axis.text.x = element_blank(),
        strip.text = element_text(size = 10, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# ggsave(tdr_profiles,
#        filename = "./results/TDR-profiles.pdf",
#        height = 14, width = 20, units = "cm", dpi = 300)

rm("tdr_profiles")

## -------------------------------------------------------------------------- ##
## Add the 'id_profile_continuous' from `data` to the interpolated dataset,
## so that, if needed, one can relate one data.frame to another
tmp <- 
  tdr_interp_plot %>% 
  dplyr::arrange(voyage, profile) %>% 
  dplyr::distinct(voyage, profile) %>% 
  dplyr::mutate(id_profile_continuous = c(1:30, 32:72))

tdr_interp_plot <-
  dplyr::left_join(tdr_interp_plot, tmp, by = c("voyage", "profile"))

rm("tmp")

## Save it
# write.csv(tdr_interp_plot,
#           file = "./data-processed/tdr_tidy-2_interpolated.csv",
#           row.names = FALSE)

## Get temperature at the surface (0-10m depth) [sst], merge [mld] & save it ####

tdr_sst <- 
  data %>% 
  dplyr::filter(depth_m <= 10) %>% 
  dplyr::group_by(voyage, id_profile, id_profile_continuous) %>% 
  dplyr::summarise(tdr_sst = mean(temperature_deg_c, na.rm = TRUE))

# boxplot(tdr_sst$tdr_sst ~ tdr_sst$voyage,
#         ylab = "Sea surface temperature", xlab = "")

## Add the 'id_profile_continuous' from `data` to the `mld_plot`,
## and join it with `tdr_sst`
tmp <- 
  mld_plot %>% 
  dplyr::mutate(profile = as.numeric(profile)) %>%
  dplyr::arrange(voyage, profile) %>% 
  dplyr::mutate(id_profile_continuous = c(1:30, 32:72)) %>% 
  dplyr::select(- profile)

tdr_sst_mld <-
  dplyr::left_join(tdr_sst, tmp, by = c("voyage", "id_profile_continuous")) %>% 
  ungroup(.)

rm("tmp", "tdr_sst", "mld_plot")

## Then, join 'sst' and 'mld' to the main `data` and save it
data <-
  dplyr::left_join(data, 
                   (tdr_sst_mld %>% 
                      dplyr::select(- c(voyage, id_profile))),
                   by = c("id_profile_continuous"))

## Save it
# write.csv(data,
#           file = "./data-processed/tdr_tidy-1_raw-with-sst-mld.csv",
#           row.names = FALSE)

## Plot (spatially) where the TDR profiles where taken ####

nz_polygon <- sf::read_sf("./data-spatial/nz/nz-coastlines-and-islands-polygons-topo-150k.gpkg")
study_area_polygon <- sf::read_sf("./data-spatial/transects/far-out_transects-3km-buffer-polygon.gpkg")

nz_base_map_simplified <- 
  ggplot(data = nz_polygon) + 
  geom_sf(color = "black", fill = "lightgrey") + 
  coord_sf(xlim = c(172.6, 173.8), ylim = c(-35, -34.2)) + 
  theme_bw()

data_id_profiles <- 
  data %>% dplyr::distinct(id_profile_continuous, .keep_all = TRUE)

map_profile_locations <-
  nz_base_map_simplified + 
  geom_point(data = data_id_profiles,
             aes(x = longitude, y = latitude),
             size = 2, shape = 21, fill = "red", alpha = 0.7) + 
  geom_label_repel(data = data_id_profiles,
                   aes(x = longitude, y = latitude, label = id_profile_continuous),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   force = 2,
                   segment.color = 'grey50',
                   max.overlaps = 20) + 
  geom_sf(data = study_area_polygon, fill = NA) + 
  coord_sf(xlim = c(172.6, 174), ylim = c(-35, -34)) + 
  facet_wrap(~ voyage) + 
  xlab("") + ylab("") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

map_profile_locations_sst <-
  nz_base_map_simplified + 
  geom_point(data = data_id_profiles,
             aes(x = longitude, y = latitude, fill = tdr_sst),
             size = 2, shape = 21) + 
  scale_fill_viridis_c(option = "inferno", "Temperature (°C)") +
  geom_sf(data = study_area_polygon, fill = NA) + 
  coord_sf(xlim = c(172.6, 174), ylim = c(-35, -34)) + 
  facet_wrap(~ voyage) + 
  xlab("") + ylab("") + 
  theme(legend.position = c(0.85, 0.21),
      legend.text = element_text(size = 12, colour = "black"),
      legend.title = element_text(size = 12, colour = "black"),
      axis.title.y = element_text(size = 12, colour = "black"),
      axis.text.y = element_text(size = 10, colour = "black"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      strip.text = element_text(size = 10, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank())

# ggsave(map_profile_locations_sst,
#        filename = "./results/TDR-profiles_map-SST.pdf",
#        height = 14, width = 20, units = "cm", dpi = 300)

map_profile_locations_mld <-
  nz_base_map_simplified + 
  geom_point(data = data_id_profiles,
             aes(x = longitude, y = latitude, fill = (depth_mld)*-1),
             size = 2, shape = 21) + 
  scale_fill_viridis_c(option = "mako", "Mixed layer depth (m)") +
  geom_sf(data = study_area_polygon, fill = NA) + 
  coord_sf(xlim = c(172.6, 174), ylim = c(-35, -34)) + 
  facet_wrap(~ voyage) + 
  xlab("") + ylab("") + 
  theme(legend.position = c(0.85, 0.21),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 10, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# ggsave(map_profile_locations_mld,
#        filename = "./results/TDR-profiles_map-MLD.pdf",
#        height = 14, width = 20, units = "cm", dpi = 300)

rm("nz_polygon", "study_area_polygon", "nz_base_map_simplified", 
   "data_id_profiles", "map_profile_locations_sst", "map_profile_locations_mld")
