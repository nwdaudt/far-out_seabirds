## 
## Data summary
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##


## Libraries ####

library(dplyr)
library(tidyr)
library(forcats)
library(sf)
library(ggplot2)
library(ggspatial)
library(patchwork)
# library(mapview) ## Only to check spatial objects

## Four-season colour palette
# palette.colors(palette = "Tableau 10")[1:4]
# "summer" = "#4E79A7"
# "autumn" = "#F28E2B"
# "winter" = "#E15759"
# "spring" = "#76B7B2"

## Read seabird data raw ####

# df_long <- read.csv("./data-processed/df_long.csv") # Just in case

df_wide_groups <- 
  read.csv("./data-processed/df_wide_groups.csv") %>% 
  dplyr::mutate(season = factor(season, 
                                levels = c("summer", "autumn", "winter", "spring"),
                                labels = c("Summer", "Autumn", "Winter", "Spring")))

df_wide_species <- 
  read.csv("./data-processed/df_wide_species.csv") %>% 
  dplyr::mutate(season = factor(season, 
                                levels = c("summer", "autumn", "winter", "spring"),
                                labels = c("Summer", "Autumn", "Winter", "Spring")))

# Get 'groups' (grps), 'taxa' (spp), and 'species' (sp_only) column mames
grps_cols <- colnames(df_wide_groups[12:20])
spp_cols <- colnames(df_wide_species[12:48]) # All "species" cols
sp_only_cols <- spp_cols[grepl("_sp$", spp_cols) == FALSE]

## Quick one -- any species that sum zero? (i.e. zero counts) ---------------- ##
zero_counts <-
  df_wide_species %>% 
  dplyr::select(all_of(sp_only_cols)) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = 'spp', values_to = 'n') %>% 
  dplyr::group_by(spp) %>% 
  dplyr::summarise(n = sum(n)) %>% 
  dplyr::filter(n == 0) %>% 
  pull(spp)

## Yes; remove them
sp_only_cols <- sp_only_cols[!sp_only_cols %in% zero_counts]

rm("zero_counts")

## Read spatial data ####

nz_polygon <- sf::read_sf("./data-spatial/nz/nz-coastlines-and-islands-polygons-topo-150k.gpkg")

isobaths <- 
  sf::read_sf("./data-spatial/nz/nz_isobath_ne_north_island.gpkg") %>% 
  dplyr::filter(DEPTH %in% c(50, 150, 200, 500, 1000)) %>% 
  sf::st_transform(4326) %>% 
  sf::st_crop(., xmin = 172.98, xmax = 173.8, ymin = -34.2, ymax = -35)

# mapview::mapview(isobaths)

transects_polygon <- sf::read_sf("./data-spatial/transects/far-out_transects-3km-buffer-polygon.gpkg")

## Effort summary ####

effort_summary <-
  df_wide_species %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(days_at_sea = n_distinct(date),
                   number_of_voyages = n_distinct(voyage),
                   km_surveyed = sum(id_dist_km),
                   # area_surveyed = ?,
                   number_of_10mincounts = n_distinct(id),
                   number_of_species = sum(colSums(across(all_of(sp_only_cols))) > 0),
                   number_of_individuals = sum(across(all_of(spp_cols))))

# write.csv(effort_summary, "./results/effort-summary.csv", row.names = FALSE)
rm("effort_summary")

list_of_species <- 
  ## 'MaoriName' and 'ScientificName' to be completed by hand
  data.frame(MaoriName = rep(NA, length(sp_only_cols)),
             EnglishName = sp_only_cols,
             ScientificName = rep(NA, length(sp_only_cols)))

# write.csv(list_of_species, "./results/list-of-species.csv", row.names = FALSE)
rm("list_of_species")

## Base map ####

nz_base_map <- 
  ggplot(data = nz_polygon) + 
  geom_sf(color = "black", fill = "lightgrey") + 
  coord_sf(xlim = c(172.6, 173.8), ylim = c(-35, -34.2)) + 
  theme_bw() +
  ggspatial::annotation_scale(location = "bl") +
  ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering(),
                                    location = "bl", 
                                    pad_x = unit(0.25, "cm"), pad_y = unit(0.55, "cm"),
                                    height = unit(0.8, "cm"), width = unit(0.8, "cm"))

nz_base_map_simplified <- 
  ggplot(data = nz_polygon) + 
  geom_sf(color = "black", fill = "lightgrey") + 
  coord_sf(xlim = c(172.6, 173.8), ylim = c(-35, -34.2)) + 
  theme_bw()

### Map 10-min counts ####
map_10min_counts_season <-
  nz_base_map + 
  ## Add isobaths
  geom_sf(data = isobaths[isobaths$DEPTH %in% c(200, 500, 1000),], 
          aes(linetype  = as.factor(DEPTH)), colour = "black") +
  scale_linetype_manual(values = c("dotted", "solid", "twodash"), name = "Isobaths") +
  ## Add 10-min count centroid points
  geom_point(data = df_wide_species,
             aes(x = lon, y = lat, color = season),
             alpha = 0.7) +
  scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                                "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  coord_sf(xlim = c(172.6, 173.8), ylim = c(-35, -34.2)) + 
  facet_wrap(~ season, ncol = 2) +
  guides(color = "none") +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# ggsave(map_10min_counts_season,
#        filename = "./results/EDA/map_10min-counts-by-season.pdf",
#        height = 15, width = 14, units = "cm", dpi = 200)

rm("map_10min_counts_season")

### Species richness and number of birds by transect and season ####

df_spp_aggregated <-
  df_wide_species %>% 
  # Group-by and summarise SPP
  dplyr::group_by(voyage, id_transect) %>% 
  dplyr::summarise(across(all_of(sp_only_cols), sum)) %>% 
  # Calculate total_birds counted within each grid, and the species richness
  dplyr::mutate(total_birds = rowSums(across(all_of(sp_only_cols))),
                sp_richness = rowSums(across(all_of(sp_only_cols), ~ . != 0) == TRUE),
                season = dplyr::case_when(
                  voyage == "01voyage" ~ "spring",
                  voyage == "02voyage" ~ "summer",
                  voyage == "03voyage" ~ "summer",
                  voyage == "04voyage" ~ "winter",
                  voyage == "05voyage" ~ "summer",
                  voyage == "06voyage" ~ "spring",
                  voyage == "07voyage" ~ "summer",
                  voyage == "08voyage" ~ "winter",
                  voyage == "09voyage" ~ "spring",
                  voyage == "10voyage" ~ "autumn")) %>% 
  dplyr::mutate(season = factor(season, 
                                levels = c("summer",  "autumn", 
                                                   "winter", "spring"),
                                labels = c("Summer",  "Autumn", 
                                           "Winter", "Spring"))) %>% 
  dplyr::ungroup(.)

violin_transect_sprich_season <- 
  ggplot(data = df_spp_aggregated,
       aes(x = season, y = sp_richness, fill = season)) +
  geom_violin(alpha = 0.8) +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  xlab("") + ylab("Number of species") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9))
  
violin_transect_nbirds_season<-
  ggplot(data = df_spp_aggregated,
         aes(x = season, y = total_birds, fill = season)) +
  geom_violin(alpha = 0.8) +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 50)) +
  xlab("") + ylab("Number of individuals") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9))

violin_transect_sprich_nbirds_season <- 
  violin_transect_sprich_season / violin_transect_nbirds_season + 
  patchwork::plot_annotation(tag_levels = 'A')

# ggsave(violin_transect_sprich_nbirds_season,
#        filename = "./results/violin_season_sprich-nbirds-by-transect.pdf",
#        height = 13, width = 10, units = "cm", dpi = 300)

rm("violin_transect_sprich_season", "violin_transect_nbirds_season",
   "violin_transect_sprich_nbirds_season")

### Groups/species number of individuals, lat/lon, by season ####

## NOTE these plots summarise values at the 10-min count level

## Select and pivot data.frame as it should be for plotting
# Groups
data_groups_by_season <-
  df_wide_groups %>% 
  dplyr::select(lat, lon, season, all_of(grps_cols)) %>% 
  tidyr::pivot_longer(cols = all_of(grps_cols),
                      names_to = "grps",
                      values_to = "grps_count") %>% 
  dplyr::mutate(zero_non_zero = ifelse(grps_count == 0, "zero", "non_zero")) %>% 
  dplyr::mutate(grps = dplyr::case_when(
    grps == "shearwater" ~ "Shearwater",
    grps == "petrel" ~ "Petrel",
    grps == "skua" ~ "Skua",
    grps == "albatross" ~ "Albatross",
    grps == "storm_diving_petrel" ~ "Storm and diving petrel",
    grps == "australasian_gannet" ~ "Gannet",
    grps == "mollymawk" ~ "Albatross",
    grps == "gull" ~ "Gull",
    grps == "prion" ~ "Prion"
  ))

# Species
data_species_by_season <-
  df_wide_species %>% 
  dplyr::select(lat, lon, season, all_of(sp_only_cols)) %>% 
  tidyr::pivot_longer(cols = all_of(sp_only_cols),
                      names_to = "spp",
                      values_to = "spp_count") %>% 
  dplyr::mutate(zero_non_zero = ifelse(spp_count == 0, "zero", "non_zero")) %>% 
  dplyr::mutate(spp = dplyr::case_when(
    spp == "bullers_shearwater" ~ "Buller's shearwater",
    spp == "fluttering_huttons_shearwater" ~ "Fluttering shearwater",
    spp == "flesh_footed_shearwater" ~ "Flesh-footed shearwater",
    spp == "grey_faced_petrel_oi" ~ "Grey-faced petrel",
    spp == "sooty_shearwater" ~ "Sooty shearwater",
    spp == "cook_pycroft_petrel" ~ "Cook's/pycroft's petrel",
    spp == "northern_giant_petrel" ~ "Northern giant petrel",
    spp == "black_petrel" ~ "Black petrel",
    spp == "wandering_albatross" ~ "Wandering albatross",
    spp == "wilsons_storm_petrel" ~ "Wilson's storm petrel",
    spp == "black_winged_petrel" ~ "Black-winged petrel",
    spp == "australasian_gannet" ~ "Australasian gannet",
    spp == "white_faced_storm_petrel" ~ "White-faced storm petrel",
    spp == "black_bellied_storm_petrel" ~ "Black-bellied storm petrel",
    spp == "northern_royal_albatross" ~ "Northern royal albatross",
    spp == "white_bellied_storm_petrel" ~ "White-bellied storm petrel",
    spp == "diving_petrel" ~ "Diving petrel",
    spp == "nz_storm_petrel" ~ "New Zealand storm petrel",
    spp == "arctic_skua" ~ "Arctic skua",
    spp == "white_capped_albatross" ~ "White-capped albatross",
    spp == "cape_pigeon" ~ "Cape petrel",
    spp == "black_browed_albatross" ~ "Black-browed albatross",
    spp == "fairy_prion" ~ "Fairy prion",
    spp == "white_necked_petrel" ~ "White-necked petrel"
  ))

#### (i) Groups - Map ####

## A single plot with all groups

map_grp_by_season_facetgrid <-
  nz_base_map_simplified +
  ## Bubbles
  geom_point(data = data_groups_by_season[data_groups_by_season$zero_non_zero == "non_zero", ],
             aes(x = lon, y = lat,
                 size = grps_count, 
                 shape = zero_non_zero, 
                 fill = season),
             alpha = 0.5) +
  scale_shape_manual(values = c("non_zero" = 21)) +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  scale_size_binned(range = c(1, 10), breaks = c(1,5,15,50,100), name = "Seabird numbers") +
  facet_grid(cols = vars(season), rows = vars(grps)) +
  guides(colour = "none", shape = "none", fill = "none") +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(strip.text = element_text(size = 8),
        axis.text = element_text(size = 7))

## It would be good to add zeroes... ---------------------------------------- ##

## Still not what I want... 

# map_grp_by_season_facetgrid <-
#   nz_base_map +
#   geom_point(data = data_groups_by_season,
#              aes(x = lon,
#                  y = lat,
#                  size = grps_count,
#                  shape = zero_non_zero,
#                  fill = season),
#              alpha = 0.6) +
#   scale_shape_manual(values = c("zero" = 4, "non_zero" = 21)) +
#   # scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
#   scale_fill_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
#   scale_size_binned(range = c(1, 10), breaks = c(1,5,15,50,100), name = "Seabird numbers") +
#   facet_grid(cols = vars(season), rows = vars(grps)) +
#   guides(colour = "none", shape = "none") +
#   xlab("") + ylab("") +
#   theme_bw() + 
#   theme(strip.text = element_text(size = 8),
#         axis.text = element_text(size = 7))

ggsave(map_grp_by_season_facetgrid,
       filename = "./results/EDA/map_grps_by_season.pdf",
       height = 35, width = 25, units = "cm")

rm("map_grp_by_season_facetgrid")

# -------------------------------------------------------------------------------------------- #
## Below, there are group-by-group maps -- I've ran it once and commented it as we might not use

# map_grps_by_season_list <- list()
# 
# # Update vector
# grps_cols <- unique(data_groups_by_season$grps)
# 
# for (grp in grps_cols) {
# 
#   name <- as.character(grp)
# 
#   # Subset data.frame
#   data_groups_by_season_subset <-
#     data_groups_by_season %>% dplyr::filter(grps == name)
# 
#   # Plot
#   map_grp_by_season <-
#     nz_base_map +
#     geom_point(data = data_groups_by_season_subset,
#                aes(x = lon,
#                    y = lat,
#                    size = grps_count,
#                    shape = zero_non_zero,
#                    color = season),
#                alpha = 0.5) +
#     scale_shape_manual(values = c("zero" = 4, "non_zero" = 16)) +
#     scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
#                                   "Winter" = "#E15759", "Spring" = "#76B7B2")) +
#     scale_size_binned(range = c(1, 10), breaks = c(1,5,15,50,100), name = "Seabird numbers") +
#     facet_wrap(~ season) +
#     guides(colour = "none", shape = "none") +
#     xlab("") + ylab("") +
#     ggtitle(label = name) +
#     theme_bw() +
#     theme(strip.text = element_text(size = 8),
#           axis.text = element_text(size = 7))
# 
#   # Save it
#   map_grps_by_season_list[[name]] <- map_grp_by_season
# 
#   rm("grp", "name", "data_groups_by_season_subset", "map_grp_by_season")
# }
# 
# ## Test
# # map_grps_by_season_list[["shearwater"]] # --- OK
# 
# ## Save and clean obj from environment
# saveRDS(map_grps_by_season_list,
#         file = "./results/EDA/list_maps_grps_by_season.rds")
# 
# rm("map_grps_by_season_list")
# gc()

#### (ii) Species - Violin plot ####

violin_species_by_season <-
  ggplot(data_species_by_season[data_species_by_season$spp_count > 0, ],
         aes(x = log(spp_count), ## Note 'log()'
             y = spp, 
             fill = season)) + 
  geom_violin(alpha = 0.8) +
  scale_fill_manual(values=c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                             "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  facet_wrap(~ season, ncol = 4, scales = "fixed") + 
  ylab("") + xlab("log(Number of individuals per 10 min count)") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 10))

# ggsave(violin_species_by_season,
#        filename = "./results/EDA/violin_log-number-individuals_species-by-season.pdf",
#        height = 15, width = 15, units = "cm", dpi = 300)

rm("violin_species_by_season")

#### (iii) Species - Map ####

map_species_by_season_list <- list()

for (sp in sp_only_cols) {
  
  name <- as.character(sp)

  # Subset data.frame
  data_species_by_season_subset <-
    data_species_by_season %>% dplyr::filter(spp == name)

  # Plot
  map_species_by_season <-
    nz_base_map +
    geom_point(data = data_species_by_season_subset,
               aes(x = lon,
                   y = lat,
                   size = spp_count,
                   shape = zero_non_zero,
                   color = season),
               alpha = 0.5) +
    scale_shape_manual(values = c("zero" = 4, "non_zero" = 16)) +
    scale_color_manual(values = c("summer" = "#4E79A7", "autumn" = "#F28E2B", 
                                  "winter" = "#E15759", "spring" = "#76B7B2")) +
    scale_size_binned(range = c(1, 10), breaks = c(1,5,15,50,100), name = "Seabird numbers") +
    facet_wrap(~ season) +
    guides(colour = "none", shape = "none") +
    xlab("") + ylab("") +
    theme_bw() + 
    theme(strip.text = element_text(size = 8),
          axis.text = element_text(size = 7))

  # Save it
  map_species_by_season_list[[name]] <- map_species_by_season

  rm("sp", "name", "data_species_by_season_subset", "map_species_by_season")
}

## Test
# map_species_by_season_list[["black_petrel"]]
# map_species_by_season_list[["bullers_shearwater"]]
# map_species_by_season_list[["grey_faced_petrel_oi"]]

saveRDS(map_species_by_season_list,
        file = "./results/EDA/list_maps_species_by_season.rds")

rm("map_species_by_season_list")
gc()

### Species Frequency of occurrence and Relative abundance ####

funs <- list(freq_occ = ~ sum(.x >= 1)/n() *100,
             freq_num = ~ sum(.x)/sum(dplyr::pick(total_birds_id)) *100)

data_species_fo_nf <-
  df_wide_species %>% 
  dplyr::mutate(total_birds_id = rowSums(across(all_of(spp_cols)))) %>% 
  dplyr::group_by(season) %>%
  dplyr::summarise(across(all_of(sp_only_cols), .fns = funs)) %>%
  tidyr::pivot_longer(cols = !season, 
                      names_to = "species_freq",
                      values_to = "value") %>%
  dplyr::mutate(value = round(value, digits = 2)) %>% 
  # split name into variables
  tidyr::separate(species_freq, 
                  into = c("species", "freq"),
                  sep = -8) %>% 
  # remove an extra underline
  dplyr::mutate(species = stringr::str_sub(species, end = -2))

data_species_fo_nf <-
  data_species_fo_nf %>% 
  dplyr::mutate(species_nice_name = dplyr::case_when(
    species == "bullers_shearwater" ~ "Buller's shearwater",
    species == "fluttering_huttons_shearwater" ~ "Fluttering shearwater",
    species == "flesh_footed_shearwater" ~ "Flesh-footed shearwater",
    species == "grey_faced_petrel_oi" ~ "Grey-faced petrel",
    species == "sooty_shearwater" ~ "Sooty shearwater",
    species == "cook_pycroft_petrel" ~ "Cook's/pycroft's petrel",
    species == "northern_giant_petrel" ~ "Northern giant petrel",
    species == "black_petrel" ~ "Black petrel",
    species == "wandering_albatross" ~ "Wandering albatross",
    species == "wilsons_storm_petrel" ~ "Wilson's storm petrel",
    species == "black_winged_petrel" ~ "Black-winged petrel",
    species == "australasian_gannet" ~ "Australasian gannet",
    species == "white_faced_storm_petrel" ~ "White-faced storm petrel",
    species == "black_bellied_storm_petrel" ~ "Black-bellied storm petrel",
    species == "northern_royal_albatross" ~ "Northern royal albatross",
    species == "white_bellied_storm_petrel" ~ "White-bellied storm petrel",
    species == "diving_petrel" ~ "Diving petrel",
    species == "nz_storm_petrel" ~ "New Zealand storm petrel",
    species == "arctic_skua" ~ "Arctic skua",
    species == "white_capped_albatross" ~ "White-capped albatross",
    species == "cape_pigeon" ~ "Cape petrel",
    species == "black_browed_albatross" ~ "Black-browed albatross",
    species == "fairy_prion" ~ "Fairy prion",
    species == "white_necked_petrel" ~ "White-necked petrel"
  ))

rm("funs")

#### (i) FO/NF plots ####

## Frequency of occurrence
plot_freq_occ <-
  data_species_fo_nf %>% 
  dplyr::filter(freq == "freq_occ") %>% 
  ggplot(., aes(x = forcats::fct_reorder(as.factor(species_nice_name), value), 
                y = value, 
                fill = season)) + 
  geom_col() +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  facet_grid(~ season) +
  ylab("Frequency of occurrence (%)") + xlab ("") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 9))

## Relative abundance (numeric frequency)
plot_freq_num <-
  data_species_fo_nf %>% 
  dplyr::filter(freq == "freq_num") %>% 
  ggplot(., aes(x = forcats::fct_reorder(as.factor(species_nice_name), value), 
                y = value, 
                fill = season)) + 
  geom_col() +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  facet_grid(~ season) +
  ylab("Relative abundance (%)") + xlab ("") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 9))

## Patchwork these plots and save it
freqs_occ_num <-
  plot_freq_occ / plot_freq_num + 
  patchwork::plot_annotation(tag_levels = 'A')

ggsave(freqs_occ_num,
       filename = "./results/species_frqs-occ-num.pdf",
       width = 16, height = 25, units = "cm", dpi = 300)

rm("plot_freq_occ", "plot_freq_num", "freqs_occ_num", "data_species_fo_nf")
