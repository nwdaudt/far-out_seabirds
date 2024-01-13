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

# >>> Colours
# "summer" = "#E69F00" 
# "winter" = "#56B4E9" 
# "spring" = "grey50"

## Read seabird data raw ####

# df_long <- read.csv("./data-processed/df_long.csv") # Just in case

df_wide_groups <- 
  read.csv("./data-processed/df_wide_groups.csv")[, -1] %>% 
  dplyr::mutate(season = factor(season, levels = c("summer", "autumn", "winter", "spring")))

df_wide_species <- 
  read.csv("./data-processed/df_wide_species.csv")[, -1] %>% 
  dplyr::mutate(season = factor(season, levels = c("summer", "autumn", "winter", "spring")))

# Get 'groups' (grps), 'taxa' (spp), and 'species' (sp_only) column mames
grps_cols <- colnames(df_wide_groups[12:20])
spp_cols <- colnames(df_wide_species[12:48]) # All "species" cols
sp_only_cols <- spp_cols[grepl("_sp$", spp_cols) == FALSE]

## Quick one -- any groups that sum zero? (i.e. zero counts) ---------------- ##
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

# write.csv(effort_summary, "./results/effort-summary.csv")
rm("effort_summary")

list_of_species <- 
  ## 'Maori_species' and 'SCI_species' to be completed by hand
  data.frame(MaoriName = rep(NA, length(sp_only_cols)),
             EnglishName = sp_only_cols,
             ScientificName = rep(NA, length(sp_only_cols)))

# write.csv(list_of_species, "./results/list-of-species.csv")
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

## Plots ####

### Map 10-min counts ####
map_10min_counts_season <-
  nz_base_map + 
  ## Add isobaths
  geom_sf(data = isobaths[isobaths$DEPTH %in% c(200, 500, 1000),], 
          aes(linetype  = as.factor(DEPTH)), colour = "black") +
  scale_linetype_manual(values = c("dotted", "solid", "twodash"), name = "isobaths") +
  ## Add 10-min count centroid points
  geom_point(data = df_wide_species,
             aes(x = lon, y = lat, color = season)) +
  scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
  coord_sf(xlim = c(172.6, 173.8), ylim = c(-35, -34.2)) + 
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "right")

ggsave(map_10min_counts_season,
       filename = "./results/EDA/map_10min-counts.pdf",
       height = 11, width = 15, units = "cm", dpi = 300)

### Mean (range) species richness and number of birds, per 10-min count (COME BACK HERE) ####

data_10min_spprich_nbirds <-
  df_wide_species %>% 
  dplyr::select(id, season, all_of(sp_only_cols)) %>% 
  tidyr::pivot_longer(cols = all_of(sp_only_cols),
                      names_to = "spp",
                      values_to = "spp_count") %>% 
  dplyr::filter(spp_count > 0) %>% 
  dplyr::group_by(id, season) %>% 
  dplyr::summarise(spp_richness = length(unique(spp)),
                   n_birds = log10(sum(spp_count))) %>% 
  dplyr::ungroup(.) %>% 
  tidyr::pivot_longer(cols = c("spp_richness", "n_birds"),
                      names_to = "var",
                      values_to = "value")

# Labels
labs <- c("Number of birds by 10-min counts (log10-transformed)", "Species richness by 10-min count")
names(labs) <- c("n_birds", "spp_richness")

boxplot_10min_spprich_nbirds_season <- 
  ggplot(data_10min_spprich_nbirds,
         aes(x = season, y = value, fill = season)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
  facet_wrap(~ var, nrow = 2, scales = "free_y", labeller = labeller(var = labs)) + 
  guides(fill = "none") +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(strip.text = element_text(size = 10))

ggsave(boxplot_10min_spprich_nbirds_season,
       filename = "./results/EDA/boxplot_10min_spp-richness_n-birds_season.pdf",
       height = 12, width = 12, units = "cm", dpi = 300)  

### Relative abundance: groups/species, by season ####

## Select and pivot data.frame as it should be for plotting
# Groups
data_groups_by_season <-
  df_wide_groups %>% 
  dplyr::select(lat, lon, season, all_of(grps_cols)) %>% 
  tidyr::pivot_longer(cols = all_of(grps_cols),
                      names_to = "grps",
                      values_to = "grps_count") %>% 
  dplyr::mutate(zero_non_zero = ifelse(grps_count == 0, "zero", "non_zero"))

# Species
data_species_by_season <-
  df_wide_species %>% 
  dplyr::select(lat, lon, season, all_of(sp_only_cols)) %>% 
  tidyr::pivot_longer(cols = all_of(sp_only_cols),
                      names_to = "spp",
                      values_to = "spp_count") %>% 
  dplyr::mutate(zero_non_zero = ifelse(spp_count == 0, "zero", "non_zero"))

#### (i) Groups - Map ####

## A single plot with all groups

map_grp_by_season_facetgrid <-
  nz_base_map +
  ## Bubbles
  geom_point(data = data_groups_by_season[data_groups_by_season$zero_non_zero == "non_zero", ],
             aes(x = lon, y = lat,
                 size = grps_count, shape = zero_non_zero, fill = season),
             alpha = 0.5) +
  scale_shape_manual(values = c("non_zero" = 21)) +
  # scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
  scale_fill_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
  scale_size_binned(range = c(1, 10), breaks = c(1,5,15,50,100), name = "Seabird numbers") +
  ## Facet
  facet_grid(cols = vars(season), rows = vars(grps)) +
  guides(colour = "none", shape = "none", fill = "none") +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(strip.text = element_text(size = 8),
        axis.text = element_text(size = 7))

## NEED TO ADD ZEROES --------------------------------------------------------- ##

# map_grp_by_season_facetgrid <-
#   map_grp_by_season_facetgrid +
#   ## Add zero counts
#   geom_point(data = data_groups_by_season[data_groups_by_season$zero_non_zero == "zero", ],
#              aes(x = lon, y = lat,
#                  size = 1, shape = zero_non_zero, color = "black")) +
#   scale_shape_manual(values = c("zero" = 4)) +
#   ## Facet
#   facet_grid(cols = vars(season), rows = vars(grps)) +
#   guides(colour = "none", shape = "none") +
#   xlab("") + ylab("") +
#   theme_bw() + 
#   theme(strip.text = element_text(size = 8),
#         axis.text = element_text(size = 7))

## Below is working, but it is still not what I want... ------------ ---------- ##
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
       height = 35, width = 25, units = "cm", dpi = 300)

# -------------------------------------------------------------------------------------------- #
## Below, there are group-by-group maps -- I've ran it once and commented it as we might not use

map_grps_by_season_list <- list()

for (grp in grps_cols) {

  name <- as.character(grp)

  # Subset data.frame
  data_groups_by_season_subset <-
    data_groups_by_season %>% dplyr::filter(grps == name)

  # Plot
  map_grp_by_season <-
    nz_base_map +
    geom_point(data = data_groups_by_season_subset,
               aes(x = lon,
                   y = lat,
                   size = grps_count,
                   shape = zero_non_zero,
                   color = season)) +
    scale_shape_manual(values = c("zero" = 4, "non_zero" = 16)) +
    scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
    scale_size_binned(range = c(1, 10), breaks = c(1,5,15,50,100), name = "Seabird numbers") +
    facet_wrap(~ season) +
    guides(colour = "none", shape = "none") +
    xlab("") + ylab("") +
    theme_bw() +
    theme(strip.text = element_text(size = 8),
          axis.text = element_text(size = 7))

  # Save it
  map_grps_by_season_list[[name]] <- map_grp_by_season

  rm("grp", "name", "data_groups_by_season_subset", "map_grp_by_season")
}

## Test
# map_grps_by_season_list[["shearwater"]] # --- OK

## Save and clean obj from environment
saveRDS(map_grps_by_season_list,
        file = "./results/EDA/list_maps_grps_by_season.rds")

rm("map_grps_by_season_list")
gc()

#### (ii) Species - Violin plot ####

violin_species_by_season <-
  ggplot(data_species_by_season,
         aes(x = log10(spp_count), ## Note 'log10()'
             y = spp, 
             fill = season)) + 
  geom_violin() +
  scale_fill_manual(values=c("summer" = "#E69F00", 
                             "winter" = "#56B4E9",
                             "spring" = "grey50")) +
  facet_wrap(~ season, ncol = 3, scales = "fixed") + 
  ylab("") + xlab("log10(relative abundance)") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 8, colour = "black"),
        axis.title = element_text(size = 9),
        strip.text = element_text(size = 8))

ggsave(violin_species_by_season,
       filename = "./results/EDA/violin_log10_species_by_season.pdf",
       height = 15, width = 15, units = "cm", dpi = 300)

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
                   color = season)) +
    scale_shape_manual(values = c("zero" = 4, "non_zero" = 16)) +
    scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
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

### Species occurrence/relative abundance ####

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

rm("funs")

#### (i) FO/NF plots ####

## Frequency of occurrence
plot_freq_occ <-
  data_species_fo_nf %>% 
  dplyr::filter(freq == "freq_occ") %>% 
  ggplot(., aes(x = forcats::fct_reorder(as.factor(species), value), 
                y = value, 
                fill = season)) + 
  geom_col() +
  scale_fill_manual(values = c("summer" = "#E69F00", 
                               "winter" = "#56B4E9", 
                               "spring" = "grey50")) +
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
  ggplot(., aes(x = forcats::fct_reorder(as.factor(species), value), 
                y = value, 
                fill = season)) + 
  geom_col() +
  scale_fill_manual(values = c("summer" = "#E69F00", 
                               "winter" = "#56B4E9", 
                               "spring" = "grey50")) +
  facet_grid(~ season) +
  ylab("Relative abundance (%)") + xlab ("") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 9))

## Patchwork these plots and save it
freqs_occ_num <-
  plot_freq_occ / plot_freq_num

ggsave(freqs_occ_num,
       filename = "./results/EDA/spp_frqs-occ-num.pdf",
       width = 16, height = 25, units = "cm", dpi = 300)

rm("plot_freq_occ", "plot_freq_num", "freqs_occ_num")
