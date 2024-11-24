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

df_long <- read.csv("./data-processed/df_long.csv")

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
grps_cols <- colnames(df_wide_groups[12:17])[-2]
spp_cols <- colnames(df_wide_species[12:49]) # All "species" cols
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

## Get 'sp_only_ROWS' from 'df_long', too
sp_only_rows <- unique(df_long$seabird_sp)[-1] # "-1" == ""
sp_only_rows <- sp_only_rows[grepl(" sp$", sp_only_rows) == FALSE]

### Prep Biomass data ####

df_biomass <- 
  df_long %>% 
  # Remove all obs without body mass (i.e. they are birds not IDed or START/END points)
  dplyr::filter(!is.na(species_mass_kg)) %>% 
  dplyr::mutate(biomass_km = (seabird_ct * species_mass_kg) / id_dist_km)

## Groups --
df_gr_biomass <- 
  df_biomass %>% 
  dplyr::group_by(season, seabird_gr_foraging) %>% 
  dplyr::summarise(biomass_gr_km = sum(biomass_km))

df_gr_biomass$season <- factor(df_gr_biomass$season,
                               levels = c("summer", "autumn", "winter", "spring"),
                               labels = c("Summer", "Autumn", "Winter", "Spring"))

df_gr_biomass$seabird_gr_foraging <- factor(df_gr_biomass$seabird_gr_foraging,
                                            levels = c("Larids",
                                                       "Large procellariids",
                                                       "Medium procellariids",
                                                       "Small procellariids",
                                                       "Sulids"),
                                            labels = c("Skuas",
                                                       "Large procellariids",
                                                       "Medium procellariids",
                                                       "Small procellariids",
                                                       "Sulids"))
## Species --
df_sp_biomass <- 
  df_biomass %>% 
  dplyr::group_by(season, seabird_sp) %>% 
  dplyr::summarise(biomass_sp_km = sum(biomass_km))

df_sp_biomass <-
  df_sp_biomass %>% 
  dplyr::mutate(seabird_sp = dplyr::case_when(
    seabird_sp == "Grey-faced petrel (Oi)" ~ "Grey-faced petrel",
    seabird_sp == "Fluttering/Hutton's shearwater" ~ "Fluttering shearwater",
    .default = as.character(seabird_sp)
  ))

df_sp_biomass$season <- factor(df_sp_biomass$season,
                               levels = c("summer", "autumn", "winter", "spring"),
                               labels = c("Summer", "Autumn", "Winter", "Spring"))

## Read spatial data ####

nz_polygon <- sf::read_sf("./data-spatial/nz/nz-coastlines-and-islands-polygons-topo-150k.gpkg")

isobaths <- 
  sf::read_sf("./data-spatial/nz/nz_isobath_ne_north_island.gpkg") %>% 
  dplyr::filter(DEPTH %in% c(50, 150, 200, 500, 1000)) %>% 
  sf::st_transform(4326) %>% 
  sf::st_crop(., xmin = 172.98, xmax = 173.8, ymin = -34.2, ymax = -35)

# mapview::mapview(isobaths)

transects_polygon <- sf::read_sf("./data-spatial/transects/far-out_transects-3km-buffer-polygon.gpkg")

## Effort summary (table) ####

effort_summary <-
  df_wide_species %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(days_at_sea = n_distinct(date),
                   number_of_voyages = n_distinct(voyage),
                   km_surveyed = round(sum(id_dist_km), digits = 1),
                   area_surveyed = round(sum(id_dist_km * 0.2), digits = 1),
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

## Summarise the data
df_spp_aggregated <-
  df_wide_species %>% 
  # Group-by and summarise SPP
  dplyr::group_by(voyage, id_transect) %>% 
  dplyr::summarise(across(all_of(c("id_dist_km", sp_only_cols)), sum)) %>% 
  # Calculate total_birds counted within each grid, and the species richness
  dplyr::mutate(total_birds = rowSums(across(all_of(sp_only_cols))),
                # density_birds = total birds / area surveyed
                density_birds = round((rowSums(across(all_of(sp_only_cols))) / sum(id_dist_km * 0.2)), digits = 3),
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

## Summarise the biomass data... needs a bit of wrangling on its own
df_biomass.transect_aggregated <-
  df_biomass %>% 
  dplyr::group_by(voyage, id_transect) %>% 
  dplyr::summarise(biomass_km_voyage_transect = sum(biomass_km)) %>% 
  dplyr::mutate(season = dplyr::case_when(
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

## Get geometric mean/sd
geom_mean.sd <- 
  df_spp_aggregated %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(sp_richness_g.mean = exp(mean(log(sp_richness))),
                   sp_richness_g.sd = exp(sd(log(sp_richness))),
                   total_birds_g.mean = exp(mean(log(total_birds))),
                   total_birds_g.sd = exp(sd(log(total_birds))),
                   density_birds_g.mean = exp(mean(log(density_birds))),
                   density_birds_g.sd = exp(sd(log(density_birds))))

tmp <- 
  df_biomass.transect_aggregated %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(biomass_g.mean = exp(mean(log(biomass_km_voyage_transect))),
                   biomass_g.sd = exp(sd(log(biomass_km_voyage_transect))))

# Put them together
geom_mean.sd <- cbind(geom_mean.sd, tmp[, -1])

## Get 'n' for each season
n_per_season <- 
  df_spp_aggregated %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::pull(n)

n_per_season <- paste0("n = ", n_per_season)

rm("tmp")

### Plots
violin_transect_sprich_season <- 
  ggplot(data = df_spp_aggregated,
       aes(x = season, y = sp_richness, fill = season, color = season)) +
  geom_violin(alpha = 0.8) +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                                "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  geom_point(data = geom_mean.sd,
             aes(x = season, y = sp_richness_g.mean), color = "black", size = 1.2) +
  geom_linerange(data = geom_mean.sd,
                aes(x = season,
                    y = sp_richness_g.mean,
                    ymin = (sp_richness_g.mean-sp_richness_g.sd),
                    ymax = (sp_richness_g.mean+sp_richness_g.sd)),
                color = "black") +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2)) +
  annotate("text", x = 1:4, y = 11.2, label = n_per_season, size = 3) +
  xlab("") + ylab("Number of species") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9))

violin_transect_nbirds_season <-
  ggplot(data = df_spp_aggregated,
         aes(x = season, y = total_birds, fill = season, color = season)) +
  geom_violin(alpha = 0.8) +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                                "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  geom_point(data = geom_mean.sd,
             aes(x = season, y = total_birds_g.mean), color = "black", size = 1.2) +
  geom_linerange(data = geom_mean.sd,
                 aes(x = season,
                     y = total_birds_g.mean,
                     ymin = (total_birds_g.mean-total_birds_g.sd),
                     ymax = (total_birds_g.mean+total_birds_g.sd)),
                 color = "black") +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 50)) +
  xlab("") + ylab("Number of individuals") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9))

violin_transect_density_season <-
  ggplot(data = df_spp_aggregated,
         aes(x = season, y = density_birds, fill = season, color = season)) +
  geom_violin(alpha = 0.8) +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                                "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  geom_point(data = geom_mean.sd,
             aes(x = season, y = density_birds_g.mean), color = "black", size = 1.2) +
  geom_linerange(data = geom_mean.sd,
                 aes(x = season,
                     y = density_birds_g.mean,
                     ymin = (density_birds_g.mean-density_birds_g.sd),
                     ymax = (density_birds_g.mean+density_birds_g.sd)),
                 color = "black") +
  xlab("") + ylab("Density (birds/km²)") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9))

violin_transect_biomass_season <-
  ggplot(data = df_biomass.transect_aggregated,
         aes(x = season, y = biomass_km_voyage_transect, fill = season, color = season)) +
  geom_violin(alpha = 0.8) +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                                "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  geom_point(data = geom_mean.sd,
             aes(x = season, y = biomass_g.mean), color = "black", size = 1.2) +
  geom_linerange(data = geom_mean.sd,
                 aes(x = season,
                     y = biomass_g.mean,
                     ymin = (biomass_g.mean-biomass_g.sd),
                     ymax = (biomass_g.mean+biomass_g.sd)),
                 color = "black") +
  xlab("") + ylab("Biomass (kg/km²)") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9))

## Patchwork them and save
violin_transect_sprich_nbirds_density_biomass_season <- 
  violin_transect_sprich_season / 
  violin_transect_nbirds_season / 
  violin_transect_density_season / 
  violin_transect_biomass_season + 
  patchwork::plot_annotation(tag_levels = 'A')

ggsave(violin_transect_sprich_nbirds_density_biomass_season,
       filename = "./results/violin_season_sprich-nbirds-density-biomass-by-transect.pdf",
       height = 21, width = 10, units = "cm", dpi = 300)

rm("violin_transect_sprich_season", 
   "violin_transect_nbirds_season",
   "violin_transect_density_season",
   "violin_transect_biomass_season",
   "violin_transect_sprich_nbirds_density_biomass_season",
   "df_spp_aggregated",
   "df_biomass.transect_aggregated",
   "geom_mean.sd")

### Groups/species number of individuals, lat/lon, by season ####

## NOTE these plots summarise values at the 10-min count level

## Select and pivot data.frame as it should be for plotting
# Groups
data_groups_by_season <-
  df_wide_groups %>% 
  dplyr::select(lat, lon, season, id_dist_km, all_of(grps_cols)) %>% 
  tidyr::pivot_longer(cols = all_of(grps_cols),
                      names_to = "grps",
                      values_to = "grps_count") %>% 
  dplyr::mutate(grps_density = grps_count / id_dist_km) %>% 
  dplyr::mutate(zero_non_zero = ifelse(grps_count == 0, "zero", "non_zero")) %>% 
  dplyr::mutate(grps = dplyr::case_when(
    grps == "medium_procellariids" ~ "Medium procellariids",
    grps == "large_procellariids" ~ "Large procellariids",
    grps == "small_procellariids" ~ "Small procellariids",
    grps == "sulids" ~ "Sulids",
    grps == "larids" ~ "Skuas",
  )) %>% 
  dplyr::mutate(grps = factor(grps,
                              levels = c("Large procellariids", "Medium procellariids",
                                         "Small procellariids", "Sulids", "Skuas")))

# Species
data_species_by_season <-
  df_wide_species %>% 
  dplyr::select(lat, lon, season, id_dist_km, all_of(sp_only_cols)) %>% 
  tidyr::pivot_longer(cols = all_of(sp_only_cols),
                      names_to = "spp",
                      values_to = "spp_count") %>% 
  dplyr::mutate(spp_density = spp_count / id_dist_km) %>% 
  dplyr::mutate(zero_non_zero = ifelse(spp_count == 0, "zero", "non_zero")) %>% 
  dplyr::mutate(spp = dplyr::case_when(
    spp == "bullers_shearwater" ~ "Buller's shearwater",
    spp == "fluttering_shearwater" ~ "Fluttering shearwater",
    spp == "flesh_footed_shearwater" ~ "Flesh-footed shearwater",
    spp == "grey_faced_petrel" ~ "Grey-faced petrel",
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
    spp == "white_necked_petrel" ~ "White-necked petrel",
    spp == "brown_skua" ~ "Brown skua"
  ))

#### (i) Groups - Map ####

map_grp.density_by_season_facetgrid <-
  nz_base_map_simplified +
  ## Bubbles
  geom_point(data = data_groups_by_season,
             aes(x = lon, y = lat,
                 size = grps_density, 
                 shape = zero_non_zero, 
                 fill = season),
             alpha = 0.5) +
  scale_shape_manual(values = c("non_zero" = 21, "zero" = 4)) +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  scale_size_binned(range = c(1, 12), breaks = c(0.2,2.5,5,10), name = "Density (birds/km²)") +
  facet_grid(cols = vars(season), rows = vars(grps)) +
  guides(colour = "none", shape = "none", fill = "none") +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(strip.text = element_text(size = 7), 
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.title = element_text(size = 8.5))

# ggsave(map_grp.density_by_season_facetgrid,
#        filename = "./results/EDA/map_grps-density_by_season.pdf",
#        height = 17, width = 20, units = "cm")

rm("map_grp.density_by_season_facetgrid", 
   # "map_grp_by_season_facetgrid",
   "data_groups_by_season")

gc()

#### (ii) Species - Violin plot ####

violin_species_by_season <-
  ggplot(data_species_by_season,
         aes(x = log(spp_count), ## NOTE: 'log()'
             y = spp, 
             fill = season)) + 
  geom_violin(alpha = 0.8) +
  scale_fill_manual(values=c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                             "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  facet_wrap(~ season, ncol = 4, scales = "fixed") + 
  ylab("") + xlab("log(Number of individuals per 10-min count)") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 9, colour = "black"),
        axis.title = element_text(size = 9),
        strip.text = element_text(size = 9))

# ggsave(violin_species_by_season,
#        filename = "./results/EDA/violin_log-number-individuals_species-by-season.pdf",
#        height = 15, width = 16, units = "cm", dpi = 300)

rm("violin_species_by_season")

#### (iii) Species - Map ####

map_species_by_season_list <- list()

sp_only_cols_nicename <- unique(data_species_by_season$spp)

for (sp in sp_only_cols_nicename) {
  
  name <- as.character(sp)

  # Subset data.frame
  data_species_by_season_subset <-
    data_species_by_season %>% dplyr::filter(spp == name)

  # Plot
  map_species_by_season <-
    nz_base_map_simplified +
    geom_point(data = data_species_by_season_subset,
               aes(x = lon, y = lat,
                   size = spp_density, 
                   shape = zero_non_zero, 
                   fill = season),
               alpha = 0.5) +
    scale_shape_manual(values = c("non_zero" = 21, "zero" = 4)) +
    scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                                 "Winter" = "#E15759", "Spring" = "#76B7B2")) +
    scale_size_binned(range = c(1, 12), breaks = c(0.2,2.5,5,10), name = "Density (birds/km²)") +
    facet_grid(cols = vars(season), rows = vars(spp)) +
    guides(colour = "none", shape = "none", fill = "none") +
    xlab("") + ylab("") +
    theme_bw() + 
    theme(strip.text = element_text(size = 8), 
          axis.text = element_text(size = 7),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

  # Save it
  map_species_by_season_list[[name]] <- map_species_by_season

  rm("sp", "name", "data_species_by_season_subset", "map_species_by_season")
}

## Test
# map_species_by_season_list[["Black petrel"]]
# map_species_by_season_list[["Buller's shearwater"]]
# map_species_by_season_list[["Grey-faced petrel"]]

# saveRDS(map_species_by_season_list,
#         file = "./results/EDA/list_maps_species_by_season.rds")

rm("map_species_by_season_list",
   "data_species_by_season")
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
    species == "fluttering_shearwater" ~ "Fluttering shearwater",
    species == "flesh_footed_shearwater" ~ "Flesh-footed shearwater",
    species == "grey_faced_petrel" ~ "Grey-faced petrel",
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
    species == "white_necked_petrel" ~ "White-necked petrel",
    species == "brown_skua" ~ "Brown skua"
  ))

rm("funs")

# write.csv(data_species_fo_nf,
#           file = "./results/species_frqs-occ-num_season.csv",
#           row.names = FALSE)

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

# ggsave(freqs_occ_num,
#        filename = "./results/species_frqs-occ-num.pdf",
#        width = 16, height = 25, units = "cm", dpi = 300)

rm("plot_freq_occ", "plot_freq_num", "freqs_occ_num", "data_species_fo_nf")

### Groups/species biomass, by season ####

##### (%) Percentage

## Groups --
biomass_gr_plot <-
  ggplot(df_gr_biomass, 
         aes(x = season, y = biomass_gr_km, fill = seabird_gr_foraging)) +
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_brewer(palette = "Dark2", name = NULL) + 
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) + 
  xlab("") + ylab("% Biomass (kg/km²)") + 
  guides(fill = guide_legend(ncol = 1)) +
  theme_bw() + 
  theme(axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.text.x = element_text(size = 7.5, colour = "black"),
        legend.text = element_text(size = 7),
        legend.position = "right")

# ggsave(biomass_gr_plot,
#        filename = "./results/biomass-per-group.pdf",
#        width = 18, height = 12, units = "cm", dpi = 300)

## Species --

# pal_26cols <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(26)

pal_26cols <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(26)

biomass_sp_plot <-
  ggplot(df_sp_biomass, 
         aes(x = season, y = biomass_sp_km, fill = seabird_sp)) +
  geom_bar(position = "fill", stat = "identity") + 
  # scale_fill_brewer(palette = "Dark2", name = NULL) + 
  scale_fill_manual(values = pal_26cols, name = NULL) +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) + 
  xlab("") + ylab("% Biomass (kg/km²)") + 
  guides(fill = guide_legend(ncol = 2)) +
  theme_bw() + 
  theme(axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.text.x = element_text(size = 7.5, colour = "black"),
        legend.text = element_text(size = 7),
        legend.position = "right")

# ggsave(biomass_sp_plot,
#        filename = "./results/biomass-per-species.pdf",
#        width = 22, height = 12, units = "cm", dpi = 300)

##### Stack

biomass_gr_plot_stack <-
  ggplot(df_gr_biomass, 
         aes(x = season, y = biomass_gr_km, fill = seabird_gr_foraging)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_brewer(palette = "Dark2", name = NULL) + 
  xlab("") + ylab("Total biomass (kg/km²)") + 
  theme_bw() + 
  theme(axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.text.x = element_text(size = 7.5, colour = "black"),
        legend.position = "none")

biomass_sp_plot_stack <-
  ggplot(df_sp_biomass, 
         aes(x = season, y = biomass_sp_km, fill = seabird_sp)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values = pal_26cols, name = NULL) +
  xlab("") + ylab("Total biomass (kg/km²)") + 
  theme_bw() + 
  theme(axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.text.x = element_text(size = 7.5, colour = "black"),
        legend.position = "none")

#### Patchwork

layout <- "
AAABBB#
CCCDDD#
"

biomass_plot <-
  biomass_gr_plot_stack + biomass_gr_plot + 
  biomass_sp_plot_stack + biomass_sp_plot + 
  patchwork::plot_annotation(tag_levels = 'A') + 
  patchwork::plot_layout(design = layout)

# ggsave(biomass_plot,
#        filename = "./results/biomass-patchwork.pdf",
#        width = 22, height = 16, units = "cm", dpi = 300)

rm("biomass_gr_plot_stack", "biomass_gr_plot",
   "biomass_sp_plot_stack", "biomass_sp_plot")

### Selected spp ~ env vars #### 

df_wide_species_env <- 
  read.csv("./data-processed/df_wide_species_chl_sst.csv")

cols_selected_spp <- c("bullers_shearwater", "grey_faced_petrel", "black_petrel",
                  "cook_pycroft_petrel", "white_faced_storm_petrel", "fairy_prion",
                  "fluttering_shearwater", "flesh_footed_shearwater")

cols_env_vars <- c("chl_a", "chl_a_log", "sst")

df_selected_spp_env <-
  df_wide_species_env %>% 
  dplyr::select(all_of(c(cols_selected_spp, cols_env_vars))) %>% 
  ## Transform it to long-format (first 'spp', then 'env_vars')
  tidyr::pivot_longer(cols = all_of(cols_selected_spp),
                      names_to = "spp",
                      values_to = "spp_numbers") %>% 
  tidyr::pivot_longer(cols = all_of(cols_env_vars),
                      names_to = "env_vars",
                      values_to = "env_vars_value") %>% 
  ## Filter away 'zero' observations
  dplyr::filter(! spp_numbers == 0) %>% 
  dplyr::mutate(spp = case_when(
    spp == "bullers_shearwater" ~ "Buller's shearwater",
    spp == "grey_faced_petrel" ~ "Grey-faced petrel",
    spp == "black_petrel" ~ "Black petrel",
    spp == "cook_pycroft_petrel" ~ "Cook's/Pycroft's petrel",
    spp == "white_faced_storm_petrel" ~ "White-faced storm petrel",
    spp == "fairy_prion" ~ "Fairy prion",
    spp == "fluttering_shearwater" ~ "Fluttering shearwater",
    spp == "flesh_footed_shearwater" ~ "Flesh-footed shearwater"
  )) %>% 
  dplyr::mutate(env_vars = case_when(
    env_vars == "chl_a" ~ "CHL-a",
    env_vars == "chl_a_log" ~ "log(CHL-a)",
    env_vars == "sst" ~ "SST"
  ))

plot_selected_spp_env_vars <-
  ggplot(data = df_selected_spp_env,
         aes(x = env_vars_value, y = log(spp_numbers))) +
  geom_point(alpha = 0.8, size = 0.8) + 
  geom_smooth() + ## Using default LOESS
  facet_wrap(~ spp + env_vars, 
             nrow = 8, ncol = 3,
             scales = "free") +
  xlab("Environmental gradient") + ylab("log(Seabird count [ind/10-min])") + 
  theme_bw() +
  theme(strip.text = element_text(size = 6),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6))

ggsave(plot_selected_spp_env_vars,
       filename = "./results/selected-spp_vs_env-vars.pdf",
       width = 16, height = 22, units = "cm", dpi = 300)
