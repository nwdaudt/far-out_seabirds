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
  read.csv("./data-processed/df_wide_groups.csv") %>% 
  ## Until this moment (Nov 2023), the only 'autumn' trip was on the 25-26/May/2023,
  ## which we'll consider as 'winter' instead...
  dplyr::mutate(season = ifelse(season == "autumn", "winter", season)) %>% 
  dplyr::mutate(season = factor(season, levels = c("summer", "autumn", "winter", "spring")))

df_wide_species <- 
  read.csv("./data-processed/df_wide_species.csv") %>% 
  ## Until this moment (Nov 2023), the only 'autumn' trip was on the 25-26/May/2023,
  ## which we'll consider as 'winter' instead...
  dplyr::mutate(season = ifelse(season == "autumn", "winter", season)) %>% 
  dplyr::mutate(season = factor(season, levels = c("summer", "autumn", "winter", "spring")))

# Get 'groups' (grps), 'taxa' (spp), and 'species' (sp_only) column mames
grps_cols <- colnames(df_wide_groups[13:ncol(df_wide_groups)])
spp_cols <- colnames(df_wide_species[13:ncol(df_wide_species)]) # All "species" cols
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
# isobaths <- ???

## Effort summary ####

effort_summary <-
  df_wide_species %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(days_at_sea = n_distinct(date),
                   number_of_voyages = n_distinct(voyage),
                   km_surveyed = sum(id_dist_km),
                   # area_surveyed = ,
                   number_of_10mincounts = n_distinct(id),
                   number_of_species = sum(colSums(across(all_of(sp_only_cols))) > 0),
                   number_of_individuals = sum(across(all_of(spp_cols))))

# write.csv(effort_summary, "./results/effort-summary.csv")
rm("effort_summary")

list_of_species <- 
  ## 'Maori_species' and 'SCI_species' to be completed by hand
  data.frame(Maori_species = rep(NA, length(sp_only_cols)),
             EN_species = sp_only_cols,
             SCI_species = rep(NA, length(sp_only_cols)))

# write.csv(list_of_species, "./results/list-of-species.csv") # [7 Nov 2023]
rm("list_of_species")

## Base map ####

nz_base_map <- 
  ggplot(data = nz_polygon) + 
  geom_sf(color = "black", fill = "lightgrey") + 
  coord_sf(xlim = c(172.6, 173.8), ylim = c(-35, -34.2)) + 
  theme_bw() +
  ggspatial::annotation_scale(location = "bl", text_cex = 1.2) +
  ggspatial::annotation_north_arrow(location = "tl")

## Plots ####

### Counts: species/season ####

# Select and pivot data.frame as it should be for plotting
data_species_by_season <-
  df_wide_species %>% 
  dplyr::select(lat, lon, season, all_of(sp_only_cols)) %>% 
  tidyr::pivot_longer(cols = all_of(sp_only_cols),
                      names_to = "spp",
                      values_to = "spp_count") %>% 
  dplyr::mutate(zero_non_zero = ifelse(spp_count == 0, "zero", "non_zero"))

#### (i) Violin plot ####

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
  ylab("") + xlab("log10(species count)") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 8, colour = "black"),
        axis.title = element_text(size = 9),
        strip.text = element_text(size = 8))

# Warning: Groups with fewer than two data points have been dropped. 

ggsave(violin_species_by_season,
       filename = "./results/EDA/violin_log10_species_by_season.pdf")

rm("violin_species_by_season")

#### (ii) Map ####

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

saveRDS(map_species_by_season_list,
        file = "./results/EDA/map_species_by_season_list.rds")

rm("map_species_by_season_list")
gc()

# maps <- readRDS("./results/EDA/map_species_by_season_list.rds")
# maps[["white_faced_storm_petrel"]]

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
