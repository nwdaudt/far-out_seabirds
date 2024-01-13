## 
## Constrained ordination & rarefaction curves
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##


## Libraries ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(gllvm)
library(iNEXT)

# >>> Colours
# "summer" = "#E69F00" 
# "winter" = "#56B4E9" 
# "spring" = "grey50"

## Read seabird data raw ####

# df_long <- read.csv("./data-processed/df_long.csv")

# df_wide_groups <- 
#   read.csv("./data-processed/df_wide_groups.csv")[, -1] %>% 
#   dplyr::mutate(season = factor(season, levels = c("summer", "autumn", "winter", "spring")))

df_wide_species <- 
  read.csv("./data-processed/df_wide_species.csv")[, -1] %>% 
  dplyr::mutate(season = factor(season, levels = c("summer", "autumn", "winter", "spring")))

## Get 'groups' (grps), 'taxa' (spp), and 'species' (sp_only) column mames

# grps_cols <- colnames(df_wide_groups[12:20])
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

## Summarise data ####

## In order to run ordination analysis, we need to 'aggregate' the data somehow 
## (to create a more robust 'multi-species' dataset). I'm choosing to spatially aggregate 
## the data using half-transects (as in practice we would run them as one) [done under script "02"].

df_spp_aggregated <-
  df_wide_species %>% 
  # Group-by and summarise
  dplyr::group_by(voyage, id_transect) %>% 
  dplyr::summarise(across(all_of(sp_only_cols), sum)) %>% 
  # Calculate total_birds counted within each grid, and the species richness
  dplyr::mutate(total_birds = rowSums(across(all_of(sp_only_cols))),
                sp_richness = rowSums(across(all_of(sp_only_cols), ~ . != 0) == TRUE),
                season = dplyr::case_when(
                  voyage == "voyage1" ~ "spring",
                  voyage == "voyage2" ~ "summer",
                  voyage == "voyage3" ~ "summer",
                  voyage == "voyage4" ~ "winter",
                  voyage == "voyage5" ~ "summer",
                  voyage == "voyage6" ~ "spring",
                  voyage == "voyage7" ~ "summer",
                  voyage == "voyage8" ~ "winter",
                  voyage == "voyage9" ~ "spring")) %>% 
  dplyr::mutate(season = factor(season, levels = c("summer", "autumn", "winter", "spring"))) %>% 
  dplyr::ungroup(.)

## Note: I haven't summarised aggregated 'groups', as we won't use these for ordination

### Constrained GLLVM (does 'season' affect species composition?) ####

# First, identify 'rare' species (i.e. less than 3 occurrences)
sp_rare_cols <- 
  # Get species names and number of occurrences
  data.frame(
    species = sp_only_cols,
    n_occ = apply(df_spp_aggregated[sp_only_cols], MARGIN = 2, function(x) sum(x >= 1)),
    row.names = NULL) %>%
  # Filter and pull species names
  dplyr::filter(n_occ < 3) %>%
  dplyr::pull(species)

# Get seabird data
spp_matrix <- 
  df_spp_aggregated %>%
  # Select species columns
  dplyr::select(all_of(sp_only_cols)) %>%
  # But, remove rare species columns -- they will be more noisy than explanatory
  dplyr::select(- all_of(sp_rare_cols))

### Run the model ------------------------------------------------------------- #

## "ideal" model? - as far as I understand
constrained_model <-
  gllvm::gllvm(y = spp_matrix, X = data.frame(season = df_spp_aggregated$season,
                                              voyage = df_spp_aggregated$voyage),
               lv.formula = ~ season,
               # num.lv = 2,          ## --- do we need to include this?
               num.lv.c = 2,
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 1234)

# Warning message:
#   In gllvm::gllvm(y = spp_matrix, X = data.frame(season = df_spp_aggregated$season,  :
#     Redundant predictors detected, some have been omitted as they explain similar information. 
## >>> Warning message regarding 'autumn', as we don't have data in this season

## Residuals -- look good!
# pdf(file = "./results/gllvm_constrained_residuals.pdf")
# plot(constrained_model, which = 1:4, mfrow = c(2,2))
# dev.off()

# summary(constrained_model)

## ordiplot.gllvm(constrained_model, biplot = TRUE, ind.spp = 15)
# coefplot(constrained_model)

## Get LV values

# (( Not sure if I need this ? ))
# 
# constrained_latent_vars_spp <-
#   as.data.frame(constrained_model$params$theta) %>%
#   tibble::rownames_to_column(var = "species")

## Arrange a df to plot
df_plot_constrained_model <-
  cbind((df_spp_aggregated %>% dplyr::select(season)),
        as.data.frame(gllvm::getLV.gllvm(constrained_model)))

## Plot
plot_constrained_model <- 
  ggplot(data = df_plot_constrained_model, 
         aes(x = CLV1, y = CLV2, color = season)) + ## CLV1 ...
  geom_point() +
  scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
  # coord_cartesian(ylim = c(-2,2), xlim = c(-2,2)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

ggsave(plot_constrained_model,
       filename = "./results/gllvm_constrained_biplot.pdf",
       height = 9, width = 12, units = "cm", dpi = 300)

### Variance explained by 'season' alone? Matrices traces, etc ####


### {mvabund} ??? ####


