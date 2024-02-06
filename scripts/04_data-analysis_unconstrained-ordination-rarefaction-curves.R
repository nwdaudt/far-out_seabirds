## 
## Unconstrained ordination & rarefaction curves
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##


## Libraries ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(gllvm)
library(corrplot)
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
  dplyr::mutate(season = factor(season, levels = c("summer", 
                                                   # "autumn", 
                                                   "winter", "spring")))

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
  dplyr::mutate(season = factor(season, levels = c("summer", 
                                                   # "autumn", 
                                                   "winter", "spring"))) %>% 
  dplyr::ungroup(.)

## Note: I haven't summarised aggregated 'groups', as we won't use these for ordination

## Prep for modelling ####

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

### Unconstrained GLLVM (purely species composition) ####

### Run the model
unconstrained_model <-
  gllvm::gllvm(y = spp_matrix, 
               num.lv = 2, 
               family = "negative.binomial",
               seed = 1234)

## Residuals -- look good!
# pdf(file = "./results/gllvm_unconstrained_residuals.pdf")
# plot(unconstrained_model, which = 1:4, mfrow = c(2,2))
# dev.off()

# summary(unconstrained_model)
## ordiplot.gllvm(unconstrained_model, biplot = TRUE, ind.spp = 10)

## Get LV values and arrange it in a dataframe to plot
df_plot_unconstrained_model <-
  cbind((df_spp_aggregated %>% dplyr::select(season)),
        as.data.frame(gllvm::getLV.gllvm(unconstrained_model)))

## Plot
plot_unconstrained_model <- 
  ggplot(data = df_plot_unconstrained_model, 
         aes(x = LV1, y = LV2, color = season)) +
  geom_point() + 
  scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
  coord_cartesian(ylim = c(-2,2), xlim = c(-2,2)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

ggsave(plot_unconstrained_model,
       filename = "./results/gllvm_unconstrained_biplot.pdf",
       height = 9, width = 12, units = "cm", dpi = 300)

saveRDS(unconstrained_model, 
        file = "./results/gllvm_unconstrained_model.rds")

### Unconstrained GLLVM (accounting for 'season') ####

## From {gllvm} Vignette 6 -----------------------------------------------------#
# << https://jenniniku.github.io/gllvm/articles/vignette6.html >>
# "When including predictor variables, the interpretation of the ordination would
# shift to a residual ordination, conditional on the predictors."
## [Accessed on 2 Feb 2024] ----------------------------------------------------#

unconstrained_model_season <-
  gllvm::gllvm(y = spp_matrix, 
               X = data.frame(season = df_spp_aggregated$season,
                              voyage = df_spp_aggregated$voyage),
               formula = ~ season,
               num.lv = 2,
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 1234)

## Residuals -- look good!
# pdf(file = "./results/gllvm_unconstrained_season_residuals.pdf")
# plot(unconstrained_model_season, which = 1:4, mfrow = c(2,2))
# dev.off()

# summary(unconstrained_model_season)
## ordiplot.gllvm(unconstrained_model_season, biplot = TRUE, ind.spp = 10)

## Get LV values and arrange it in a dataframe to plot
df_plot_unconstrained_season_model <-
  cbind((df_spp_aggregated %>% dplyr::select(season)),
        as.data.frame(gllvm::getLV.gllvm(unconstrained_model_season)))

## Plot
plot_unconstrained_season_model <- 
  ggplot(data = df_plot_unconstrained_season_model, 
         aes(x = LV1, y = LV2, color = season)) +
  geom_point() + 
  scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
  theme_bw() + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

ggsave(plot_unconstrained_season_model,
       filename = "./results/gllvm_unconstrained_season_biplot.pdf",
       height = 9, width = 12, units = "cm", dpi = 300)

saveRDS(unconstrained_model_season, 
        file = "./results/gllvm_unconstrained_season_model.rds")

## Season effect on species -- gllvm::coefplot ####

pdf(file = "./results/gllvm_coefplot-season.pdf", height = 7, width = 11)
# Graphical parameters
par(mfrow = c(1, 2), 
    mar = c(0.1, 0.1, 0.1, 0.1), 
    oma = c(1, 6.5, 1, 1))

gllvm::coefplot(unconstrained_model_season,
                order = FALSE,
                cex.ylab = 1,
                which.Xcoef = 1 # Winter
                )

gllvm::coefplot(unconstrained_model_season,
                order = FALSE,
                cex.ylab = 0.001,
                which.Xcoef = 2 # Spring
                )
dev.off()

# After 'coefplot' we need to re-set graphical parameters back to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

### Co-occurrence patterns & Variation explained ####

## From {gllvm} Vignette 1 -----------------------------------------------------#
# << https://jenniniku.github.io/gllvm/articles/vignette1.html#studying-co-occurrence-patterns >>
## [Accessed on 2 Feb 2024] ----------------------------------------------------#

## --- Visualise co-occurrence patterns with, and without accounting for predictors --- ##

## Get residual correlation matrix for each model
residuals_corr_unconstrained <- gllvm::getResidualCor(unconstrained_model)
residuals_corr_unconstrained_season <- gllvm::getResidualCor(unconstrained_model_season)

## Plot co-occurrence patterns (residual correlation between species)
pdf(file = "./results/gllvm_residual-correlation-matrices_co-occurrence.pdf", height = 4, width = 12)
par(mfrow = c(1,2))
# unconstrained without predictors
corrplot::corrplot(residuals_corr_unconstrained, diag = FALSE, type = "lower", 
                   method = "square", tl.srt = 25, tl.col = "grey35", tl.cex = 0.8, 
                   col = COL2('PuOr'), cl.cex = 0.68)
# unconstrained with predictors (season)
corrplot::corrplot(residuals_corr_unconstrained_season, diag = FALSE, type = "lower", 
                   method = "square", tl.srt = 25, tl.col = "grey35", tl.cex = 0.8,
                   col = COL2('PuOr'), cl.cex = 0.68)
dev.off()

# After 'correlation' plots we need to re-set graphical parameters back to default
par(mfrow = c(1, 1))

## --- How much of the variation of the data did the predictor accounted for? --- ##

## Get residual covariance matrix for each model
residuals_cov_unconstrained <- gllvm::getResidualCov(unconstrained_model)
residuals_cov_unconstrained_season <- gllvm::getResidualCov(unconstrained_model_season)

(1 - residuals_cov_unconstrained_season$trace / residuals_cov_unconstrained$trace) * 100
## >> 45.39177

### {iNEXT} - rarefaction curves ####

df_inext <- 
  # Note: using 10-min counts as the unit (not the 'aggregated' dataset)
  df_wide_species %>%
  dplyr::select(season, all_of(sp_only_cols))

df_inext[sp_only_cols] <- apply(df_inext[sp_only_cols], 2, function(x) replace(x, x >= 1, 1))

list_inext <- 
  list("spring" = data.frame(t(df_inext[df_inext$season == "spring", 2:ncol(df_inext)])), 
       "summer" = data.frame(t(df_inext[df_inext$season == "summer", 2:ncol(df_inext)])), 
       "winter" = data.frame(t(df_inext[df_inext$season == "winter", 2:ncol(df_inext)])))


## iNEXT function [q = 0 means spp richness itself]
inext_obj <- iNEXT::iNEXT(list_inext, q = c(0), datatype = "incidence_raw") ## q = c(0,1,2)

# Sample-size-based R/E curve
curve_sample_size_based <- 
  iNEXT::ggiNEXT(inext_obj, type = 1, se = TRUE) +  ## facet.var="Order.q"
  scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
  scale_fill_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
  coord_cartesian(ylim = c(0, 35)) +
  theme_bw() + 
  theme(legend.position = "none")

# Coverage-based R/E curves
curve_coverage_based <- 
  iNEXT::ggiNEXT(inext_obj, type = 3, se = TRUE) + 
  scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
  scale_fill_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
  coord_cartesian(ylim = c(0, 35)) +
  ylab("") +
  theme_bw()

# Sample completeness curve
# curve_sample_completeness <- ggiNEXT(inext_obj, type = 2, se = TRUE)

inext_plot <- curve_sample_size_based + curve_coverage_based

ggsave(inext_plot, 
       filename = "./results/iNEXT.pdf", 
       height = 8 , width = 15, units = "cm", dpi = 300)
