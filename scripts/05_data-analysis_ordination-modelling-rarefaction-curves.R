## 
## Unconstrained ordinations & rarefaction curves
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##


## Libraries ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggExtra)
library(patchwork)
library(gllvm)
library(corrplot)
library(iNEXT)

# "summer" = "#4E79A7"
# "autumn" = "#F28E2B", 
# "winter" = "#E15759"
# "spring" = "#76B7B2"

## Read seabird data raw ####

df_wide_species <- 
  read.csv("./data-processed/df_wide_species_chl.csv") %>% 
  dplyr::mutate(season = factor(season, 
                                levels = c("summer", "autumn", 
                                           "winter", "spring"),
                                labels = c("Summer", "Autumn", 
                                           "Winter", "Spring")))

## Get 'taxa' (spp_) and 'species' (sp_only) column names

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
## the data using half-transects, as in practice we would run them as one sample unit. 
## ['id_transect' was done under script "02"].

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

df_chl_aggregated <-
  df_wide_species %>% 
  # Group-by and summarise CHL
  dplyr::group_by(voyage, id_transect) %>% 
  dplyr::summarise(chl_a = mean(chl_a, na.rm = TRUE))

df_spp_aggregated <- cbind(df_spp_aggregated, chl_a = df_chl_aggregated$chl_a)

rm("df_chl_aggregated")

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

## Check column names and make them nice for plots and results
# colnames(spp_matrix)

colnames(spp_matrix) <- c(
  "Buller's shearwater", "Fluttering shearwater", "Flesh-footed shearwater",
  "Grey-faced petrel", "Sooty shearwater", "Cook's/Pycroft's petrel",
  "Northern giant petrel", "Black petrel", "Wandering albatross",
  "Black-winged petrel", "Australasian gannet", "White-faced storm petrel",
  "Black-bellied storm petrel", "White-bellied storm petrel", "Diving petrel",
  "New Zealand storm petrel", "White-capped albatross", "Cape petrel",
  "Black-browed albatross", "Fairy prion"
)

### Unconstrained GLLVM, null model (purely species composition) ####

### Run NULL models --------------------------------------------------------- ##

gllvm_null_lv3 <- 
  gllvm::gllvm(y = spp_matrix, 
               num.lv = 3, 
               family = "negative.binomial",
               seed = 1234)

gllvm_null_lv2 <- 
  gllvm::gllvm(y = spp_matrix, 
               num.lv = 2, 
               family = "negative.binomial",
               seed = 1234)

gllvm_null_lv1 <- 
  gllvm::gllvm(y = spp_matrix, 
               num.lv = 1, 
               family = "negative.binomial",
               seed = 1234)

BIC(gllvm_null_lv3, gllvm_null_lv2, gllvm_null_lv1)

#                df      BIC
# gllvm_null_lv3 97 4282.522
# gllvm_null_lv2 79 4200.364
# gllvm_null_lv1 60 4139.632 ## <--- Best model

## Residuals -- look great
# pdf(file = "./results/gllvm_null_lv1_residuals.pdf")
# plot(gllvm_null_lv1, which = 1:4, mfrow = c(2,2))
# dev.off()

rm("gllvm_null_lv3", "gllvm_null_lv2")

### Ordination plot --------------------------------------------------------- ##

## Get LV values and arrange it in a dataframe to plot
df_plot_null_model <-
  cbind((df_spp_aggregated %>% dplyr::select(season, chl_a)),
        as.data.frame(gllvm::getLV.gllvm(gllvm_null_lv1)))

## Plots

## Colour-code by season
plot_gllvm_null_season <-
  ggplot(data = df_plot_null_model,
         aes(x = V1,
             y = rep(0, times = nrow(df_plot_null_model)),
             color = season)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B",
                                "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  xlab("Latent Variable 1") + ylab("") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

plot_gllvm_null_season <-
  ggExtra::ggMarginal(plot_gllvm_null_season,
                      type = "density",
                      groupColour = TRUE,
                      groupFill = TRUE)

# ggsave(plot_gllvm_null_season,
#        filename = "./results/gllvm_null_lv1_biplot_season.pdf",
#        height = 8, width = 9, units = "cm", dpi = 300)

## Facet by season, colour-code by CHL
plot_gllvm_null_chl <-
  ggplot(data = df_plot_null_model,
         aes(x = V1,
             y = rep(0, times = nrow(df_plot_null_model)),
             color = chl_a)) +
  geom_point(alpha = 0.6, size = 3) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen", 
                       name = "Chlorophyll-a concentration [mg m^-3]") + 
  xlab("Latent Variable 1") + ylab("") +
  facet_wrap(~ season, scales = "free") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

# ggsave(plot_gllvm_null_chl,
#        filename = "./results/gllvm_null_lv1_biplot_facet-season-chl.pdf",
#        height = 12, width = 14, units = "cm", dpi = 300)

rm("df_plot_null_model", "plot_gllvm_null_season", "plot_gllvm_null_chl")

### Save the model object in case needed later ------------------------------ ##

saveRDS(gllvm_null_lv1,
        file = "./results/gllvm_null_lv1_model.rds")

### Unconstrained GLLVM, accounting predictors ####

## -----------------------------------------------------------------------------#
## From {gllvm} Vignette 6 -----------------------------------------------------#
# << https://jenniniku.github.io/gllvm/articles/vignette6.html >>
# "When including predictor variables, the interpretation of the ordination would
# shift to a residual ordination, conditional on the predictors."
## [Accessed on 2 Feb 2024] ----------------------------------------------------#
## -----------------------------------------------------------------------------#

### Run models with PREDICTORS ---------------------------------------------- ##

## Note: 'chl_a' in row number '33' returned a NaN value; 
## therefore we have to remove it to run the model.

### LV == 1
gllvm_pred_lv1_season.chl <-
  gllvm::gllvm(y = spp_matrix[-33,], 
               X = data.frame(season = df_spp_aggregated$season[-33],
                              chl_a = df_spp_aggregated$chl_a[-33],
                              voyage = df_spp_aggregated$voyage[-33]),
               formula = ~ season + chl_a,
               num.lv = 1,
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 1234)

gllvm_pred_lv1_season <-
  gllvm::gllvm(y = spp_matrix[-33,], 
               X = data.frame(season = df_spp_aggregated$season[-33],
                              chl_a = df_spp_aggregated$chl_a[-33],
                              voyage = df_spp_aggregated$voyage[-33]),
               formula = ~ season,
               num.lv = 1,
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 1234)

gllvm_pred_lv1_chl <-
  gllvm::gllvm(y = spp_matrix[-33,], 
               X = data.frame(season = df_spp_aggregated$season[-33],
                              chl_a = df_spp_aggregated$chl_a[-33],
                              voyage = df_spp_aggregated$voyage[-33]),
               formula = ~ chl_a,
               num.lv = 1,
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 1234)

###LV == 0
gllvm_pred_lv0_season.chl <-
  gllvm::gllvm(y = spp_matrix[-33,], 
               X = data.frame(season = df_spp_aggregated$season[-33],
                              chl_a = df_spp_aggregated$chl_a[-33],
                              voyage = df_spp_aggregated$voyage[-33]),
               formula = ~ season + chl_a,
               num.lv = 0,
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 1234)

gllvm_pred_lv0_season <-
  gllvm::gllvm(y = spp_matrix[-33,], 
               X = data.frame(season = df_spp_aggregated$season[-33],
                              chl_a = df_spp_aggregated$chl_a[-33],
                              voyage = df_spp_aggregated$voyage[-33]),
               formula = ~ season,
               num.lv = 0,
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 1234)

gllvm_pred_lv0_chl <-
  gllvm::gllvm(y = spp_matrix[-33,], 
               X = data.frame(season = df_spp_aggregated$season[-33],
                              chl_a = df_spp_aggregated$chl_a[-33],
                              voyage = df_spp_aggregated$voyage[-33]),
               formula = ~ chl_a,
               num.lv = 0,
               family = "negative.binomial",
               row.eff = ~(1|voyage),
               seed = 1234)

BIC(gllvm_pred_lv1_season.chl,
    gllvm_pred_lv1_season,
    gllvm_pred_lv1_chl,
    gllvm_pred_lv0_season.chl,
    gllvm_pred_lv0_season,
    gllvm_pred_lv0_chl)

#                            df      BIC
# gllvm_pred_lv1_season.chl 141 4026.795
# gllvm_pred_lv1_season     121 3993.811
# gllvm_pred_lv1_chl         81 4114.641
# gllvm_pred_lv0_season.chl 121 3935.717
# gllvm_pred_lv0_season     101 3902.735 ## <--- Best model
# gllvm_pred_lv0_chl         61 4105.945

## Residuals -- look great
# pdf(file = "./results/gllvm_pred_lv0_season_residuals.pdf")
# plot(gllvm_pred_lv0_season, which = 1:4, mfrow = c(2,2))
# dev.off()

rm("gllvm_pred_lv1_season.chl", 
   #"gllvm_pred_lv1_season",
   "gllvm_pred_lv1_chl", "gllvm_pred_lv0_season.chl", "gllvm_pred_lv0_chl")

### Save the model object in case needed later ------------------------------ ##

saveRDS(gllvm_pred_lv0_season,
        file = "./results/gllvm_pred_lv0_season_model.rds")

### Coefficient plot -------------------------------------------------------- ##

### As there is no LV, there is no way of making an 'ordination plot'.
### However, we can check the effects of 'season' on each species using coefficient plots

### Plot 'season' effect based on 'gllvm_pred_lv0_season' model ------------- ##

# pdf(file = "./results/gllvm_pred_lv0_coefplot-season.pdf",
#     height = 6, width = 10)
# # Graphical parameters
# par(mfrow = c(1, 3),
#     mar = c(0.1, 0.1, 0.1, 0.1),
#     oma = c(1, 6.5, 1, 1))
# 
# gllvm::coefplot(gllvm_pred_lv0_season,
#                 order = FALSE,
#                 cex.ylab = 1.1,
#                 cex.lab = 0.0001,
#                 which.Xcoef = 1 # Autumn
#                 )
# title(xlab = "Autumn", cex.lab = 1.4)
# 
# gllvm::coefplot(gllvm_pred_lv0_season,
#                 order = FALSE,
#                 cex.ylab = 0.001,
#                 cex.lab = 0.0001,
#                 which.Xcoef = 2 # Winter
#                 )
# title(xlab = "Winter", cex.lab = 1.4)
# 
# gllvm::coefplot(gllvm_pred_lv0_season,
#                 order = FALSE,
#                 cex.ylab = 0.001,
#                 cex.lab = 0.0001,
#                 which.Xcoef = 3 # Spring
#                 )
# title(xlab = "Spring", cex.lab = 1.4)
# dev.off()

### Adjust 'xlim' to make the plot readable --------------------------------- ##

pdf(file = "./results/gllvm_pred_lv0_coefplot-season_xlim-adjusted.pdf", 
    height = 6, width = 10)
# Graphical parameters
par(mfrow = c(1, 3), 
    mar = c(0.1, 0.1, 0.1, 0.1), 
    oma = c(1, 6.5, 1, 1))

gllvm::coefplot(gllvm_pred_lv0_season,
                order = FALSE,
                cex.ylab = 1.1,
                cex.lab = 0.0001,
                which.Xcoef = 1, # Autumn
                xlim.list = list(c(-15, 15))
                )
title(xlab = "Autumn", cex.lab = 1.4)

gllvm::coefplot(gllvm_pred_lv0_season,
                order = FALSE,
                cex.ylab = 0.001,
                cex.lab = 0.0001,
                which.Xcoef = 2, # Winter
                xlim.list = list(c(-15, 15))
                )
title(xlab = "Winter", cex.lab = 1.4)

gllvm::coefplot(gllvm_pred_lv0_season,
                order = FALSE,
                cex.ylab = 0.001,
                cex.lab = 0.0001,
                which.Xcoef = 3, # Spring
                xlim.list = list(c(-15, 15))
                )
title(xlab = "Spring", cex.lab = 1.4)
dev.off()

### After 'coefplot', re-set graphical parameters back to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

### Co-occurrence patterns & Variation explained (caution! -- read 'NOTES') ####

## From {gllvm} Vignette 1 -----------------------------------------------------#
# << https://jenniniku.github.io/gllvm/articles/vignette1.html#studying-co-occurrence-patterns >>
## [Accessed on 2 Feb 2024] ----------------------------------------------------#

### Visualise co-occurrence patterns without/with accounting for predictors

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# NOTE: 
# The best model accounting for predictors retained ZERO latent variables.
# As such, we cannot get the residual species-correlation matrix, as these values
# are given by the latent variable(s). 
# 
# The selected model with zero latent variables indicates that most of the variability 
# in the data was explained by the predictor (season) and therefore there is no
# need for including a latent variable.
#
# Thus, on purpose, for the next plot I used the model with ONE latent variable 
# plus the same retained predictor (season) as the best selected model. 
# By doing so, it allowed to get the residual species-correlation matrix and running the plot.
# 
# Not surprisingly, and reinforcing no need for latent variables, you will see
# that the correlation between species basically collapses to zero when accounting 
# for their seasonality.
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

## Get residual species-correlation matrix for each model
residuals_corr_null <- gllvm::getResidualCor(gllvm_null_lv1)
residuals_corr_pred <- gllvm::getResidualCor(gllvm_pred_lv1_season)

### Plot co-occurrence patterns (residual correlation between species) ------ ##

pdf(file = "./results/gllvm_co-occurrence_residual-correlation-matrices.pdf", 
    height = 4, width = 12)
par(mfrow = c(1,2))

# GLLVM Null
corrplot::corrplot(residuals_corr_null, diag = FALSE, type = "lower", 
                   method = "square", tl.srt = 25, tl.col = "grey35", tl.cex = 0.8, 
                   col = COL2('PuOr'), cl.cex = 0.68)

# GLLVM with predictor (season)
corrplot::corrplot(residuals_corr_pred, diag = FALSE, type = "lower", 
                   method = "square", tl.srt = 25, tl.col = "grey35", tl.cex = 0.8,
                   col = COL2('PuOr'), cl.cex = 0.68)
dev.off()

# After 'correlation' plots we need to re-set graphical parameters back to default
par(mfrow = c(1, 1))

### How much of the variation of the data did the predictor accounted for? -- ##

# ## Get residual covariance matrix for each model
# residuals_cov_null <- gllvm::getResidualCov(gllvm_null_lv1)
# residuals_cov_pred_lv1_season <- gllvm::getResidualCov(gllvm_pred_lv1_season)
# 
# (1 - residuals_cov_pred_lv1_season$trace / residuals_cov_null$trace) * 100
# ## >> 37.7258

## Note, however, that again I used the 'gllvm_pred_lv1_season' model which includes
## one latent variable. This calculations can get tricky with latent variables, so this
## numbers are not to trust blindly. Nonetheless, it suggests ~40% of the variability
## in the data was explained by season alone -- which is very high value for a single predictor.

rm("residuals_corr_null", "residuals_corr_pred", "residuals_cov_null", "residuals_cov_pred_lv1_season")

### Comparing predictions between GLLVMs accounting for season with(out) LVs and raw data ####

## Get predicted/expected values for unconstrained model *without* LVs (lv0)

fitmod_lv0 <- data.frame(
  exp(
    predict(gllvm_pred_lv0_season, newX = data.frame(season = df_spp_aggregated$season))
    )
  )

fitmod_lv0 <- fitmod_lv0[order(df_spp_aggregated$season), ]

fitlong_lv0 <- 
  tidyr::gather(data.frame(site = 1:nrow(fitmod_lv0), fitmod_lv0), 
                key = "Species", value = "Number", 
                Buller.s.shearwater:Fairy.prion)

fitlong_lv0 <-
  cbind(fitlong_lv0, Source = rep("LV = 0", times = nrow(fitlong_lv0)))

## Get predicted/expected values for unconstrained model *with* LVs (lv2)

fitmod_lv1 <- data.frame(
  exp(
    predict(gllvm_pred_lv1_season, newX = data.frame(season = df_spp_aggregated$season),
            level = 0)
  )
)

fitmod_lv1 <- fitmod_lv1[order(df_spp_aggregated$season), ]

fitlong_lv1 <- 
  tidyr::gather(data.frame(site = 1:nrow(fitmod_lv1), fitmod_lv1), 
                key = "Species", value = "Number", 
                Buller.s.shearwater:Fairy.prion)

fitlong_lv1 <-
  cbind(fitlong_lv1, Source = rep("LV = 1", times = nrow(fitlong_lv1)))

rm("fitmod_lv1")

## Reshape raw data to the same format
yord <- spp_matrix[order(df_spp_aggregated$season), ]

ylong <- 
  tidyr::gather(data.frame(site = 1:nrow(fitmod_lv0), yord), 
                key = "Species", value = "Number", 
                Buller.s.shearwater:Fairy.prion)

ylong <-
  cbind(ylong, Source = rep("Raw data", times = nrow(ylong)))

rm("yord", "fitmod_lv0")

## Bind dataframes
df_lv0_lv1_raw <- rbind(fitlong_lv0, fitlong_lv1, ylong)

rm("fitlong_lv0", "fitlong_lv1", "ylong")

## Compare results through a plot

plot_comparing_lv0_lv1_raw <-
  ggplot(data = df_lv0_lv1_raw, aes(x = site, y = Number, colour = Source, shape = Source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#000000", "#F8766D", "#00BA38")) +
  facet_wrap(~ Species, scales = "free_y") + 
  ylab("Count") + xlab("Sample") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6))

# ggsave(plot_comparing_lv0_lv1_raw,
#        filename = "./results/comparing_lv0_lv1_raw.pdf",
#        height = 15, width = 20, units = "cm", dpi = 300)

rm("df_lv0_lv1_raw", "plot_comparing_lv0_lv1_raw")

### {iNEXT} - rarefaction curves ####

df_inext <- 
  # Note: using *10-min* counts as the unit (*not* the 'aggregated' dataset)
  df_wide_species %>%
  dplyr::select(season, all_of(sp_only_cols))

df_inext[sp_only_cols] <- apply(df_inext[sp_only_cols], 2, function(x) replace(x, x >= 1, 1))

list_inext <- 
  list("Summer" = data.frame(t(df_inext[df_inext$season == "Summer", 2:ncol(df_inext)])), 
       "Autumn" = data.frame(t(df_inext[df_inext$season == "Autumn", 2:ncol(df_inext)])),
       "Winter" = data.frame(t(df_inext[df_inext$season == "Winter", 2:ncol(df_inext)])),
       "Spring" = data.frame(t(df_inext[df_inext$season == "Spring", 2:ncol(df_inext)])))


## iNEXT function [q = 0 means spp richness itself]
inext_obj <- iNEXT::iNEXT(list_inext, q = c(0), datatype = "incidence_raw") ## q = c(0,1,2)

# Sample-size-based R/E curve
curve_sample_size_based <- 
  iNEXT::ggiNEXT(inext_obj, type = 1, se = TRUE) +  ## facet.var="Order.q"
  scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                                "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  coord_cartesian(ylim = c(0, 35)) +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

# Coverage-based R/E curves
curve_coverage_based <- 
  iNEXT::ggiNEXT(inext_obj, type = 3, se = TRUE) + 
  scale_color_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                                "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  scale_fill_manual(values = c("Summer" = "#4E79A7", "Autumn" = "#F28E2B", 
                               "Winter" = "#E15759", "Spring" = "#76B7B2")) +
  coord_cartesian(ylim = c(0, 35)) +
  ylab("") +
  theme_bw() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

# Sample completeness curve
# curve_sample_completeness <- iNEXT::ggiNEXT(inext_obj, type = 2, se = TRUE)

inext_plot <- 
  curve_sample_size_based + curve_coverage_based + 
  patchwork::plot_annotation(tag_levels = 'A')

# ggsave(inext_plot, 
#        filename = "./results/iNEXT.pdf", 
#        height = 8, width = 16, units = "cm", dpi = 300)

rm("df_inext", "list_inext", "inext_obj", 
   "curve_sample_size_based", "curve_coverage_based", "inext_plot")
