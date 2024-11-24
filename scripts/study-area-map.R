



## Libraries ####

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggspatial)


nz_polygon <- sf::read_sf("./data-spatial/nz/nz-coastlines-and-islands-polygons-topo-150k.gpkg")

isobaths <- 
  sf::read_sf("./data-spatial/nz/nz_isobath_ne_north_island.gpkg") %>% 
  dplyr::filter(DEPTH %in% c(50, 200, 500, 1000)) %>% 
  sf::st_transform(4326)

# plot(isobaths[,c(2,6)])

transects_polygon <- sf::read_sf("./data-spatial/transects/far-out_transects-3km-buffer-polygon.gpkg")
# plot(transects_polygon)

transects <- sf::read_sf("./data-spatial/transects/far-out_segmented-transects.gpkg")
# plot(transects)

### MAPS ####

nz_base_map <- 
  ggplot(data = nz_polygon) + 
  geom_sf(color = "black", fill = "black") + 
  coord_sf(xlim = c(166.2, 178.7), ylim = c(-34, -47.3)) +
  geom_rect(data = nz_polygon,
            aes(xmin = 172, xmax = 173.8, ymin = -35, ymax = -34),
            colour = "red", fill = "NA", linewidth = 1.2) +
  theme_void()

ggsave(nz_base_map,
       filename = "./results/EDA/study-area/study-area-map_NZ.pdf",
       height = 6, width = 4.5, dpi = 300)

northland_base_map <- 
  ggplot(data = nz_polygon) + 
  geom_sf(color = "black", fill = "wheat2") +
  coord_sf(xlim = c(172.6, 173.8), ylim = c(-35, -34.2)) + 
  theme_bw() +
  ggspatial::annotation_scale(location = "bl",
                              height = unit(0.4, "cm"),
                              text_cex = 1.3) +
  ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering(),
                                    location = "bl", 
                                    pad_x = unit(0.25, "cm"), pad_y = unit(0.58, "cm"),
                                    height = unit(2, "cm"), width = unit(2, "cm"))

farout_systematic.transects <-
  northland_base_map + 
  ## Add isobaths
  geom_sf(data = isobaths, 
          aes(linetype  = as.factor(DEPTH)), colour = "grey50") +
  scale_linetype_manual(values = c("dotted", "longdash", "solid", "twodash"), name = "") +
  ## Add study area polygon
  geom_sf(data = transects_polygon, colour = "black", fill = NA, linewidth = 1.2) +
  ## Add transects
  geom_sf(data = transects, aes(color = as.factor(id)), linewidth = 3.5) +
  scale_color_brewer(palette = "Paired", guide = NULL) +
  coord_sf(xlim = c(172, 173.8), ylim = c(-35, -34)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14))

ggsave(farout_systematic.transects,
       filename = "./results/EDA/study-area/study-area-map_farout-systematic-transects.pdf",
       height = 7.5, width = 10, dpi = 300)

  