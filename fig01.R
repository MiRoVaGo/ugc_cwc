library(data.table)
library(ggplot2)
library(sf)
library(ggpubr)
library(scales)
library(ggpattern)

base <- readRDS('data/elevation.rds')
cz <- st_read("data/gadm41_CZE_0.shp")
basins <- st_read('data/wmobb_basins.shp')
danube <- basins[basins$WMOBB == 642 | basins$WMOBB == 643 | basins$WMOBB == 635, ]
danube <- st_union(danube)
elbe <- basins[basins$WMOBB == 640, ]
oder <- basins[basins$WMOBB == 657 | basins$WMOBB == 658, ]



ggplot(base) +
  geom_raster(aes(x = lon, y = lat, fill = z)) +
  scale_fill_gradientn(colors = c('#307424', '#F0CF9F', '#BD8D15', '#9B8411',
                                  '#7E4B11', '#864747', '#FBFBFB')) +
  geom_sf_pattern(data = danube, fill = NA, color = 'black',
                  linewidth = 2, pattern = "stripe", pattern_fill = "black") +
  geom_sf(data = elbe, fill = alpha("gray",0.5), color = 'gray', linewidth = 2) +
  #geom_sf(data = oder, fill = alpha("gray",0.4), color = NA) +
  geom_sf(data = cz, fill = NA, color = "red", linewidth = 1) +
  annotate('text', x = 13.5, y = 49.15, label = 'Šumava Mt', angle = -45,
           fontface = 2) +
  annotate('text', x = 15.55, y = 50.65, label = 'Sudetic Mt', angle = -30,
           fontface = 2) +
  annotate('text', x = 13.2, y = 50.4, label = 'Ore Mt', angle = 35,
           fontface = 2) +
  coord_sf(expand = FALSE, xlim = c(11.5, 19), ylim = c(48, 51.5)) +
  labs(x = NULL, y = NULL, fill = 'Elevation\nin [m]') +
  theme(axis.text = element_text(size = 16), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
        axis.ticks.length = unit(-0.25, "cm"))

ggsave('fig01.pdf', width = 8, height = 4.5, dpi = 600, device = cairo_pdf)
