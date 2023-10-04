library(data.table)
library(ggplot2)
library(dplyr)
library(sf)
library(ggpubr)
library(scales)
library(raster)
library(pRecipe)

# Basins
basins <- st_read('data/wmobb_basins.shp')
danube <- basins[basins$WMOBB == 642 | basins$WMOBB == 643 | basins$WMOBB == 635, ]
danube <- st_union(danube) %>% st_sf() %>% as("Spatial")
dummie_terra <- raster(crs = crs(danube), vals = 1,
                         resolution = c(0.25/6, 0.25/6)) %>%
  rasterize(danube, .)
dummie_mhm <- raster(crs = crs(danube), vals = 1,
                     resolution = c(0.25/2, 0.25/2)) %>%
  rasterize(danube, .)
dummie_era <- raster(crs = crs(danube), vals = 1,
                     resolution = c(0.1, 0.1)) %>%
  rasterize(danube, .)

danube_terra <- tabular(dummie_terra)
danube_mhm <- tabular(dummie_mhm)
danube_era <- tabular(dummie_era)
danube_era$lon <- danube_era$lon + 0.05
danube_era$lat <- danube_era$lat + 0.05

elbe <- basins[basins$WMOBB == 640, ] %>% st_geometry() %>% as("Spatial")
dummie_terra <- raster(crs = crs(elbe), vals = 1,
                       resolution = c(0.25/6, 0.25/6)) %>%
  rasterize(elbe, .)
dummie_mhm <- raster(crs = crs(elbe), vals = 1,
                     resolution = c(0.25/2, 0.25/2)) %>%
  rasterize(elbe, .)
dummie_era <- raster(crs = crs(elbe), vals = 1,
                     resolution = c(0.1, 0.1)) %>%
  rasterize(elbe, .)

elbe_terra <- tabular(dummie_terra)
elbe_mhm <- tabular(dummie_mhm)
elbe_era <- tabular(dummie_era)
elbe_era$lon <- elbe_era$lon + 0.05
elbe_era$lat <- elbe_era$lat + 0.05

# Data tp
## Terra
terraclimate <- readRDS("data/tp/terra_ppt_cropped.rds")
setnames(terraclimate, c("lon", "lat", "date", "value"))
terraclimate[danube_terra[, .(lon, lat)], basin := "Morava", on = .(lon, lat)]
terraclimate[elbe_terra[, .(lon, lat)], basin := "Labe", on = .(lon, lat)]
terraclimate[is.na(basin), basin := "Odra"]

terra_czechia <- fldmean(terraclimate[,.(lon, lat, date, value)])
terra_czechia[, date := year(date)]
terra_czechia <- terra_czechia[, .(value = sum(value)), .(date)]
terra_czechia$domain <- "Czechia"
terra_danube <- fldmean(terraclimate[basin == "Morava", .(lon, lat, date, value)])
terra_danube[, date := year(date)]
terra_danube <- terra_danube[, .(value = sum(value)), .(date)]
terra_danube$domain <- "Morava"
terra_elbe <- fldmean(terraclimate[basin == "Labe", .(lon, lat, date, value)])
terra_elbe[, date := year(date)]
terra_elbe <- terra_elbe[, .(value = sum(value)), .(date)]
terra_elbe$domain <- "Labe"
terra_oder <- fldmean(terraclimate[basin == "Odra", .(lon, lat, date, value)])
terra_oder[, date := year(date)]
terra_oder <- terra_oder[, .(value = sum(value)), .(date)]
terra_oder$domain <- "Odra"

terraclimate <- rbind(terra_czechia, terra_danube, terra_elbe, terra_oder)

## mHM
mhm <- readRDS("data/tp/mhm_tp_cropped.rds")
setnames(mhm, c("lon", "lat", "date", "value"))
mhm[danube_mhm[, .(lon, lat)], basin := "Morava", on = .(lon, lat)]
mhm[elbe_mhm[, .(lon, lat)], basin := "Labe", on = .(lon, lat)]
mhm[is.na(basin), basin := "Odra"]

mhm_czechia <- fldmean(mhm[,.(lon, lat, date, value)])
mhm_czechia[, date := year(date)]
mhm_czechia <- mhm_czechia[, .(value = sum(value)), .(date)]
mhm_czechia$domain <- "Czechia"
mhm_danube <- fldmean(mhm[basin == "Morava", .(lon, lat, date, value)])
mhm_danube[, date := year(date)]
mhm_danube <- mhm_danube[, .(value = sum(value)), .(date)]
mhm_danube$domain <- "Morava"
mhm_elbe <- fldmean(mhm[basin == "Labe", .(lon, lat, date, value)])
mhm_elbe[, date := year(date)]
mhm_elbe <- mhm_elbe[, .(value = sum(value)), .(date)]
mhm_elbe$domain <- "Labe"
mhm_oder <- fldmean(mhm[basin == "Odra", .(lon, lat, date, value)])
mhm_oder[, date := year(date)]
mhm_oder <- mhm_oder[, .(value = sum(value)), .(date)]
mhm_oder$domain <- "Odra"

mhm <- rbind(mhm_czechia, mhm_danube, mhm_elbe, mhm_oder)

## ERA5
era5 <- readRDS("data/tp/era5_tp_cropped.rds")
setnames(era5, c("lon", "lat", "date", "value"))
era5[danube_era[, .(lon, lat)], basin := "Morava", on = .(lon, lat)]
era5[elbe_era[, .(lon, lat)], basin := "Labe", on = .(lon, lat)]
era5[is.na(basin), basin := "Odra"]

era_czechia <- fldmean(era5[,.(lon, lat, date, value)])
era_czechia[, date := year(date)]
era_czechia <- era_czechia[, .(value = sum(value)), .(date)]
era_czechia$domain <- "Czechia"
era_danube <- fldmean(era5[basin == "Morava", .(lon, lat, date, value)])
era_danube[, date := year(date)]
era_danube <- era_danube[, .(value = sum(value)), .(date)]
era_danube$domain <- "Morava"
era_elbe <- fldmean(era5[basin == "Labe", .(lon, lat, date, value)])
era_elbe[, date := year(date)]
era_elbe <- era_elbe[, .(value = sum(value)), .(date)]
era_elbe$domain <- "Labe"
era_oder <- fldmean(era5[basin == "Odra", .(lon, lat, date, value)])
era_oder[, date := year(date)]
era_oder <- era_oder[, .(value = sum(value)), .(date)]
era_oder$domain <- "Odra"

era5 <- rbind(era_czechia, era_danube, era_elbe, era_oder)

era5$name <- "ERA5-Land"
mhm$name <- "mHM"
terraclimate$name <- "TerraClimate"

tp_all <- rbind(terraclimate, mhm, era5)
setnames(tp_all, "value", "P")

# Data et
## Terra
terraclimate <- readRDS("data/e/terra_aet_cropped.rds")
setnames(terraclimate, c("lon", "lat", "date", "value"))
terraclimate[danube_terra[, .(lon, lat)], basin := "Morava", on = .(lon, lat)]
terraclimate[elbe_terra[, .(lon, lat)], basin := "Labe", on = .(lon, lat)]
terraclimate[is.na(basin), basin := "Odra"]

terra_czechia <- fldmean(terraclimate[,.(lon, lat, date, value)])
terra_czechia[, date := year(date)]
terra_czechia <- terra_czechia[, .(value = sum(value)), .(date)]
terra_czechia$domain <- "Czechia"
terra_danube <- fldmean(terraclimate[basin == "Morava", .(lon, lat, date, value)])
terra_danube[, date := year(date)]
terra_danube <- terra_danube[, .(value = sum(value)), .(date)]
terra_danube$domain <- "Morava"
terra_elbe <- fldmean(terraclimate[basin == "Labe", .(lon, lat, date, value)])
terra_elbe[, date := year(date)]
terra_elbe <- terra_elbe[, .(value = sum(value)), .(date)]
terra_elbe$domain <- "Labe"
terra_oder <- fldmean(terraclimate[basin == "Odra", .(lon, lat, date, value)])
terra_oder[, date := year(date)]
terra_oder <- terra_oder[, .(value = sum(value)), .(date)]
terra_oder$domain <- "Odra"

terraclimate <- rbind(terra_czechia, terra_danube, terra_elbe, terra_oder)

## mHM
mhm <- readRDS("data/e/mhm_e_cropped.rds")
setnames(mhm, c("lon", "lat", "date", "value"))
mhm[danube_mhm[, .(lon, lat)], basin := "Morava", on = .(lon, lat)]
mhm[elbe_mhm[, .(lon, lat)], basin := "Labe", on = .(lon, lat)]
mhm[is.na(basin), basin := "Odra"]

mhm_czechia <- fldmean(mhm[,.(lon, lat, date, value)])
mhm_czechia[, date := year(date)]
mhm_czechia <- mhm_czechia[, .(value = sum(value)), .(date)]
mhm_czechia$domain <- "Czechia"
mhm_danube <- fldmean(mhm[basin == "Morava", .(lon, lat, date, value)])
mhm_danube[, date := year(date)]
mhm_danube <- mhm_danube[, .(value = sum(value)), .(date)]
mhm_danube$domain <- "Morava"
mhm_elbe <- fldmean(mhm[basin == "Labe", .(lon, lat, date, value)])
mhm_elbe[, date := year(date)]
mhm_elbe <- mhm_elbe[, .(value = sum(value)), .(date)]
mhm_elbe$domain <- "Labe"
mhm_oder <- fldmean(mhm[basin == "Odra", .(lon, lat, date, value)])
mhm_oder[, date := year(date)]
mhm_oder <- mhm_oder[, .(value = sum(value)), .(date)]
mhm_oder$domain <- "Odra"

mhm <- rbind(mhm_czechia, mhm_danube, mhm_elbe, mhm_oder)

## ERA5
era5 <- readRDS("data/e/era5_e_cropped.rds")
setnames(era5, c("lon", "lat", "date", "value"))
era5[danube_era[, .(lon, lat)], basin := "Morava", on = .(lon, lat)]
era5[elbe_era[, .(lon, lat)], basin := "Labe", on = .(lon, lat)]
era5[is.na(basin), basin := "Odra"]

era_czechia <- fldmean(era5[,.(lon, lat, date, value)])
era_czechia[, date := year(date)]
era_czechia <- era_czechia[, .(value = sum(value)), .(date)]
era_czechia$domain <- "Czechia"
era_danube <- fldmean(era5[basin == "Morava", .(lon, lat, date, value)])
era_danube[, date := year(date)]
era_danube <- era_danube[, .(value = sum(value)), .(date)]
era_danube$domain <- "Morava"
era_elbe <- fldmean(era5[basin == "Labe", .(lon, lat, date, value)])
era_elbe[, date := year(date)]
era_elbe <- era_elbe[, .(value = sum(value)), .(date)]
era_elbe$domain <- "Labe"
era_oder <- fldmean(era5[basin == "Odra", .(lon, lat, date, value)])
era_oder[, date := year(date)]
era_oder <- era_oder[, .(value = sum(value)), .(date)]
era_oder$domain <- "Odra"

era5 <- rbind(era_czechia, era_danube, era_elbe, era_oder)

era5$name <- "ERA5-Land"
mhm$name <- "mHM"
terraclimate$name <- "TerraClimate"

et_all <- rbind(terraclimate, mhm, era5)
setnames(et_all, "value", "E")

# Data ro
## Terra
terraclimate <- readRDS("data/ro/terra_q_cropped.rds")
setnames(terraclimate, c("lon", "lat", "date", "value"))
terraclimate[danube_terra[, .(lon, lat)], basin := "Morava", on = .(lon, lat)]
terraclimate[elbe_terra[, .(lon, lat)], basin := "Labe", on = .(lon, lat)]
terraclimate[is.na(basin), basin := "Odra"]

terra_czechia <- fldmean(terraclimate[,.(lon, lat, date, value)])
terra_czechia[, date := year(date)]
terra_czechia <- terra_czechia[, .(value = sum(value)), .(date)]
terra_czechia$domain <- "Czechia"
terra_danube <- fldmean(terraclimate[basin == "Morava", .(lon, lat, date, value)])
terra_danube[, date := year(date)]
terra_danube <- terra_danube[, .(value = sum(value)), .(date)]
terra_danube$domain <- "Morava"
terra_elbe <- fldmean(terraclimate[basin == "Labe", .(lon, lat, date, value)])
terra_elbe[, date := year(date)]
terra_elbe <- terra_elbe[, .(value = sum(value)), .(date)]
terra_elbe$domain <- "Labe"
terra_oder <- fldmean(terraclimate[basin == "Odra", .(lon, lat, date, value)])
terra_oder[, date := year(date)]
terra_oder <- terra_oder[, .(value = sum(value)), .(date)]
terra_oder$domain <- "Odra"

terraclimate <- rbind(terra_czechia, terra_danube, terra_elbe, terra_oder)

## mHM
mhm <- readRDS("data/ro/mhm_ro_cropped.rds")
setnames(mhm, c("lon", "lat", "date", "value"))
mhm[danube_mhm[, .(lon, lat)], basin := "Morava", on = .(lon, lat)]
mhm[elbe_mhm[, .(lon, lat)], basin := "Labe", on = .(lon, lat)]
mhm[is.na(basin), basin := "Odra"]

mhm_czechia <- fldmean(mhm[,.(lon, lat, date, value)])
mhm_czechia[, date := year(date)]
mhm_czechia <- mhm_czechia[, .(value = sum(value)), .(date)]
mhm_czechia$domain <- "Czechia"
mhm_danube <- fldmean(mhm[basin == "Morava", .(lon, lat, date, value)])
mhm_danube[, date := year(date)]
mhm_danube <- mhm_danube[, .(value = sum(value)), .(date)]
mhm_danube$domain <- "Morava"
mhm_elbe <- fldmean(mhm[basin == "Labe", .(lon, lat, date, value)])
mhm_elbe[, date := year(date)]
mhm_elbe <- mhm_elbe[, .(value = sum(value)), .(date)]
mhm_elbe$domain <- "Labe"
mhm_oder <- fldmean(mhm[basin == "Odra", .(lon, lat, date, value)])
mhm_oder[, date := year(date)]
mhm_oder <- mhm_oder[, .(value = sum(value)), .(date)]
mhm_oder$domain <- "Odra"

mhm <- rbind(mhm_czechia, mhm_danube, mhm_elbe, mhm_oder)

## ERA5
era5 <- readRDS("data/ro/era5_ro_cropped.rds")
setnames(era5, c("lon", "lat", "date", "value"))
era5[danube_era[, .(lon, lat)], basin := "Morava", on = .(lon, lat)]
era5[elbe_era[, .(lon, lat)], basin := "Labe", on = .(lon, lat)]
era5[is.na(basin), basin := "Odra"]

era_czechia <- fldmean(era5[,.(lon, lat, date, value)])
era_czechia[, date := year(date)]
era_czechia <- era_czechia[, .(value = sum(value)), .(date)]
era_czechia$domain <- "Czechia"
era_danube <- fldmean(era5[basin == "Morava", .(lon, lat, date, value)])
era_danube[, date := year(date)]
era_danube <- era_danube[, .(value = sum(value)), .(date)]
era_danube$domain <- "Morava"
era_elbe <- fldmean(era5[basin == "Labe", .(lon, lat, date, value)])
era_elbe[, date := year(date)]
era_elbe <- era_elbe[, .(value = sum(value)), .(date)]
era_elbe$domain <- "Labe"
era_oder <- fldmean(era5[basin == "Odra", .(lon, lat, date, value)])
era_oder[, date := year(date)]
era_oder <- era_oder[, .(value = sum(value)), .(date)]
era_oder$domain <- "Odra"

era5 <- rbind(era_czechia, era_danube, era_elbe, era_oder)

era5$name <- "ERA5-Land"
mhm$name <- "mHM"
terraclimate$name <- "TerraClimate"

ro_all <- rbind(terraclimate, mhm, era5)
setnames(ro_all, "value", "Q")

#
data_all <- merge(tp_all, et_all, by = c("date", "domain", "name")) %>%
  merge(ro_all, by = c("date", "domain", "name"))
data_all[, Residual := P - E - Q, by = .(date, domain, name)]
data_all[, `Cumulative\nResidual` := cumsum(Residual), by = .(domain, name)]

data_all$name <- factor(data_all$name,
                        levels = c('TerraClimate', 'mHM', 'ERA5-Land'))

data_all <- melt(data_all,c('date', 'domain', 'name'))

cols <- c("#1f78b4", "#33a02c", "#7570b3", "black", "#d95f02")

p00 <- ggplot(data_all, aes(x = date, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  facet_grid(domain ~ name) +
  theme_bw() +
  scale_color_manual(values = cols,
                     labels = c('Residual' = expression(xi),
                                'Cumulative\nResidual' =
                                  expression(paste("c(", xi, ")",
                                                   sep = "")))) + 
  labs(x = NULL, y = 'Water Flux in [mm]', title = NULL, color = NULL) +
  #scale_y_continuous(limits = c(-600, 200)) +
  theme(plot.title = element_text(size = 32),
        axis.text = element_text(size = 24), 
        axis.title = element_text(size = 28),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.text = element_text(size = 24), 
        legend.title = element_text(size = 28),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black",
                                        linewidth = 1))

ggsave("fig03.pdf", p00, width = 16, height = 9*1.5, dpi = 600)

