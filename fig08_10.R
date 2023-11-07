library(data.table)
library(ggplot2)
library(dplyr)
library(sf)
library(ggpubr)
library(scales)

GOLDEN_RATIO <- (1 + sqrt(5))/2
cz <- st_read("./data/gadm41_CZE_0.shp")
###
# terraclimate-Land
###
terraclimate_et <- readRDS("data/e/terra_aet_cropped.rds")
terraclimate_tp <- readRDS("data/tp/terra_ppt_cropped.rds")
terraclimate_ro <- readRDS("data/ro/terra_q_cropped.rds")

terraclimate_pme <- merge(terraclimate_tp, terraclimate_et, by = c("x", "y", "date"))
terraclimate_pme <- terraclimate_pme[, value := value.x - value.y][, .(x, y, date, value)]

terraclimate_res <- merge(terraclimate_pme, terraclimate_ro, by = c("x", "y", "date"))
terraclimate_res <- terraclimate_res[, value := value.x - value.y][, .(x, y, date, value)]

terraclimate_res <- terraclimate_res[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
terraclimate_res <- dcast(terraclimate_res, x + y + Z + season ~ period, value.var = "annual")
terraclimate_res <- terraclimate_res[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()

terraclimate_pme <- terraclimate_pme[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
terraclimate_pme <- dcast(terraclimate_pme, x + y + Z + season ~ period, value.var = "annual")
terraclimate_pme <- terraclimate_pme[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()

terraclimate_et <- terraclimate_et[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
terraclimate_et <- dcast(terraclimate_et, x + y + Z + season ~ period, value.var = "annual")
terraclimate_et <- terraclimate_et[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()

terraclimate_tp <- terraclimate_tp[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
terraclimate_tp <- dcast(terraclimate_tp, x + y + Z + season ~ period, value.var = "annual")
terraclimate_tp <- terraclimate_tp[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()

terraclimate_ro <- terraclimate_ro[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
terraclimate_ro <- dcast(terraclimate_ro, x + y + Z + season ~ period, value.var = "annual")
terraclimate_ro <- terraclimate_ro[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()

###
# Plots
###
p01 <- ggplot(terraclimate_tp) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(terraclimate_tp$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal P", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black",
                                        linewidth = 2))

p02 <- ggplot(terraclimate_et) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(terraclimate_et$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal E", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black",
                                        linewidth = 2))

p03 <- ggplot(terraclimate_ro) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(terraclimate_ro$median))) +
  theme_bw() +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal Q", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 2))

p04 <- ggplot(terraclimate_pme) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(terraclimate_pme$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal (P - E)", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black",
                                        linewidth = 2))

p05 <- ggplot(terraclimate_res) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(terraclimate_res$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL,
       title = "Changes in Seasonal (P - E - Q)",
       fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black",
                                        linewidth = 2))

p00 <- ggarrange(p01 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p02 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p03 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p04 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p05 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 nrow = 5, align = "v",
                 common.legend = TRUE, legend = "right") 

ggsave("fig08.pdf", p00, width = 4.5*GOLDEN_RATIO*3.5, height = 4.5*5, dpi = 600)
###
# era5-Land
###
era5_et <- readRDS("data/e/era5_e_cropped.rds")
era5_tp <- readRDS("data/tp/era5_tp_cropped.rds")
era5_ro <- readRDS("data/ro/era5_ro_cropped.rds")

era5_pme <- merge(era5_tp, era5_et, by = c("x", "y", "date"))
era5_pme <- era5_pme[, value := value.x - value.y][, .(x, y, date, value)]

era5_res <- merge(era5_pme, era5_ro, by = c("x", "y", "date"))
era5_res <- era5_res[, value := value.x - value.y][, .(x, y, date, value)]

era5_res <- era5_res[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
era5_res <- dcast(era5_res, x + y + Z + season ~ period, value.var = "annual")
era5_res <- era5_res[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()

era5_pme <- era5_pme[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
era5_pme <- dcast(era5_pme, x + y + Z + season ~ period, value.var = "annual")
era5_pme <- era5_pme[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()

era5_et <- era5_et[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
era5_et <- dcast(era5_et, x + y + Z + season ~ period, value.var = "annual")
era5_et <- era5_et[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()

era5_tp <- era5_tp[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
era5_tp <- dcast(era5_tp, x + y + Z + season ~ period, value.var = "annual")
era5_tp <- era5_tp[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()

era5_ro <- era5_ro[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
era5_ro <- dcast(era5_ro, x + y + Z + season ~ period, value.var = "annual")
era5_ro <- era5_ro[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()

###
# Plots
###
p01 <- ggplot(era5_tp) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(era5_tp$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal P", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 2))

p02 <- ggplot(era5_et) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(era5_et$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal E", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 2))

p03 <- ggplot(era5_ro) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(era5_ro$median))) +
  theme_bw() +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal Q", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 2))

p04 <- ggplot(era5_pme) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')), ncol =4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar", limits = c(-1,1)*max(abs(era5_pme$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal (P - E)", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 2))

p05 <- ggplot(era5_res) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(era5_res$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL,
       title = "Changes in Seasonal (P - E - Q)",
       fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black",
                                        linewidth = 2))

p00 <- ggarrange(p01 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p02 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p03 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p04 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p05 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 nrow = 5, align = "v",
                 common.legend = TRUE, legend = "right") 

ggsave("fig10.pdf", p00, width = 4.5*GOLDEN_RATIO*3.5, height = 4.5*5, dpi = 600)
###
# mhm
###
mhm_et <- readRDS("data/e/mhm_e_cropped.rds")
mhm_tp <- readRDS("data/tp/mhm_tp_cropped.rds")
mhm_ro <- readRDS("data/ro/mhm_ro_cropped.rds")

mhm_pme <- merge(mhm_tp, mhm_et, by = c("x", "y", "date"))
mhm_pme <- mhm_pme[, value := value.x - value.y][, .(x, y, date, value)]

mhm_res <- merge(mhm_pme, mhm_ro, by = c("x", "y", "date"))
mhm_res <- mhm_res[, value := value.x - value.y][, .(x, y, date, value)]

mhm_res <- mhm_res[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
mhm_res <- dcast(mhm_res, x + y + Z + season ~ period, value.var = "annual")
mhm_res <- mhm_res[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()
mhm_res_quantiles <- quantile(mhm_res$median, probs = c(.005, .995))
mhm_res <- mhm_res[median < mhm_res_quantiles[1], median := mhm_res_quantiles[1]
][median > mhm_res_quantiles[2], median := mhm_res_quantiles[2]]

mhm_pme <- mhm_pme[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
mhm_pme <- dcast(mhm_pme, x + y + Z + season ~ period, value.var = "annual")
mhm_pme <- mhm_pme[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()
mhm_pme_quantiles <- quantile(mhm_pme$median, probs = c(.005, .995))
mhm_pme <- mhm_pme[median < mhm_pme_quantiles[1], median := mhm_pme_quantiles[1]
][median > mhm_pme_quantiles[2], median := mhm_pme_quantiles[2]]

mhm_et <- mhm_et[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
mhm_et <- dcast(mhm_et, x + y + Z + season ~ period, value.var = "annual")
mhm_et <- mhm_et[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()
mhm_et_quantiles <- quantile(mhm_et$median, probs = c(.005, .995))
mhm_et <- mhm_et[median < mhm_et_quantiles[1], median := mhm_et_quantiles[1]
][median > mhm_et_quantiles[2], median := mhm_et_quantiles[2]]

mhm_tp <- mhm_tp[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
mhm_tp <- dcast(mhm_tp, x + y + Z + season ~ period, value.var = "annual")
mhm_tp <- mhm_tp[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()
mhm_tp_quantiles <- quantile(mhm_tp$median, probs = c(.005, .995))
mhm_tp <- mhm_tp[median < mhm_tp_quantiles[1], median := mhm_tp_quantiles[1]
][median > mhm_tp_quantiles[2], median := mhm_tp_quantiles[2]]

mhm_ro <- mhm_ro[year(date) <= 1990, period := "1961-1990"
][year(date) > 1990, period := "1991-2020"
][month(date) < 3 | month(date) == 12, season := "Winter"
][month(date) > 2 & month(date) < 6, season := "Spring"
][month(date) > 5 & month(date) < 9, season := "Summer"
][month(date) > 8 & month(date) < 12, season := "Autumn"
][, annual := sum(value, na.rm = TRUE), by = .(x, y, year(date), season)
][, Z := year(date)][, .(x, y, Z, period, annual, season)] %>% unique()
mhm_ro <- dcast(mhm_ro, x + y + Z + season ~ period, value.var = "annual")
mhm_ro <- mhm_ro[, median.1 := median(`1961-1990`, na.rm = TRUE), by = .(x, y, season)
][, median.2 := median(`1991-2020`, na.rm = TRUE), by = .(x, y, season)
][, median := median.2 - median.1, by = .(x,y, season)
][, .(x, y, median,season)] %>% unique()
mhm_ro_quantiles <- quantile(mhm_ro$median, probs = c(.005, .995))
mhm_ro <- mhm_ro[median < mhm_ro_quantiles[1], median := mhm_ro_quantiles[1]
][median > mhm_ro_quantiles[2], median := mhm_ro_quantiles[2]]

###
# Plots
###
p01 <- ggplot(mhm_tp) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')), ncol =4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar", limits = c(-1,1)*max(abs(mhm_tp$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal P", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 2))

p02 <- ggplot(mhm_et) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(mhm_et$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal E", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 2))

p03 <- ggplot(mhm_ro) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(mhm_ro$median))) +
  theme_bw() +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal Q", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black",
                                        linewidth = 2))

p04 <- ggplot(mhm_pme) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(mhm_pme$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Changes in Seasonal (P - E)", fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black",
                                        linewidth = 2))

p05 <- ggplot(mhm_res) +
  geom_raster(aes(x = x, y = y, fill = median)) +
  geom_sf(data = cz, fill = NA, color = "black", linewidth = 1) +
  coord_sf(expand = FALSE) +
  facet_wrap(~factor(season, levels=c('Winter', 'Spring', 'Summer', 'Autumn')),
             ncol = 4) +
  scale_fill_distiller(palette = "PRGn", direction = 1, guide = "colourbar",
                       limits = c(-1,1)*max(abs(mhm_res$median))) +
  theme_bw() +
  labs(x = NULL, y = NULL,
       title = "Changes in Seasonal (P - E - Q)",
       fill = "[mm]") +
  theme(plot.title = element_text(size = 30), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24),
        panel.grid = element_line(color = "black"),
        panel.border = element_rect(colour = "black", linewidth = 2),
        panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length=unit(-0.25, "cm"),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black",
                                        linewidth = 2))

p00 <- ggarrange(p01 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p02 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p03 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p04 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 p05 + theme(legend.key.height = unit(dev.size()[2]/5, "inches")),
                 nrow = 5, align = "v",
                 common.legend = TRUE, legend = "right") 

ggsave("fig09.pdf", p00, width = 4.5*GOLDEN_RATIO*3.5, height = 4.5*5, dpi = 600)
