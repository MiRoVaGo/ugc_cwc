library(data.table)
library(ggplot2)
library(ggpubr)
###
# tp
###
era5 <- fread("data/tp/era5_tp_cropped_ts.csv")
era5$name <- "ERA5-Land"
mhm <- fread("data/tp/mhm_tp_cropped_ts.csv")
mhm$name <- "mHM"
terraclimate <- fread("data/tp/terraclimate_tp_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$type <- NULL
terraclimate$name <- "TerraClimate"

tp_all <- rbind(era5, mhm, terraclimate)
tp_all <- tp_all[year(date) <= 1990, period := "1961-1990"
                 ][year(date) > 1990, period := "1991-2020"
                   ][, Z := year(date)
                     ][, annual := sum(value, na.rm = TRUE), by = .(Z, name)
                       ][, .(Z, annual, period, name)] %>% unique()
###
# e
###
era5 <- fread("data/e/era5_e_cropped_ts.csv")
era5$name <- "ERA5-Land"
mhm <- fread("data/e/mhm_e_cropped_ts.csv")
mhm$name <- "mHM"
terraclimate <- fread("data/e/terraclimate_e_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$type <- NULL
terraclimate$name <- "TerraClimate"

e_all <- rbind(era5, mhm, terraclimate)
e_all <- e_all[year(date) <= 1990, period := "1961-1990"
               ][year(date) > 1990, period := "1991-2020"
                 ][, Z := year(date)
                   ][, annual := sum(value, na.rm = TRUE), by = .(Z, name)
                     ][, .(Z, annual, period, name)] %>% unique()
###
# q
##
era5 <- fread("data/ro/era5_ro_cropped_ts.csv")
era5$name <- "ERA5-Land"
mhm <- fread("data/ro/mhm_ro_cropped_ts.csv")
mhm$name <- "mHM"
terraclimate <- fread("data/ro/terraclimate_ro_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$type <- NULL
terraclimate$name <- "TerraClimate"

ro_all <- rbind(era5, mhm, terraclimate)
ro_all <- ro_all[year(date) <= 1990, period := "1961-1990"
                 ][year(date) > 1990, period := "1991-2020"
                   ][, Z := year(date)
                     ][, annual := sum(value, na.rm = TRUE), by = .(Z, name)
                       ][, .(Z, annual, period, name)] %>% unique()
###
# pme
###
pme_all <- merge(tp_all, e_all, by = c('Z', 'name', 'period'))
pme_all <- pme_all[, annual := annual.x - annual.y, by = .(Z, name)
                   ][, .(Z, annual, period, name)]

data_all <- merge(tp_all, e_all, by = c('Z', 'name', 'period')) %>%
  merge(ro_all, by = c('Z', 'name', 'period')) %>%
  merge(pme_all, by = c('Z', 'name', 'period'))
setnames(data_all, c('Z', 'name', 'period', 'P', 'E', 'Q', 'P - E'))
data_all <- melt(data_all,c('Z', 'name', 'period'))

data_all$name <- factor(data_all$name,
                        levels = c('TerraClimate', 'mHM', 'ERA5-Land'))

p00 <- ggplot(data_all, aes(x = name, y = value, fill = period)) +
  geom_boxplot() +
  theme_bw() + 
  labs(x = NULL, y = 'Water Flux in [mm]', fill = "Period", title = NULL) +
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  facet_wrap(~variable, nrow = 1) +
  theme(plot.title = element_text(size = 32),
        axis.text = element_text(size = 24), 
        axis.title = element_text(size = 28),
        axis.text.x = element_text(angle = 90),
        legend.text = element_text(size = 24), 
        legend.title = element_text(size = 28),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black",
                                        linewidth = 1))

ggsave("fig05.pdf", p00, width = 16, height = 9, dpi = 600)
