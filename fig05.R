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
                 ][year(date) > 1990, period := "1991-2020"]
# e
era5 <- fread("data/e/era5_e_cropped_ts.csv")
era5$name <- "ERA5-Land"
mhm <- fread("data/e/mhm_e_cropped_ts.csv")
mhm$name <- "mHM"
terraclimate <- fread("data/e/terraclimate_e_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$type <- NULL
terraclimate$name <- "TerraClimate"

e_all <- rbind(era5, mhm, terraclimate)
e_all <- e_all[year(date) <= 1990, period := "1961-1990"
               ][year(date) > 1990, period := "1991-2020"]
# q
era5 <- fread("data/ro/era5_ro_cropped_ts.csv")
era5$name <- "ERA5-Land"
mhm <- fread("data/ro/mhm_ro_cropped_ts.csv")
mhm$name <- "mHM"
terraclimate <- fread("data/ro/terraclimate_ro_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$type <- NULL
terraclimate$name <- "TerraClimate"

ro_all <- rbind(era5, mhm, terraclimate)
ro_all <- ro_all[year(date) <= 1990, period := "1961-1990"
                 ][year(date) > 1990, period := "1991-2020"]
# pme
pme_all <- merge(tp_all, e_all, by = c('date', 'name', 'period'))
pme_all <- pme_all[, value := value.x - value.y, by = .(date, name)
                   ][, .(date, value, name, period)]

data_all <- merge(tp_all, e_all, by = c('date', 'name', 'period')) %>%
  merge(ro_all, by = c('date', 'name', 'period')) %>%
  merge(pme_all, by = c('date', 'name', 'period'))
setnames(data_all, c('date', 'name', 'period', 'P', 'E', 'Q', 'P - E'))
data_all <- melt(data_all,c('date', 'name', 'period'))

data_all$name <- factor(data_all$name, levels = c('TerraClimate', 'mHM', 'ERA5-Land'))

p00 <- ggplot(data_all, aes(x = as.factor(month(date)), y = value, fill = period)) +
  geom_boxplot() +
  theme_bw() + 
  labs(x = NULL, y = 'Water Flux in [mm]', fill = "Period", title = NULL) +
  scale_fill_manual(values = c("#CCCCCC", "#818181")) +
  scale_x_discrete(breaks = seq(1, 12), 
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                              "Aug", "Sep", "Oct", "Nov", "Dec")) +
  facet_grid(variable ~ name) +
  theme(plot.title = element_text(size = 32),
        axis.text = element_text(size = 24), 
        axis.title = element_text(size = 28), 
        legend.text = element_text(size = 24), 
        legend.title = element_text(size = 28),
        strip.text = element_text(size = 28),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 1),
        legend.position = 'bottom')

ggsave("plots/boxplot.pdf", p00, width = 8.15*3.1, height = 5.01*4, dpi = 600)
