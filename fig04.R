library(data.table)
library(ggplot2)
library(ggpubr)
###
# tp
###
era5 <- fread("data/tp/era5_tp_cropped_ts.csv")
era5$name <- "era5"
mhm <- fread("data/tp/mhm_tp_cropped_ts.csv")
mhm$name <- "mhm"
terraclimate <- fread("data/tp/terraclimate_tp_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$type <- NULL
terraclimate$name <- "terraclimate"

tp_all <- rbind(era5, mhm, terraclimate)
tp_all <- tp_all[year(date) <= 1990, period := "1961-1990"
                 ][year(date) > 1990, period := "1991-2020"
                   ][, Z := year(date)
                     ][, annual := sum(value, na.rm = TRUE), by = .(Z, name)
                       ][, .(Z, annual, period, name)] %>% unique()

p01 <- ggplot(tp_all[name == "era5"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 50, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = "Count", title = " ", fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2)) +
  scale_x_continuous(limits = c(450, 1050), breaks = seq(500, 1000, 100)) +
  geom_vline(xintercept = median(tp_all[name == "era5" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(tp_all[name == "era5" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

p02 <- ggplot(tp_all[name == "mhm"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 50, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = "Count", title = " ", fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2)) +
  scale_x_continuous(limits = c(450, 1050), breaks = seq(500, 1000, 100)) +
  geom_vline(xintercept = median(tp_all[name == "mhm" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(tp_all[name == "mhm" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

p03 <- ggplot(tp_all[name == "terraclimate"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 50, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = "Count", title = "Annual P in [mm]", fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2)) +
  scale_x_continuous(limits = c(450, 1050), breaks = seq(500, 1000, 100)) +
  geom_vline(xintercept = median(tp_all[name == "terraclimate" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(tp_all[name == "terraclimate" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

gg_tp <- ggarrange(p03, p02, p01, nrow = 3, align = "v")
###
# e
###
era5 <- fread("data/e/era5_e_cropped_ts.csv")
era5$name <- "era5"
mhm <- fread("data/e/mhm_e_cropped_ts.csv")
mhm$name <- "mhm"
terraclimate <- fread("data/e/terraclimate_e_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$type <- NULL
terraclimate$name <- "terraclimate"

e_all <- rbind(era5, mhm, terraclimate)
e_all <- e_all[year(date) <= 1990, period := "1961-1990"
               ][year(date) > 1990, period := "1991-2020"
                 ][, Z := year(date)
                   ][, annual := sum(value, na.rm = TRUE), by = .(Z, name)
                     ][, .(Z, annual, period, name)] %>% unique()

p04 <- ggplot(e_all[name == "era5"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 10, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = " ", title = " ", fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2)) +
  scale_x_continuous(limits = c(425, 675), breaks = seq(450, 650, 50)) +
  geom_vline(xintercept = median(e_all[name == "era5" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(e_all[name == "era5" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

p05 <- ggplot(e_all[name == "mhm"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 10, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = " ", title = " ", fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2)) +
  scale_x_continuous(limits = c(425, 675), breaks = seq(450, 650, 50)) +
  geom_vline(xintercept = median(e_all[name == "mhm" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(e_all[name == "mhm" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

p06 <- ggplot(e_all[name == "terraclimate"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 10, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = " ", title = "Annual E in [mm]", fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2)) +
  scale_x_continuous(limits = c(425, 675), breaks = seq(450, 650, 50)) +
  geom_vline(xintercept = median(e_all[name == "terraclimate" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(e_all[name == "terraclimate" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

gg_e <- ggarrange(p06, p05, p04, nrow = 3, align = "v")
###
# q
##
era5 <- fread("data/ro/era5_ro_cropped_ts.csv")
era5$name <- "era5"
mhm <- fread("data/ro/mhm_ro_cropped_ts.csv")
mhm$name <- "mhm"
terraclimate <- fread("data/ro/terraclimate_ro_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$type <- NULL
terraclimate$name <- "terraclimate"

ro_all <- rbind(era5, mhm, terraclimate)
ro_all <- ro_all[year(date) <= 1990, period := "1961-1990"
                 ][year(date) > 1990, period := "1991-2020"
                   ][, Z := year(date)
                     ][, annual := sum(value, na.rm = TRUE), by = .(Z, name)
                       ][, .(Z, annual, period, name)] %>% unique()

p07 <- ggplot(ro_all[name == "era5"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 25, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = " ", title = " ", fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2)) +
  scale_x_continuous(limits = c(50, 425), breaks = seq(100, 400, 100)) +
  geom_vline(xintercept = median(ro_all[name == "era5" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(ro_all[name == "era5" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

p08 <- ggplot(ro_all[name == "mhm"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 25, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = " ", title = " ", fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2)) +
  scale_x_continuous(limits = c(50, 425), breaks = seq(100, 400, 100)) +
  geom_vline(xintercept = median(ro_all[name == "mhm" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(ro_all[name == "mhm" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

p09 <- ggplot(ro_all[name == "terraclimate"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 25, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = " ", title = "Annual Q in [mm]",
       fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2)) +
  scale_x_continuous(limits = c(50, 425), breaks = seq(100, 400, 100)) +
  geom_vline(xintercept = median(ro_all[name == "terraclimate" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(ro_all[name == "terraclimate" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

gg_ro <- ggarrange(p09, p08, p07, nrow = 3, align = "v")
###
# pme
###
pme_all <- merge(tp_all, e_all, by = c('Z', 'name', 'period'))
pme_all <- pme_all[, annual := annual.x - annual.y, by = .(Z, name)
                   ][, .(Z, annual, period, name)]

p10 <- ggplot(pme_all[name == "era5"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 25, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = " ", title = " ", fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2))  +
  scale_x_continuous(limits = c(-50, 450), breaks = seq(0, 400, 100)) +
  geom_vline(xintercept = median(pme_all[name == "era5" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(pme_all[name == "era5" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

p11 <- ggplot(pme_all[name == "mhm"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 25, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = " ", title = " ", fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2))  +
  scale_x_continuous(limits = c(-50, 450), breaks = seq(0, 400, 100)) +
  geom_vline(xintercept = median(pme_all[name == "mhm" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(pme_all[name == "mhm" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

p12 <- ggplot(pme_all[name == "terraclimate"], aes(x = annual, fill = period)) +
  geom_histogram(binwidth = 25, color = "black", position = "dodge") +
  theme_bw() + 
  labs(x = NULL, y = " ", title = "Annual P - E in [mm]",
       fill = "Period") + 
  scale_fill_manual(values = c("#0C7BCD", "#FFC20A")) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0),
                     breaks = seq(2, 8, 2))  +
  scale_x_continuous(limits = c(-50, 450), breaks = seq(0, 400, 100)) +
  geom_vline(xintercept = median(pme_all[name == "terraclimate" & Z <= 1990, annual]), 
             linetype = "dashed", color = "#0C7BCD", linewidth = 1) +
  geom_vline(xintercept = median(pme_all[name == "terraclimate" & Z > 1990, annual]), 
             linetype = "dashed", color = "#FFC20A", linewidth = 1) +
  theme(plot.title = element_text(size=28), axis.text = element_text(size = 20), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

gg_pet <- ggarrange(p10, p11, p12, nrow = 3, align = "v")

p_terra <- annotate_figure(ggarrange(p03, p06, p09, p12, ncol = 4, align = 'hv',
                                     common.legend = TRUE, legend = "none"),
                           right = text_grob("TerraClimate", rot = -90, size = 24))
p_mhm <- annotate_figure(ggarrange(p02, p05, p08, p11, ncol = 4, align = 'hv',
                                   common.legend = TRUE, legend = "none"),
                         right = text_grob("mHM", rot = -90, size = 24))
p_era <- annotate_figure(ggarrange(p01, p04, p07, p10, ncol = 4, align = 'hv',
                                   common.legend = TRUE, legend = "none"),
                         right = text_grob("ERA5-Land", rot = -90, size = 24))


p00 <- ggarrange(p_terra, p_mhm, p_era,
                 get_legend(p01, 'bottom'),
                 nrow = 4, align = "hv", heights = c(1,1,1,0.2))

ggsave("fig04.pdf", p00, width = 5.01*4, height = 5.01*3.1, dpi = 600)
