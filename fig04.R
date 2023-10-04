library(data.table)
library(ggplot2)
library(ggpubr)
library(toOrdinal)
library(gtable)
library(grid)
library(patchwork)
library(gridExtra)
###
# tp
###
cru_ts <- fread("data/tp/cru-ts_tp_mm_cropped_196101_202012_025_monthly_ts.csv")
cru_ts$name <- "CRU TS v4.06"
e_obs <- fread("data/tp/mhm_tp_cropped_ts.csv")
e_obs$name <- "mHM"
era5 <- fread("data/tp/era5_tp_cropped_ts.csv")
era5$name <- "ERA5-Land"
ncep_ncar <- fread("data/tp/ncep_ts.csv")
ncep_ncar$name <- "NCEP/NCAR R1"
precl <- fread("data/tp/precl_tp_mm_cropped_196101_202012_025_monthly_ts.csv")
precl$name <- "PREC/L"
terraclimate <- fread("data/tp/terraclimate_tp_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$name <- "TerraClimate"
chmi <- fread("data/tp/chmi.csv")
chmi$name <- "CHMI"

tp_all <- rbind(cru_ts, e_obs, era5, ncep_ncar, precl, terraclimate, fill = TRUE)
tp_all <- tp_all[, type := NULL]
setnames(tp_all, c("value", "name"), c("tp", "tp_name"))

tp_cor <- merge(tp_all, chmi, by = "date", allow.cartesian = TRUE)
tp_cor <- tp_cor[, Year := year(date)
][, annual_tp := sum(tp), by =.(Year, tp_name)
][, annual := sum(value), by = .(Year, tp_name)
][, .(Year, annual_tp, tp_name, annual, name)] %>% unique()
tp_cor <- tp_cor[, tp_cor := cor(annual_tp, annual), by = .(tp_name, name)
][, .(tp_name, tp_cor)] %>% unique()
###
# et
###
era5 <- fread("data/e/era5_e_cropped_ts.csv")
era5$name <- "ERA5-Land"
mhm <- fread("data/e/mhm_e_cropped_ts.csv")
mhm$name <- "mHM"
ncep_ncar <- fread("data/e/ncep_ts.csv")
ncep_ncar$name <- "NCEP/NCAR R1"
terraclimate <- fread("data/e/terraclimate_e_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$name <- "TerraClimate"

e_all <- rbind(era5, mhm, ncep_ncar, terraclimate, fill = TRUE)
e_all <- e_all[, type := NULL]
setnames(e_all, c("value", "name"), c("e", "e_name"))
###
# ro
###
era5 <- fread("data/ro/era5_ro_cropped_ts.csv")
era5$name <- "ERA5-Land"
mhm <- fread("data/ro/mhm_ro_cropped_ts.csv")
mhm$name <- "mHM"
ncep_ncar <- fread("data/ro/ncep-ncar_cropped_ts.csv")
ncep_ncar$name <- "NCEP/NCAR R1"
terraclimate <- fread("data/ro/terraclimate_ro_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$name <- "TerraClimate"
grdc <- fread("data/ro/grdc.csv")
grdc$name <- "GRDC"

ro_all <- rbind(era5, mhm, ncep_ncar, terraclimate, fill = TRUE)
ro_all <- ro_all[, type := NULL]
setnames(ro_all, c("value", "name"), c("ro", "ro_name"))

ro_cor <- merge(ro_all, grdc, by = "date", allow.cartesian = TRUE)
ro_cor <- ro_cor[, Year := year(date)
][, annual_ro := sum(ro), by =.(Year, ro_name)
][, annual := sum(value), by = .(Year, ro_name)
][, .(Year, annual_ro, ro_name, annual, name)] %>% unique()
ro_cor <- ro_cor[, ro_cor := cor(annual_ro, annual), by = .(ro_name, name)
][, .(ro_name, ro_cor)] %>% unique()
###
# budget
###
cwc <- merge(tp_all, e_all, by = "date", allow.cartesian = TRUE) %>%
  merge(ro_all, by = "date", allow.cartesian = TRUE)
cwc <- cwc[, Z := year(date)
][, P := sum(tp), by = .(Z, tp_name, e_name, ro_name)
][, E := sum(e), by = .(Z, tp_name, e_name, ro_name)
][, Q := sum(ro), by = .(Z, tp_name, e_name, ro_name)
][, r := P - E - Q
][, r_cor := cor(P - E, Q), by = .(tp_name, e_name, ro_name)
][, .(Z, r, r_cor, tp_name, e_name, ro_name)] %>% unique()

cwc_dist <- copy(cwc)
cwc_dist <- cwc_dist[, r_mean := mean(r), by = .(tp_name, e_name, ro_name)
][, r_sd := sd(r), by = .(tp_name, e_name, ro_name)
][, .(r_mean, r_sd, r_cor, tp_name, e_name, ro_name)] %>%
  unique()
cwc_dist <- merge(cwc_dist, tp_cor, by = "tp_name", allow.cartesian = TRUE) %>%
  merge(ro_cor, by = "ro_name", allow.cartesian = TRUE)
cwc_dist <- cwc_dist[order(cwc_dist$r_sd, abs(cwc_dist$r_mean), rev(r_cor),
                           rev(tp_cor), rev(ro_cor))]

cwc_rank <- copy(cwc_dist)
cwc_rank <- cwc_rank[, rank := (r_sd*abs(r_mean))/((r_cor*tp_cor*ro_cor)^2)
][, .(rank, tp_name, e_name, ro_name)]
cwc_rank <- cwc_rank[order(rank)]
cwc_rank$rank_idx <- seq.int(nrow(cwc_rank))
cwc_rank <- cwc_rank[, .(rank_idx, tp_name, e_name, ro_name)]

cwc <- merge(cwc, cwc_rank, by = c("tp_name", "e_name", "ro_name"),
             allow.cartesian = TRUE)

plot_ranks <- c(1:6, 14, 24, 38, 48, 72, 87, 96)
###
# Plots
###
p01 <- ggplot(cwc[rank_idx %in% plot_ranks]) +
  geom_density(aes(x = r, fill = rank_idx, group = rank_idx)) +
  scale_fill_distiller(guide = "colourbar", palette = "BrBG") +
  theme_bw() +
  labs(x = "Budget Residual [mm]", y = "Density", fill = "Ranking") +
  scale_x_continuous(limits = c(-720, 720), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.014), expand = c(0, 0),
                     breaks = seq(0.002, 0.012, 0.002)) +
  theme(panel.border = element_rect(colour = "black", linewidth = 2),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        axis.ticks.length.y = unit(-.25, "cm"))

p02 <- copy(cwc_rank)
p02$rank_idx <- toOrdinal(p02$rank_idx)
setnames(p02, c("rank_idx", "tp_name", "e_name", "ro_name"),
         c("Ranking", "P Data", "E Data", "Q Data"))
p02 <- tableGrob(p02, rows = NULL, theme = ttheme_minimal(base_size = 16))

p02 <- gtable_add_grob(p02, grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                 y0 = unit(0,"npc"),
                                                 x1 = unit(1,"npc"),
                                                 y1 = unit(0,"npc"),
                                                 gp = gpar(lwd = 4)),
                       t = 1, b = 1, l = 1, r = 4)

p03 <- ggplot(cwc) +
  geom_density(aes(x = r, fill = rank_idx, group = rank_idx)) +
  scale_fill_distiller(guide = "colourbar", palette = "BrBG") +
  theme_bw() + 
  labs(x = "Budget Residual [mm]", y = "Density", fill = "Ranking") +
  scale_x_continuous(limits = c(-720, 720), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.014), expand = c(0, 0),
                     breaks = seq(0.002, 0.012, 0.002)) +
  theme(panel.border = element_rect(colour = "black", linewidth = 2),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 16), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20), 
        axis.ticks.length.y = unit(-.25, "cm"))

p00 <- ggarrange(p02)
ggsave("sup01.pdf", p00, width = 9, height = 16*2)
ggsave("sup02.pdf", p03, width = 8, height = 4.5)
ggsave("fig04.pdf", p01, width = 8, height = 4.5)
