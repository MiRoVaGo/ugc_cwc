library(data.table)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(Metrics)
library(gtable)
library(grid)
###
# tp
###
chmi <- fread("data/tp/chmi.csv")
chmi$name <- "CHMI"
chmi <- chmi[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
cru_ts <- fread("data/tp/cru-ts_tp_mm_cropped_196101_202012_025_monthly_ts.csv")
cru_ts$name <- "CRU TS v4.06"
cru_ts <- cru_ts[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
e_obs <- fread("data/tp/mhm_tp_cropped_ts.csv")
e_obs$name <- "mHM (E-OBS)"
e_obs <- e_obs[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
era5 <- fread("data/tp/era5_tp_cropped_ts.csv")
era5$name <- "ERA5-Land"
era5 <- era5[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
ncep_ncar <- fread("data/tp/ncep_ts.csv")
ncep_ncar$name <- "NCEP/NCAR R1"
ncep_ncar <- ncep_ncar[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
precl <- fread("data/tp/precl_tp_mm_cropped_196101_202012_025_monthly_ts.csv")
precl$name <- "PREC/L"
precl <- precl[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
terraclimate <- fread("data/tp/terraclimate_tp_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$name <- "TerraClimate"
terraclimate <- terraclimate[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()

mean_chmi <- mean(chmi[Year > 1980 & Year < 2011, P])
mean_cru_ts <- mean(cru_ts[Year > 1980 & Year < 2011, P])
mean_e_obs <- mean(e_obs[Year > 1980 & Year < 2011, P])
mean_era5 <- mean(era5[Year > 1980 & Year < 2011, P])
mean_ncep_ncar <- mean(ncep_ncar[Year > 1980 & Year < 2011, P])
mean_precl <- mean(precl[Year > 1980 & Year < 2011, P])
mean_terraclimate <- mean(terraclimate[Year > 1980 & Year < 2011, P])
sd_chmi <- sd(chmi[Year > 1980 & Year < 2011, P])
sd_cru_ts <- sd(cru_ts[Year > 1980 & Year < 2011, P])
sd_e_obs <- sd(e_obs[Year > 1980 & Year < 2011, P])
sd_era5 <- sd(era5[Year > 1980 & Year < 2011, P])
sd_ncep_ncar <- sd(ncep_ncar[Year > 1980 & Year < 2011, P])
sd_precl <- sd(precl[Year > 1980 & Year < 2011, P])
sd_terraclimate <- sd(terraclimate[Year > 1980 & Year < 2011, P])

global_cli <- data.frame(name = c("CHMI", "mHM (E-OBS)", "ERA5-Land", "TerraClimate",
                                  "CRU TS v4.06", "PREC/L", "NCEP/NCAR R1"),
                         cli_mean = c(mean_chmi, mean_e_obs, mean_era5,
                                      mean_terraclimate, mean_cru_ts,  
                                      mean_precl, mean_ncep_ncar),
                         cli_sd = c(sd_chmi, sd_e_obs, sd_era5, sd_terraclimate,
                                    sd_cru_ts, sd_precl, sd_ncep_ncar))
global_cli$name <- factor(global_cli$name, levels = global_cli$name)

sim <- merge(cru_ts[, tp1 := 100*(P - mean_cru_ts)/mean_cru_ts][, .(Year, tp1)],
             e_obs[, tp2 := 100*(P - mean_e_obs)/mean_e_obs][, .(Year, tp2)],
             by = "Year", all = TRUE) %>% 
  merge(era5[, tp3 := 100*(P - mean_era5)/mean_era5][, .(Year, tp3)],
        by = "Year", all = TRUE) %>%
  merge(ncep_ncar[, tp4 := 100*(P - mean_ncep_ncar)/mean_ncep_ncar
                  ][, .(Year, tp4)], by = "Year", all = TRUE) %>%
  merge(precl[, tp5 := 100*(P - mean_precl)/mean_precl][, .(Year, tp5)],
        by = "Year", all = TRUE) %>%
  merge(terraclimate[, tp6 := 100*(P - mean_terraclimate)/mean_terraclimate
                     ][, .(Year, tp6)], by = "Year", all = TRUE)

sim <- sim[, P := mean(c(tp1, tp2, tp3, tp4, tp5, tp6)), by = Year
           ][, spread := sd(c(tp1, tp2, tp3, tp4, tp5, tp6)), by = Year
             ][, .(Year, P, spread)]

global_cor <- rbind(e_obs[, sims := name][, .(Year, P, sims)],
                    era5[, sims := name][, .(Year, P, sims)],
                    terraclimate[, sims := name][, .(Year, P, sims)],
                    cru_ts[, sims := name][, .(Year, P, sims)],
                    precl[, sims := name][, .(Year, P, sims)],
                    ncep_ncar[, sims := name][, .(Year, P, sims)]) %>% 
  merge(chmi[, obs := name][, .(Year, P, obs)], by = "Year",
        allow.cartesian = TRUE)

global <- rbind(cru_ts[, .(Year, P, name)],
                e_obs[, .(Year, P, name)],
                era5[, .(Year, P, name)],
                ncep_ncar[, .(Year, P, name)],
                precl[, .(Year, P, name)],
                terraclimate[, .(Year, P, name)])

p00 <- copy(global_cor)
p00 <- p00[, `p-value` := formatC(cor.test(P.y, P.x)$p.value, format = "e",
                                  digits = 0), by = .(sims, obs)
           ][, RMSE := round(rmse(P.y, P.x), 3), by = .(sims, obs)
             ][, `R-squared` := round((cor(P.y, P.x)^2), 3), by = .(sims, obs)
               ][, .(obs, sims, `R-squared`, `p-value`, RMSE)] %>%
  unique() %>% setnames("obs", "Observations") %>% 
  setnames("sims", "Data Sets") %>%
  tableGrob(rows = NULL, theme = ttheme_minimal(base_size = 12))

p00 <- gtable_add_grob(p00, grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                 y0 = unit(0,"npc"),
                                                 x1 = unit(1,"npc"),
                                                 y1 = unit(0,"npc"),
                                                 gp = gpar(lwd = 2)),
                       t = 1, b = 1, l = 1, r = 5)

p01 <- copy(global_cli) %>% as.data.table()
p01 <- p01[, .(Name = name,
               avg = paste0(round(cli_mean), ' ± ',
                            sd = round(cli_sd)))] %>%
  setnames("avg", "1981-2020 Average") %>%
  tableGrob(rows = NULL, theme = ttheme_minimal(base_size = 12))

p01 <- gtable_add_grob(p01, grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                 y0 = unit(0,"npc"),
                                                 x1 = unit(1,"npc"),
                                                 y1 = unit(0,"npc"),
                                                 gp = gpar(lwd = 2)),
                       t = 1, b = 1, l = 1, r = 2)

p02 <- ggplot(data = sim, aes(x = Year, y = P)) +
  geom_ribbon(aes(ymax = P + spread, ymin = P - spread), fill = "gray") +
  geom_line(aes(color = "Data Sets")) +
  geom_line(color = "white") +
  geom_line(data = chmi, aes(y = 100*(P - mean_chmi)/mean_chmi,
                                           color = "CHMI"), linewidth = 1) +
  scale_x_continuous(limits = c(1961, 2020), expand = c(0, 0), 
                     breaks = seq(1920, 2040, 10)) +
  scale_color_manual(name = NULL, values = c("CHMI" = "#1f78b4",
                                             "Data Sets" = "gray")) +
  scale_y_continuous(limits = c(-150, 92), expand = c(0, 0), 
                     breaks = seq(-100, 100, 25)) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = "Precipitation\nAnomaly [%]", title = NULL) +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", linewidth = 2),
        panel.grid = element_blank(), 
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.text.x = element_text(face = "bold", hjust = 1),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        axis.ticks.length.y = unit(-.25, "cm"), axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.92, 0.88))

p03 <- p02 + annotation_custom(p00, xmin = 1998, ymax = -25) +
  annotation_custom(p01, xmin = 1915, ymax = -15)
###
#
###
gleam <- fread("data/e/gleam_e_mm_cropped_198001_202112_025_monthly_ts.csv")
gleam$name <- "GLEAM v3.6a"
gleam <- gleam[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
e_obs <- fread("data/e/mhm_e_cropped_ts.csv")
e_obs$name <- "mHM"
e_obs <- e_obs[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
era5 <- fread("data/e/era5_e_cropped_ts.csv")
era5$name <- "ERA5-Land"
era5 <- era5[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
ncep_ncar <- fread("data/e/ncep_ts.csv")
ncep_ncar$name <- "NCEP/NCAR R1"
ncep_ncar <- ncep_ncar[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
terraclimate <- fread("data/e/terraclimate_e_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$name <- "TerraClimate"
terraclimate <- terraclimate[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()

mean_gleam <- mean(gleam[Year > 1980 & Year < 2011, P])
mean_e_obs <- mean(e_obs[Year > 1980 & Year < 2011, P])
mean_era5 <- mean(era5[Year > 1980 & Year < 2011, P])
mean_ncep_ncar <- mean(ncep_ncar[Year > 1980 & Year < 2011, P])
mean_terraclimate <- mean(terraclimate[Year > 1980 & Year < 2011, P])
sd_gleam <- sd(gleam[Year > 1980 & Year < 2011, P])
sd_e_obs <- sd(e_obs[Year > 1980 & Year < 2011, P])
sd_era5 <- sd(era5[Year > 1980 & Year < 2011, P])
sd_ncep_ncar <- sd(ncep_ncar[Year > 1980 & Year < 2011, P])
sd_terraclimate <- sd(terraclimate[Year > 1980 & Year < 2011, P])

global_e_cli <- data.frame(name = c("GLEAM v3.6a", "ERA5-Land", "mHM", 
                                    "TerraClimate", "NCEP/NCAR R1"),
                           cli_mean = c(mean_gleam, mean_era5, mean_e_obs,
                                        mean_terraclimate, mean_ncep_ncar),
                           cli_sd = c(sd_gleam, sd_era5, sd_e_obs,
                                      sd_terraclimate, sd_ncep_ncar))
global_e_cli$name <- factor(global_e_cli$name, levels = global_e_cli$name)

sim_e <- merge(e_obs[, tp1 := 100*(P - mean_e_obs)/mean_e_obs][, .(Year, tp1)],
               era5[, tp2 := 100*(P - mean_era5)/mean_era5][, .(Year, tp2)],
               by = "Year", all = TRUE) %>% 
  merge(ncep_ncar[, tp3 := 100*(P - mean_ncep_ncar)/mean_ncep_ncar
  ][, .(Year, tp3)], by = "Year", all = TRUE) %>%
  merge(terraclimate[, tp4 := 100*(P - mean_terraclimate)/mean_terraclimate
  ][, .(Year, tp4)], by = "Year", all = TRUE)

sim_e <- sim_e[, P := mean(c(tp1, tp2, tp3, tp4)), by = Year
][, spread := sd(c(tp1, tp2, tp3, tp4)), by = Year
][, .(Year, P, spread)]

global_e_cor <- rbind(era5[, sim_es := name][, .(Year, P, sim_es)],
                      e_obs[, sim_es := name][, .(Year, P, sim_es)],
                      terraclimate[, sim_es := name][, .(Year, P, sim_es)],
                      ncep_ncar[, sim_es := name][, .(Year, P, sim_es)]) %>% 
  merge(gleam[, obs := name][, .(Year, P, obs)], by = "Year",
        allow.cartesian = TRUE)


global_e <- rbind(e_obs[, .(Year, P, name)],
                  era5[, .(Year, P, name)],
                  ncep_ncar[, .(Year, P, name)],
                  terraclimate[, .(Year, P, name)])

p04 <- copy(global_e_cor)
p04 <- p04[, `p-value` := formatC(cor.test(P.y, P.x)$p.value, format = "e",
                                  digits = 0), by = .(sim_es, obs)
][, RMSE := round(rmse(P.y, P.x), 3), by = .(sim_es, obs)
][, `R-squared` := round((cor(P.y, P.x)^2), 3), by = .(sim_es, obs)
][, .(obs, sim_es, `R-squared`, `p-value`, RMSE)] %>%
  unique() %>% setnames("obs", "Observations") %>% 
  setnames("sim_es", "Data Sets") %>%
  tableGrob(rows = NULL, theme = ttheme_minimal(base_size = 12))

p04 <- gtable_add_grob(p04, grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                 y0 = unit(0,"npc"),
                                                 x1 = unit(1,"npc"),
                                                 y1 = unit(0,"npc"),
                                                 gp = gpar(lwd = 4)),
                       t = 1, b = 1, l = 1, r = 5)

p05 <- copy(global_e_cli) %>% as.data.table()
p05 <- p05[, .(Name = name,
               avg = paste0(round(cli_mean), ' ± ',
                            sd = round(cli_sd)))] %>%
  setnames("avg", "1981-2020 Average") %>%
  tableGrob(rows = NULL, theme = ttheme_minimal(base_size = 12))

p05 <- gtable_add_grob(p05, grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                 y0 = unit(0,"npc"),
                                                 x1 = unit(1,"npc"),
                                                 y1 = unit(0,"npc"),
                                                 gp = gpar(lwd = 2)),
                       t = 1, b = 1, l = 1, r = 2)

p06 <- ggplot(data = sim_e, aes(x = Year, y = P)) +
  geom_ribbon(aes(ymax = P + spread, ymin = P - spread), fill = "gray") +
  geom_line(aes(color = "Data Sets")) +
  geom_line(color = "white") +
  geom_line(data = gleam, aes(y = 100*(P - mean_gleam)/mean_gleam,
                              color = "GLEAM v3.6a"), linewidth = 1) +
  scale_x_continuous(limits = c(1961, 2020), expand = c(0, 0), 
                     breaks = seq(1920, 2040, 10)) +
  scale_color_manual(name = NULL, values = c("GLEAM v3.6a" = "#33a02c",
                                             "Data Sets" = "gray")) +
  scale_y_continuous(limits = c(-150, 92), expand = c(0, 0), 
                     breaks = seq(-100, 100, 25)) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = "Evapotranspiration\nAnomaly [%]", title = NULL) +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", linewidth = 2),
        panel.grid = element_blank(), 
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.text.x = element_text(face = "bold", hjust = 1),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        axis.ticks.length.y = unit(-.25, "cm"), axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.92, 0.88))

p07 <- p06 + annotation_custom(p04, xmin = 1998, ymax = -50) +
  annotation_custom(p05, xmin = 1915, ymax = -25)
###
#
###
grun <- fread("data/ro/grun_ro_mm_cropped_190201_201412_025_monthly_ts.csv")
grun$name <- "GRUN v1"
grun <- grun[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
e_obs <- fread("data/ro/mhm_ro_cropped_ts.csv")
e_obs$name <- "mHM"
e_obs <- e_obs[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
era5 <- fread("data/ro/era5_ro_cropped_ts.csv")
era5$name <- "ERA5-Land"
era5 <- era5[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
ncep_ncar <- fread("data/ro/ncep-ncar_cropped_ts.csv")
ncep_ncar$name <- "NCEP/NCAR R1"
ncep_ncar <- ncep_ncar[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()
terraclimate <- fread("data/ro/terraclimate_ro_mm_cropped_196101_202012_025_monthly_ts.csv")
terraclimate$name <- "TerraClimate"
terraclimate <- terraclimate[, Year := year(date)][, P := sum(value), by = Year][, .(Year, P, name)] %>% unique()

mean_grun <- mean(grun[Year > 1980 & Year < 2011, P])
mean_e_obs <- mean(e_obs[Year > 1980 & Year < 2011, P])
mean_era5 <- mean(era5[Year > 1980 & Year < 2011, P])
mean_ncep_ncar <- mean(ncep_ncar[Year > 1980 & Year < 2011, P])
mean_terraclimate <- mean(terraclimate[Year > 1980 & Year < 2011, P])
sd_grun <- sd(grun[Year > 1980 & Year < 2011, P])
sd_e_obs <- sd(e_obs[Year > 1980 & Year < 2011, P])
sd_era5 <- sd(era5[Year > 1980 & Year < 2011, P])
sd_ncep_ncar <- sd(ncep_ncar[Year > 1980 & Year < 2011, P])
sd_terraclimate <- sd(terraclimate[Year > 1980 & Year < 2011, P])

global_q_cli <- data.frame(name = c("GRUN v1", "mHM", "TerraClimate", "ERA5-Land",
                                    "NCEP/NCAR R1"),
                           cli_mean = c(mean_grun, mean_e_obs, mean_terraclimate,
                                        mean_era5, mean_ncep_ncar),
                           cli_sd = c(sd_grun, sd_e_obs, sd_terraclimate, sd_era5,
                                      sd_ncep_ncar))
global_q_cli$name <- factor(global_q_cli$name, levels = global_q_cli$name)

sim_q <- merge(e_obs[, tp1 := 100*(P - mean_e_obs)/mean_e_obs][, .(Year, tp1)],
               era5[, tp2 := 100*(P - mean_era5)/mean_era5][, .(Year, tp2)],
               by = "Year", all = TRUE) %>% 
  merge(ncep_ncar[, tp3 := 100*(P - mean_ncep_ncar)/mean_ncep_ncar
  ][, .(Year, tp3)], by = "Year", all = TRUE) %>%
  merge(terraclimate[, tp4 := 100*(P - mean_terraclimate)/mean_terraclimate
  ][, .(Year, tp4)], by = "Year", all = TRUE)

sim_q <- sim_q[, P := mean(c(tp1, tp2, tp3, tp4)), by = Year
][, spread := sd(c(tp1, tp2, tp3, tp4)), by = Year
][, .(Year, P, spread)]

global_cor <- rbind(e_obs[, sim_qs := name][, .(Year, P, sim_qs)],
                    terraclimate[, sim_qs := name][, .(Year, P, sim_qs)],
                    era5[, sim_qs := name][, .(Year, P, sim_qs)],
                    ncep_ncar[, sim_qs := name][, .(Year, P, sim_qs)]) %>% 
  merge(grun[, obs := name][, .(Year, P, obs)], by = "Year",
        allow.cartesian = TRUE)


global <- rbind(e_obs[, .(Year, P, name)],
                era5[, .(Year, P, name)],
                ncep_ncar[, .(Year, P, name)],
                terraclimate[, .(Year, P, name)])

p08 <- copy(global_cor)
p08 <- p08[, `p-value` := formatC(cor.test(P.y, P.x)$p.value, format = "e",
                                  digits = 0), by = .(sim_qs, obs)
][, RMSE := round(rmse(P.y, P.x), 3), by = .(sim_qs, obs)
][, `R-squared` := round((cor(P.y, P.x)^2), 3), by = .(sim_qs, obs)
][, .(obs, sim_qs, `R-squared`, `p-value`, RMSE)] %>%
  unique() %>% setnames("obs", "Observations") %>% 
  setnames("sim_qs", "Data Sets") %>%
  tableGrob(rows = NULL, theme = ttheme_minimal(base_size = 12))

p08 <- gtable_add_grob(p08, grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                 y0 = unit(0,"npc"),
                                                 x1 = unit(1,"npc"),
                                                 y1 = unit(0,"npc"),
                                                 gp = gpar(lwd = 4)),
                       t = 1, b = 1, l = 1, r = 5)

p09 <- copy(global_q_cli) %>% as.data.table()
p09 <- p09[, .(Name = name,
               avg = paste0(round(cli_mean), ' ± ',
                            sd = round(cli_sd)))] %>%
  setnames("avg", "1981-2020 Average") %>%
  tableGrob(rows = NULL, theme = ttheme_minimal(base_size = 12))

p09 <- gtable_add_grob(p09, grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                 y0 = unit(0,"npc"),
                                                 x1 = unit(1,"npc"),
                                                 y1 = unit(0,"npc"),
                                                 gp = gpar(lwd = 2)),
                       t = 1, b = 1, l = 1, r = 2)

p10 <- ggplot(data = sim_q, aes(x = Year, y = P)) +
  geom_ribbon(aes(ymax = P + spread, ymin = P - spread), fill = "gray") +
  geom_line(aes(color = "Data Sets")) +
  geom_line(color = "white") +
  geom_line(data = grun, aes(y = 100*(P - mean_grun)/mean_grun,
                             color = "GRUN v1"), linewidth = 1) +
  scale_x_continuous(limits = c(1961, 2020), expand = c(0, 0), 
                     breaks = seq(1920, 2040, 10)) +
  scale_color_manual(name = NULL, values = c("GRUN v1" = "#7570b3",
                                             "Data Sets" = "gray")) +
  scale_y_continuous(limits = c(-150, 92), expand = c(0, 0), 
                     breaks = seq(-100, 100, 25)) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = "Runoff\nAnomaly [%]", title = NULL) +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", linewidth = 2),
        panel.grid = element_blank(), 
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.text.x = element_text(face = "bold", hjust = 1),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        axis.ticks.length.y = unit(-.25, "cm"), axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.94, 0.88))

p11 <- p10 + annotation_custom(p08, xmin = 1998, ymax = -35) +
  annotation_custom(p09, xmin = 1915, ymax = -25)
###
#
###
p12 <- ggarrange(p03, p07, p11, ncol = 1, labels = c('a', "b", "c"), align = 'hv')


ggsave("plots/stats.pdf", p12, width = 16, height = 4.5*3, dpi = 600)
