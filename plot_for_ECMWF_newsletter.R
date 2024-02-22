library(tidyverse)
library(tidync)

library(ncdf4)
nc <- nc_open("data/2017-12-02_temperature_forecast.nc")
t2m <- ncvar_get(nc, "t2m")
lon <- nc$dim$longitude$vals
lat <- nc$dim$latitude$vals


df <- tibble(
  lon = rep(lon, length(lat)),
  lat = rep(lat, each = length(!!lon)),
  t2m = as.vector(t2m[, , 1])
)

pp1 <- df %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = t2m - 273.15)) +
  scale_fill_gradient2(
    name = "°C",
    low = hcl(240, l = 20, c = 80),
    high = hcl(13, l = 20, c = 80)
  ) +
  borders() +
  coord_fixed(
    ratio = 1.5,
    expand = FALSE,
    xlim = range(lon) + c(-0.125, 0.125),
    ylim = range(lat) + c(-0.125, 0.125)
  ) +
  theme_void() +
  theme(
    panel.border = element_rect(colour = 1, fill = NA, linewidth = 0.5), 
    plot.margin = margin(3, 3, 3, 3, "pt")
  )

png("example_forecast.png", width = 4.6, height = 8, units = 'in', res = 200)
pp1
dev.off()


## read forecasts

path <- "data/v1.0"
methods_dict <- c(
  `ARSO_ANET_v1.0` = "ANET",
  `Bielefeld-University_AR-EMOS_v1.2` = "AR-EMOS",
  `ECMWF_RepresentativnessWithBiasCorrection_test-v1.1` = "ASRE",
  `KIT_simple-NN_v1.1` = "DRN",
  `MetOffice_IMPROVER-reliabilitycalibration-v1.3.1_v1.4` = "RC",
  `RMIB_Pythie-MBM-AbsCRPSmin-commit21a29a9_global-v1.0` = "MBM",
  `University-of-Hildesheim_D-Vine-Copula_v1.0` = "DVQR",
  `ZAMG_EMOS-v1.1` = "EMOS",
  `test_data_forecasts` = "ECMWF DMO"
)

obsfile <- list.files(path, "observations.nc", full.names = TRUE)

fcfiles <- paste0(
  path, 
  "/1_ESSD-benchmark_", 
  names(methods_dict), 
  ".nc"
) %>% 
  gsub("1_ESSD-benchmark_test_data", "ESSD_benchmark_test_data", .)

## read observations
obs <- obsfile %>% 
  tidync::tidync("t2m") %>%
  tidync::hyper_tibble(na.rm = FALSE) %>%
  dplyr::mutate(
    time = as.POSIXct("1970-01-01", tz = 'UTC') + 
      as.difftime(time, units = "secs")
  ) %>%
  dplyr::rename(obs = t2m)

## read station metadata and join in 
## the model orography and percentage missing
stations <- obsfile %>%
  tidync::tidync("D0") %>% 
  tidync::hyper_tibble(na.rm = FALSE) %>%
  dplyr::left_join(
    read.csv(paste0(path, "/model_orography_on_stations.csv")), 
    by = "station_id"
  ) %>%
  dplyr::left_join(
    read.csv(paste0(path, "/percentage_of_nan_in_t2m.csv")) %>%
      dplyr::rename(percentage_missing = X0), 
    by = "station_id"
  ) %>% 
  dplyr::left_join(
    read.csv(paste0(path, "/percentage_of_nan_in_t2m_bvs.csv")) %>%
      dplyr::rename(fraction_available = fraction) %>%
      dplyr::select(-X), 
    by = c("station_id", "station_name")
  )

station_ids <- stations %>% 
  filter(station_name %in% c("Säntis", "Koksijde")) %>%
  pull(station_id)


pp1_plus <- pp1 + 
  geom_point(
    data = stations, 
    aes(x = longitude, y = latitude), 
    size = 0.2, 
    col = "grey"
  ) + 
  geom_point(
    data = filter(stations, station_id %in% station_ids),
    aes(x = longitude, y = latitude)
  ) + 
  geom_text(
    data = filter(stations, station_id %in% station_ids),
    aes(x = longitude, y = latitude, label = station_name),
    hjust = 0.7,
    vjust = -0.5
  )

png("example_forecast_with_locations.png", width = 4.6 / 3 * 2, height = 8 / 3 * 2, units = 'in', res = 200)
pp1_plus
dev.off()


colours <- tibble(
  source = unname(methods_dict)
) %>%
  dplyr::filter(source != "ECMWF DMO") %>%
  dplyr::mutate(
    sourceval = as.numeric(factor(source))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    colour = hcl(sourceval / max(sourceval) * 360, l = 50, c = 70)
  )

source_colours <- setNames(
  c("#333333", colours$colour), 
  c("ECMWF DMO", colours$source)
)


source("read_data.R")
source("verif_functions.R")

fcst <- lapply(fcfiles, function(file) {
  print(file)
  read_file(file) %>%
    filter(
      time == as.Date("2017-12-02"), 
      station_id %in% station_ids
    ) %>%
    mutate(
      name = basename(file)
    )
}) %>%
  bind_rows()

methods_facet <- methods_dict[c(9, 1:4, NA, 5:8)]
methods_facet[6] <- "dummy"

fcst_for_plot <- fcst %>% 
  mutate(
    quants = t(apply(fcst, 1, quantile, c(0.1, 0.5, 0.9))) %>%
      scorer::toensdf(cols = paste0("q_", c(10, 50, 90)))
  ) %>%
  dplyr::select(-fcst) %>%
  unnest(quants) %>%
  mutate(
    name = gsub(".*benchmark_", "", name),
    name = gsub(".nc", "", name),
    name = methods_dict[name], 
    name = factor(name, methods_facet, order = TRUE)
  ) %>%
  left_join(obs) %>% 
  mutate(
    across(c(contains("q_"), obs), function(x) x - 273.15),
    reftime = time, 
    time = reftime + as.difftime(step, units = "hours") 
  ) %>% 
  left_join(stations, by = "station_id")


pp2a <- fcst_for_plot %>% 
  filter(station_id == station_ids[1]) %>%
  ggplot(aes(x = step, y = `q_50`, col = name, fill = name)) + 
  geom_ribbon(aes(ymin = `q_10`, ymax = `q_90`), col = NA) + 
  geom_line(linewidth = 0.5) + 
  geom_line(aes(y = obs), linewidth = 0.5, col = grey(0.3), lty = 2) + 
  facet_wrap( ~ name, nrow = 2, drop = FALSE) + 
  theme_void() + 
  theme(
    legend.position = "none", 
    strip.text = element_blank()
  ) + 
  scale_colour_manual(values = source_colours) + 
  scale_fill_manual(
    values = setNames(paste0(source_colours, "55"), names(source_colours))
  )

png("example_timeseries_Koksijde.png", width = 6, height = 3, units = "in", res = 200)
pp2a
dev.off()

pp2b <- fcst_for_plot %>% 
  filter(station_id == station_ids[2]) %>%
  ggplot(aes(x = step, y = `q_50`, col = name, fill = name)) + 
  geom_ribbon(aes(ymin = `q_10`, ymax = `q_90`), col = NA) + 
  geom_line(linewidth = 0.5) + 
  geom_line(aes(y = obs), linewidth = 0.5, col = grey(0.3), lty = 2) + 
  facet_wrap( ~ name, nrow = 2, drop = FALSE) + 
  theme_void() + 
  theme(
    legend.position = "none", 
    strip.text = element_blank()
  ) + 
  scale_colour_manual(values = source_colours) + 
  scale_fill_manual(
    values = setNames(paste0(source_colours, "55"), names(source_colours))
  )

png("example_timeseries_Saentis.png", width = 6, height = 3, units = "in", res = 200)
pp2b
dev.off()

scores_mn <- readRDS("scores_mn.rds")

pp3 <- scores_mn %>% 
  ggplot(aes(x = step, y = crps, col = source)) + 
  geom_line(linewidth = 0.8) + 
  scale_colour_manual(values = source_colours) + 
  theme_light() + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(0, 120, 24)) + 
  labs(x = "Lead time (hours)", y = "CRPS (°C)") 

png("example_scores.png", width = 5, height = 3, units = 'in', res = 200)
pp3 
dev.off()

library(patchwork)

png("example_all.png", width = 8, height = 3, units = 'in', res = 200)
layout <- "
AA#BBBB#####
AA#BBBB#DDDD
AA#BBBB#DDDD
AA######DDDD
AA#CCCC#DDDD
AA#CCCC#DDDD
AA#CCCC#####
"
pp1_plus + pp2a + pp2b + pp3 +
  plot_layout(design = layout)
dev.off()


## first review
darkgrey <- grey(0.2)
grey <- grey(0.6)
(
  pp1_plus <- df %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_tile(aes(fill = t2m - 273.15)) +
    scale_fill_gradient2(
      name = "°C",
      low = hcl(240, l = 20, c = 80),
      high = hcl(13, l = 20, c = 80)
    ) +
    borders(col = grey, linewidth = 0.3) +
    coord_fixed(
      ratio = 1.5,
      expand = FALSE,
      xlim = range(lon) + c(-0.125, 0.125),
      ylim = range(lat) + c(-0.125, 0.125)
    ) +
    geom_rect(
      data = stations %>% 
        dplyr::summarize(
          across(c(longitude, latitude), list(min = min, max = max))
        ) %>%
        dplyr::mutate(lon = 0, lat = 0),
      aes(
        xmin = longitude_min - 0.2, xmax = longitude_max + 0.2, 
        ymin = latitude_min - 0.2, ymax = latitude_max + 0.2, 
      ), 
      col = grey,
      fill = NA,
      linewidth = 0.3
    ) + 
    geom_point(
      data = stations, 
      aes(x = longitude, y = latitude), 
      size = 0.2, 
      col = grey
    ) + 
    geom_point(
      data = filter(stations, station_id %in% station_ids),
      aes(x = longitude, y = latitude),
      col = darkgrey
    ) + 
    geom_text(
      data = filter(stations, station_id %in% station_ids),
      aes(x = longitude, y = latitude, label = station_name),
      hjust = 0.7,
      vjust = -0.5,
      col = darkgrey
    ) + 
    ggtitle(
      "ECMWF IFS temperature forecast"
    ) + 
    theme_void() +
    theme(
      panel.border = element_rect(colour = darkgrey, fill = NA, linewidth = 0.3), 
      legend.position = c(0.12, 0.87),
      legend.background = element_rect(colour = NA),
      legend.margin = margin(t = c(-5, 3, 6, 3), unit = 'pt'),
      legend.title = element_text(hjust = 1, vjust = -4, size = unit(9, "pt")),
      plot.title = element_text(size = unit(11, "pt"), vjust = 4)
    )  
)



pp2 <- sapply(unique(fcst_for_plot$station_name), function(nn) {
  data <- fcst_for_plot %>% 
    filter(station_name == !!nn)
  ylim <- data %>% 
    dplyr::select(c(obs, contains("q_"))) %>%
    unlist() %>% 
    range()
  pp_1 <- data %>% 
    filter(grepl("ECMWF", name)) %>% 
    ggplot(aes(x = time, y = q_50, col = name, fill = name)) + 
    geom_ribbon(aes(ymin = q_10, ymax = q_90), col = NA) + 
    geom_line(linewidth = 0.5) + 
    geom_line(aes(y = obs), linewidth = 0.5, col = grey(0.3), lty = 2) + 
    expand_limits(y = ylim) + 
    theme_light() + 
    theme(
      legend.position = "none", 
    ) + 
    scale_colour_manual(values = source_colours) + 
    scale_fill_manual(
      values = setNames(paste0(source_colours, "55"), names(source_colours))
    ) + 
    labs(y = "Temperature (°C)", x = "") + 
    ggtitle("ECMWF IFS")
  pp_2 <- data %>% 
    filter(!grepl("ECMWF", name)) %>% 
    ggplot(aes(x = time, y = q_50, col = name, fill = name)) + 
    geom_ribbon(aes(ymin = q_10, ymax = q_90), col = NA) + 
    geom_line(linewidth = 0.5) + 
    geom_line(aes(y = obs), linewidth = 0.5, col = grey(0.3), lty = 2) + 
    expand_limits(y = ylim) + 
    theme_light() + 
    theme(
      legend.position = "none", 
      strip.text = element_blank()
    ) + 
    facet_wrap(~name, nrow = 2) + 
    scale_colour_manual(values = source_colours) + 
    scale_fill_manual(
      values = setNames(paste0(source_colours, "55"), names(source_colours))
    ) + 
    labs(x = "", y = "") + 
    ggtitle("Postprocessed forecasts using different methods")
  list(pp_1, pp_2)
}, 
simplify = FALSE)

subplot_layout <- "
ABBBB
ABBBB
ABBBB
#BBBB
#BBBB
"

pp2_subplots <- sapply(names(pp2), function(nn) {
  pp2[[nn]][[1]] + pp2[[nn]][[2]] + plot_layout(design = subplot_layout) 
}, simplify = FALSE)

pp2_plots <- sapply(unique(fcst_for_plot$station_name), function(nn) {
  fcst_for_plot %>% 
    filter(station_name == nn) %>%
    ggplot(aes(x = time, y = `q_50`, col = name, fill = name)) + 
    geom_ribbon(aes(ymin = `q_10`, ymax = `q_90`), col = NA) + 
    geom_line(linewidth = 0.5) + 
    geom_line(aes(y = obs), linewidth = 0.5, col = grey(0.3), lty = 2) + 
    facet_wrap( ~ name, nrow = 2, drop = FALSE) + 
    
    scale_colour_manual(values = source_colours) + 
    scale_fill_manual(
      values = setNames(paste0(source_colours, "55"), names(source_colours))
    ) + 
    labs(x = "", y = "Temperature (°C)") + 
    ggtitle(
      "ECMWF IFS              Postprocessed forecasts using different postprocessing methods"
    ) +
    theme_light() + 
    theme(
      legend.position = "none", 
      strip.text = element_blank(),
      plot.title = element_text(size = unit(11, "pt"))
    )
}, 
simplify = FALSE)




playout <- "
AAAA#BBBBBBBBB
AAAA#BBBBBBBBB
AAAA#BBBBBBBBB
AAAA##########
AAAA#CCCCCCCCC
AAAA#CCCCCCCCC
AAAA#CCCCCCCCC
"

png("plot_for_ECMWF_newsletter.png", width = 11.5, height = 7, units = 'in', res = 200)
pp1_plus + pp2_plots[[1]] + pp2_plots[[2]] + 
  plot_layout(design = playout)
dev.off()

