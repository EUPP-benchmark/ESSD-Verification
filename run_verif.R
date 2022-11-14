## set the path to your data directory
path <- "/app_scratch/fc_development/users/bhj/ESSD_verification"
## this is needed in `read_data.R`
obsfile <- list.files(path, "observations.nc", full.names = TRUE)
fcfiles <- setdiff(list.files(path, ".nc$", full.names = TRUE), obsfile)
# I had trouble reading the v0.9.nc file, therefore excluded here
fcfiles <- grep("v0.9.nc", fcfiles, invert = TRUE, value = TRUE)

source("read_data.R")
source("verif_functions.R")



#Read all the data (This is also deleting the previous variables)
all_data <- read_data_from_path()

#Compute several scores as a function of lead-time and Station
ll_veri <- verif_for_st_lt(all_data)



# here's an alternative take that hopefully works for all files
# yet to be checked though
# NB: loading and computation (CRPS) takes quite some time
scores <- lapply(fcfiles, compute_scores, obs = obs) %>%
  dplyr::bind_rows()

## compute plots by lead time
scores_mn <- scores %>%
  tidyr::drop_na() %>% 
  dplyr::group_by(source, step) %>%
  dplyr::summarize(
    bias = mean(mn - obs),
    mae = mean(abs(mn - obs)), 
    rmse = sqrt(mean((mn - obs)**2)), 
    sd = mean(sd), 
    crps = mean(crps), 
    .groups = "drop"
  ) %>% 
  dplyr::mutate(
    s2e = sd / rmse
  )

# maps would work like this
scores_map <- scores %>% 
  tidyr::drop_na() %>%
  dplyr::group_by(source, station_id) %>%
  dplyr::summarize(
    bias = mean(mn - obs),
    mn = mean(mn), 
    obs = mean(obs),
    .groups = "drop"
  ) %>%
  left_join(stations, by = "station_id")

## apparently some of the datasets have the stations sorted differently
map_cor <- scores_map %>% 
  group_by(source) %>%
  summarize(cor = cor(mn, obs))

map_cor

# Bielefeld seems to have some issues
okish <- map_cor %>%
  dplyr::filter(cor > 0.7) %>%
  dplyr::pull("source")

## some example plots with data that might be ok
library(ggplot2)

scores_mn %>%
  dplyr::filter(source %in% okish) %>%
  tidyr::pivot_longer(-c(source, step)) %>%
  ggplot(aes(x = step, y = value, col = source)) + 
  geom_line() + 
  facet_wrap(~ name, scales= "free_y", nrow = 3) + 
  theme_light()

scores_map %>%
  dplyr::filter(source %in% okish) %>%
  ggplot(aes(x = longitude, y = latitude)) + 
  borders(
    xlim = range(scores_map$longitude), 
    ylim = range(scores_map$latitude)
  ) + 
  geom_point(aes(fill = bias), pch = 21, size = 3) + 
  facet_wrap(~source) + 
  scale_fill_gradient2(low = "darkblue", high = "darkred") + 
  coord_fixed(
    ratio = 1/ cos(50/180*pi),
    xlim = range(scores_map$longitude), 
    ylim = range(scores_map$latitude)
  ) +
  theme_void()

## rank histogram
scores %>% 
  tidyr::drop_na() %>% 
  group_by(source, rank) %>%
  count() %>%
  ggplot(aes(x = rank, y = n)) + 
  geom_bar(stat = "identity") +  
  facet_wrap(~source) + 
  theme_light()


