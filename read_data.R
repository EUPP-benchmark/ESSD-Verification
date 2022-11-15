#load the needed libraries for importing nc files
library(ncdf4)
library(ncdf4.helpers)
library(dplyr)
library(tibble)
library(tidync)

#Path to the data, no need to follow same approach
read_data_from_path <- function(path='/Users/aitorrent/Desktop/input/') {
  list(
    "list_obs" = read_obs(paste0(path, "ESSD_benchmark_test_data_observations.nc")), 
    "list_fc" = read_fc(paste0(path, "ESSD_benchmark_test_data_forecasts.nc"))
  )
}



read_obs <- function(file = "ESSD_benchmark_test_data_observations.nc", 
                     varname = "t2m") {
  obs_test <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(obs_test))
  station_id_obs_test <- ncvar_get(obs_test, varid = "station_id")
  fctime_obs_test<- as.POSIXct(ncvar_get(obs_test, varid = "time"), origin = '1970-01-01')
  data_obs_test <- ncvar_get(obs_test, varid = varname)
  lt_obs_test<- ncvar_get(obs_test, varid = "step")
  
  list(
    "station_id_obs_test" = station_id_obs_test,
    "fctime_obs_test" = fctime_obs_test,
    "data_obs_test" = data_obs_test,
    "lt_obs_test" = lt_obs_test
  )
}

read_fc <- function(file = "ESSD_benchmark_test_data_forecasts.nc", 
                    varname = "t2m") {
  fc_test <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(fc_test))
  station_id_fc_test <- ncdf4::ncvar_get(fc_test, varid = "station_id")
  fctime_fc_test<- as.POSIXct(ncdf4::ncvar_get(fc_test, varid = "time"), origin = '1970-01-01')
  data_fc_test <- ncdf4::ncvar_get(fc_test, varid = varname)
  lt_fc_test<- ncdf4::ncvar_get(fc_test, varid = "step")
  ens_fc_test<- ncdf4::ncvar_get(fc_test, varid = "number")

  list(
    "station_id_fc_test" = station_id_fc_test,
    "fctime_fc_test" = fctime_fc_test,
    "data_fc_test" = data_fc_test,
    "lt_fc_test" = lt_fc_test,
    "ens_fc_test" = ens_fc_test
  )
}

## function to read forecast files with minimum sanity checks
read_file <- function(file, varname = "t2m") {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc))
  
  ## read data
  ff <- tidync::tidync(file, varname) %>% 
    tidync::hyper_tbl_cube()
  
  ## rename dimensions if necessary
  names(ff$dims) <- names(ff$dims) %>%
    gsub("forecast_reference_time", "time", .) %>%
    gsub("forecast_lead_time", "step", .) %>%
    gsub("percentile", "number", .)

  ## parse time
  tdim <- grep("time", names(nc$dim), value = TRUE) %>% 
    setdiff("forecast_lead_time")
  tunits <- nc$dim[[tdim]]$units
  if (tunits == "") {
    warning(paste0("dimension `time` in ", basename(file), " is not properly specified"))
    ff$dims$time <- as.POSIXct("2017-01-01", tz = "UTC") + 
      as.difftime(seq_along(ff$dims$time) - 1, units = 'days')
  } else {
    ff$dims$time <- as.POSIXct(gsub(".*since ", "", tunits), tz = "UTC") + 
      as.difftime(ff$dims$time, units = paste0(substr(tunits, 1, 3), "s"))
  }
  if (max(ff$dims$step) <= 21) {
    warning(paste0("dimension `step` in ", basename(file), " is not properly specified"))
    ff$dims$step <- seq(0, by = 6, length.out = length(ff$dims$step))
  }
  if (max(ff$dims$station_id) <= 229) {
    warning(paste0("dimension `station_id` in ", basename(file), " is not properly specified"))
    ff$dims$station_id <- stations$station_id[ff$dims$station_id]
  }
  ## drop the number dimension and convert to data.frame 
  ## with fcst as matrix column
  out <- expand.grid(ff$dims[setdiff(names(ff$dims), "number")]) %>%
    tibble::as_tibble()
  dimnames(ff$mets[[varname]]) <- ff$dims
  out$fcst <- ff$mets[[varname]] %>%
    aperm(c(names(out), "number")) %>%
    matrix(ncol = length(ff$dims$number))
  out
}
