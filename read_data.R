#load the needed libraries for importing nc files
library(ncdf4)
library(ncdf4.helpers)

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

# there is quite some diversity in forecast files
# therefore we could resort to 'assuming' that dimensions
# are in the correct order
read_file <- function(file, varname = "t2m") {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc))
  ncvar_get(nc, varname)
}


