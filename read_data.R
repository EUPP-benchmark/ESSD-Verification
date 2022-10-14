#clean the variable list
rm(list = ls())

#load the needed libraries for importing nc files
library(ncdf4)
library(ncdf4.helpers)

#Path to the data, no need to follow same approach
read_data_from_path <- function(path='/Users/aitorrent/Desktop/input/') {
  fc_test <- nc_open(paste0(path,'ESSD_benchmark_test_data_forecasts.nc'))
  station_id_fc_test <- ncvar_get(fc_test, varid = "station_id")
  fctime_fc_test<- as.POSIXct(ncvar_get(fc_test, varid = "time"), origin = '1970-01-01')
  data_fc_test <- ncvar_get(fc_test, varid = "t2m")
  lt_fc_test<- ncvar_get(fc_test, varid = "step")
  ens_fc_test<- ncvar_get(fc_test, varid = "number")
  nc_close(fc_test)
  
  obs_test <- nc_open(paste0(path,'ESSD_benchmark_test_data_observations.nc'))
  station_id_obs_test <- ncvar_get(obs_test, varid = "station_id")
  fctime_obs_test<- as.POSIXct(ncvar_get(obs_test, varid = "time"), origin = '1970-01-01')
  data_obs_test <- ncvar_get(obs_test, varid = "t2m")
  lt_obs_test<- ncvar_get(obs_test, varid = "step")
  nc_close(obs_test)
  
  list_fc <- list(
    "station_id_fc_test" = station_id_fc_test,
    "fctime_fc_test" = fctime_fc_test,
    "data_fc_test" = data_fc_test,
    "lt_fc_test" = lt_fc_test,
    "ens_fc_test" = ens_fc_test
  )
  
  list_obs <- list(
    "station_id_obs_test" = station_id_obs_test,
    "fctime_obs_test" = fctime_obs_test,
    "data_obs_test" = data_obs_test,
    "lt_obs_test" = lt_obs_test
  )
  
  my_list <- list("list_obs" = list_obs, "list_fc" = list_fc)
  return(my_list) 
}
