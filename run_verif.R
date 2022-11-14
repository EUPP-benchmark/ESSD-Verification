source("read_data.R")
source("verif_functions.R")

#Read all the data (This is also deleting the previous variables)
all_data <- read_data_from_path()

#Compute several scores as a function of lead-time and Station
ll_veri <- verif_for_st_lt(all_data)



# here's an alternative take that works independently of 
# missing dimension specification in input data
# there is no metadata (time, station_id, etc.) for now!
scores <- to_data_frame(
  fc = read_file("1_ESSD-benchmark_ARSO_ANET_v1.1.nc"),
  obs = read_file("ESSD_benchmark_test_data_observations.nc")
) %>% 
  compute_scores()
