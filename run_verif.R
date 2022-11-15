source("read_data.R")
source("verif_functions.R")


#Read all the data (This is also deleting the previous variables)
all_data <- read_data_from_path()

#Compute several scores as a function of lead-time and Station
ll_veri <- verif_for_st_lt(all_data)
