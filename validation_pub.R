rm(list = ls())
library(ncdf4) #opening ncdf files
library(abind) #binding arrays

#Before you start:
#1) Please see also aux_functs.R for auxiliary functions and to see abbreviations used  here
#2)  I renamed ESSD_benchmark_test_data_observations.nc -> 1_ESSD-benchmark_obs.nc and put it in same folder as post-processed data
#3)  I renamed ESSD_benchmark_test_data_forecasts.nc -> 1_ESSD-benchmark_raw.nc and put it in same folder as post-processed data
#4) Please change folders below

#PARAMETERS TO BE ADAPTED TO YOUR OWN SYSTEM
dir.fig <- "/mnt/netapp/home/bertvs/ARCHIVE_bertvs/R/EUPP/figs/"
dir.nc <- "/mnt/HDS_MEDYCLIM/MEDYCLIM/PREDANTAR/R/EUPP/benchmark/verification/" #directory with all netcdf data
dir.rdata <- "/mnt/HDS_MEDYCLIM/MEDYCLIM/PREDANTAR/R/EUPP/benchmark/rdata/" #directory where all Rdata will be stored
current.dir <- "/mnt/netapp/home/bertvs/ARCHIVE_bertvs/R/EUPP/bench/" #current directory

#loading auxiliary functions and constats:
source(paste0(current.dir, "aux_functs.R")) 

#Fixed variables 
var.ver <-"t2m" #netcdf variable for 2m temperature
pp.met.lst <- c(
	"MetOffice_IMPROVER-reliabilitycalibration-v1.3.1_v1.0",
	"ARSO_ANET_v1.0", #"ARSO_ANET_v0.9", "ARSO_ANET_v1.1",
	"KIT_simple-NN_v1.0",
	"ZAMG_EMOS-v1.1",
	"ECMWF_RepresentativnessWithBiasCorrection_test-v1.0",
	"University-of-Hildesheim_D-Vine-Copula_v1.0",
	"RMIB_Pythie-MBM-AbsCRPSmin-commit21a29a9_seasonal-v1.0",
	"raw") #calibration methods. This includes the "raw" forecast. Please add methods when they become available.
pp.met.all.lst <- c(pp.met.lst, "obs")

#STEP 1) get metadata by extracting it from the raw (uncorrected) forecast
file.fc.ver <- make.nc.file.name("raw")
id.fc.ver <- nc_open(file.fc.ver)
meta.ver <- get.meta(id.fc.ver)



#initialize 
temp.verif.ver <- array.dim(dimnames = list(method = pp.met.all.lst, 
			member = meta.ver$mem.id, leadtime = meta.ver$lt.str.lst, 
			 date = meta.ver$date.str.lst,  station = meta.ver$stat.lst))

cat("opening data from calibration method:\n")
for(pp.met.to.use in pp.met.all.lst){
	cat(pp.met.to.use, "\n")
	file.to.use <- make.nc.file.name(pp.met.to.use)
	nc.id <- nc_open(file.to.use)	
	temp.tmp <- ncvar_get(nc.id, var.ver) - zero.kelvin
	if(pp.met.to.use == "obs"){
		temp.tmp <- spr(temp.tmp, meta.ver$amt.mem, 1)
	}
	aperm.dex <- match(as.numeric(dim(temp.verif.ver)[2:5]), dim(temp.tmp)) #The arrays must be permuted since they seem to be randomly stored
	temp.tmp <- aperm(temp.tmp, aperm.dex)
	temp.verif.ver[pp.met.to.use, , , , ] <- temp.tmp
	rm(temp.tmp)
	nc_close(nc.id)
}
gg
#Save all the data and the metadata
file.to.save <- paste0(dir.rdata, "all_pp.Rdata")
save(file = file.to.save,  meta.ver, temp.verif.ver)
