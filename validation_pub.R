rm(list = ls())
library(ncdf4) #opening ncdf files
library(abind) #binding arrays

#Before you start:
#1) Please see also aux_functs.R for auxiliary functions and to see abbreviations used  here
#2)  I renamed ESSD_benchmark_test_data_observations.nc -> 1_ESSD-benchmark_obs.nc and put it in same folder as post-processed data
#3)  I renamed ESSD_benchmark_test_data_forecasts.nc -> 1_ESSD-benchmark_raw.nc and put it in same folder as post-processed data
#4) Please change folders below


#loading auxiliary functions and constats:
current.dir <- "/mnt/netapp/home/bertvs/ARCHIVE_bertvs/R/EUPP/bench/" #current directory
source(paste0(current.dir, "aux_functs.R")) 
source(paste0(current.dir, "aux_params.R")) 

#STEP 1) get metadata by extracting it from the raw (uncorrected) forecast
file.fc.ver <- make.nc.file.name("raw")
id.fc.ver <- nc_open(file.fc.ver)
meta.ver <- get.meta(id.fc.ver)
meta.ver <- correct.alt(meta = meta.ver, file.csv = file.alt.correct)


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
	} else if(pp.met.to.use == "raw"){
		#correction to the right station elevation
		for(i.stat in seq(1, length(meta.ver$stat.lst))){
			temp.tmp[ , , , i.stat] <- temp.tmp[ , , , i.stat] -
				(meta.ver$alt.obs[i.stat] - meta.ver$alt.fc[i.stat]) * lapse.rate
		}
	}
	aperm.dex <- match(as.numeric(dim(temp.verif.ver)[seq(2, 5)]), dim(temp.tmp)) #The arrays must be permuted since they seem to be randomly stored
	temp.tmp <- aperm(temp.tmp, aperm.dex)
	temp.verif.ver[pp.met.to.use, , , , ] <- temp.tmp
	rm(temp.tmp)
	nc_close(nc.id)
}

temp.verif.ver[ temp.verif.ver < abs.min.temp] <- abs.min.temp
temp.verif.ver[ temp.verif.ver > abs.max.temp] <- abs.max.temp

#Save all the data and the metadata
file.to.save <- paste0(dir.rdata, "all_pp.Rdata")
save(file = file.to.save,  meta.ver, temp.verif.ver)
rm(meta.ver, temp.verif.ver)
