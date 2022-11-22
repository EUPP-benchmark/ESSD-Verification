rm(list = ls())
library(ncdf4) #opening ncdf files
library(abind) #binding arrays
library(RColorBrewer)
library(rasterVis)
library(fields)

thres.stat <- 0.5 #Stations with a fraction of observations below this threshold are removed, at least when use.sel.na = T
use.sel.na <- F #When true, some stations are removed from station averages.
redo.calc.score <- F

#loading auxiliary functions and constats:
current.dir <- "/mnt/netapp/home/bertvs/ARCHIVE_bertvs/R/EUPP/bench/" #current directory
source(paste0(current.dir, "aux_functs.R")) #auxiliary variables defined here
source(paste0(current.dir, "aux_params.R"))  #auxiliary parameters defined here


# get metadata by extracting it from the raw (uncorrected) forecast
file.fc.ver <- make.nc.file.name("raw")
id.fc.ver <- nc_open(file.fc.ver)
meta.ver <- get.meta(id.fc.ver)
meta.ver <- correct.alt(meta = meta.ver, file.csv = file.alt.correct)
alt.stat.dex <- order(meta.ver$alt.obs) 

#Load or calculate the fraction of available data per stations
file.to.use <- paste0(dir.rdata, "percentage_of_nan_in_t2m.Rdata")
if(!file.exists(file.to.use)){
	file.obs.tr <- paste0(dir.nc.tr, "ESSD_benchmark_training_data_observations.nc")
	file.fc.tr <- paste0(dir.nc.tr, "ESSD_benchmark_training_data_forecasts.nc")
	id.fc.tr <- nc_open(file.fc.tr)
	meta.tr <- get.meta(id.fc.tr)
	meta.tr <- correct.alt(meta = meta.tr, file.csv = file.alt.correct)
	id.obs.tr <- nc_open(file.obs.tr)	
	temp.obs.tr <- ncvar_get(id.obs.tr, var.ver) - zero.kelvin
	temp.fc.tr <- ncvar_get(id.fc.tr, var.ver) - zero.kelvin
	temp.obs.tr <- spr(temp.obs.tr, meta.tr$amt.mem, 1)
	temp.arr <- temp.obs.tr * temp.fc.tr
	#stat.ok2 <- apply(temp.obs.tr, c( 5), function(x){mean(!is.na(x))})
	stat.ok <- apply(apply(temp.arr, c(2, 5), function(x){mean(!is.na(x))}), c(2), min, na.rm = T)
	#stat.ok2 <- apply(apply(temp.obs.tr, c(2, 5), function(x){mean(!is.na(x))}), c(2), min, na.rm = T)
	names(stat.ok) <- meta.tr$stat.lst
	rm(temp.obs.tr, temp.arr, temp.fc.tr)
	nc_close(id.obs.tr)
	save(file = file.to.use,  stat.ok)
	file.csv <- paste0(dir.rdata, "percentage_of_nan_in_t2m_bvs.csv")
	df.tmp <- data.frame(station_name = meta.tr$stat.lst, station_id = meta.tr$stat.id.lst, fraction = as.numeric(stat.ok)) 
	write.csv(file.csv, df.tmp)
} else {
	load(file = file.to.use)
}


#Select stations for which at least one PP method did not have any values. These stations will taken out in case of station averages:
file.to.use <- paste0(dir.rdata, "stations_not_post-processed.Rdata")
if(file.exists(file.to.use)){
	load(file = file.to.use)
} else {
	file.to.open <- paste0(dir.rdata, "all_pp.Rdata")
	load(file = file.to.open)
	stat.rem.msk <- apply(apply(temp.verif.ver, c(1, 5), function(x){all(is.na(x))}), 2, any)
	save(file = file.to.use,  stat.rem.msk)
	rm(temp.verif.ver)
}
#Deterimine which stations to use when calculating station averages:
stat.sel <- !stat.rem.msk
stat.sel.lst <- meta.ver$stat.lst[stat.sel]

#Calculate scores
file.to.use <- paste0(dir.rdata, "scores_all.Rdata")
if(!file.exists(file.to.use) | redo.calc.score){
	file.to.open <- paste0(dir.rdata, "all_pp.Rdata")
	load(file = file.to.open)
	score.all <- array.dim(dimnames = list(metric = metric.lst, method = pp.met.lst, 
				leadtime = meta.ver$lt.str.lst, station = meta.ver$stat.lst))
	for(metric.to.use in metric.lst){
		cat("Recalculating metric:", metric.to.use, "\n")
		metric.fun.to.use <- eval(parse(text = paste0("calc.", metric.to.use)))
		score.all[metric.to.use, , , ] <- apply(temp.verif.ver, 
			c("leadtime", "station"), calc.score, 
			score = metric.fun.to.use)
	}
	score.ss.all <- score.all
	for(pp.met.to.use in pp.met.lst){
		score.ss.all[ , pp.met.to.use, , ] <- 1 - score.all[ , pp.met.to.use, , ] / score.all[ , "raw", , ]
	}
	save(score.all, score.ss.all, file = file.to.use)
	rm(temp.verif.ver)
} else {
	load(file = file.to.use)
}

#Recalculate the s2e score
score.all["s2e", , , ] <- score.all["sd", , , ] / score.all["rmse.em", , , ]
for(pp.met.to.use in pp.met.lst){
	score.ss.all[ "s2e", pp.met.to.use, , ] <- 1 - score.all[ "s2e", pp.met.to.use, , ] / score.all[ "s2e", "raw", , ]
}

score.all[is.infinite(score.all)] <- NA
score.ss.all[is.infinite(score.ss.all)] <- NA
file.str <- "show_stats_"

for(metr.to.use in metric.lst){
	#av.st <- apply(score.all[metr.to.use, , , ], c(1, 2), mean, na.rm = T)
	av.lt <- apply(score.all[metr.to.use, -pp.met.to.rem.dex, , ], c(1, 3), mean, na.rm = T)
	av.lt <- set.dim.names(av.lt, names.dim = c("method", "station"), 
		dim.names = list(pp.inst.lst[-pp.met.to.rem.dex], meta.ver$stat.lst))
	
	av.lt.ss <- apply(score.ss.all[metr.to.use, -pp.met.to.rem.dex, , ], c(1, 3), mean, na.rm = T)
	av.lt.ss <- set.dim.names(av.lt.ss, names.dim = c("method", "station"), 
		dim.names = list(pp.inst.lst[-pp.met.to.rem.dex], meta.ver$stat.lst))
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, ".pdf")
	plot.2D(arr = av.lt[ , alt.stat.dex], file = file.fig, margin.below = 0)
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_sel.pdf")
	plot.2D(arr = av.lt[ pp.inst.sel.lst , alt.stat.dex], file = file.fig, margin.below = 0)
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_ss.pdf")
	plot.2D(arr = av.lt.ss[ , alt.stat.dex], file = file.fig, margin.below = 0, zlim.hard = c(-2, 1))
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_ss_sel.pdf")
	plot.2D(arr = av.lt.ss[ pp.inst.sel.lst , alt.stat.dex], file = file.fig, margin.below = 0, zlim.hard = c(-2, 1))
}


file.str <- "show_lead_time_"
for(metr.to.use in metric.lst){
	#av.st <- apply(score.all[metr.to.use, , , ], c(1, 2), mean, na.rm = T)
	av.lt <- apply(score.all[metr.to.use, -pp.met.to.rem.dex, , stat.sel.lst], c(1, 2), mean, na.rm = T)
	av.lt <- set.dim.names(av.lt, names.dim = c("method", "lead.time"), 
		dim.names = list(pp.inst.lst[-pp.met.to.rem.dex], meta.ver$lt.lst))
	
	av.lt.ss <- apply(score.ss.all[metr.to.use, -pp.met.to.rem.dex, , stat.sel.lst], c(1, 2), mean, na.rm = T)
	av.lt.ss <- set.dim.names(av.lt.ss, names.dim = c("method", "lead.tim"), 
		dim.names = list(pp.inst.lst[-pp.met.to.rem.dex], meta.ver$lt.lst))
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, ".pdf")
	double.panel.matplot(arr = t(av.lt), file = file.fig, col.run.lst = list(run = c("obs"), col = c("#000000")))
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_sel.pdf")
	double.panel.matplot(arr = t(av.lt[ pp.inst.sel.lst , ]), file = file.fig, col.run.lst = list(run = c("obs"), col = c("#000000")))
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_ss.pdf")
	double.panel.matplot(arr = t(av.lt.ss), file = file.fig) #, zlim.hard = c(-2, 1)
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_ss_sel.pdf")
	double.panel.matplot(arr = t(av.lt.ss[ pp.inst.sel.lst , ]), file = file.fig)
}

#Plot dependence on threshold to remove station data
score.all.thres <- array.dim(dimnames = list(metric = metric.lst, method = pp.inst.lst, 
			leadtime = meta.ver$lt.str.lst, threshold = thres.str.lst))
score.ss.all.thres <- array.dim(dimnames = list(metric = metric.lst, method = pp.inst.lst, 
			leadtime = meta.ver$lt.str.lst, threshold = thres.str.lst))
for(thres.to.use in thres.str.lst){
	stat.sel.to.use <- (stat.ok > thres.val.lst[thres.to.use]) & !stat.rem.msk
	score.all.thres[ , , , thres.to.use] <- apply(score.all[ , , , stat.sel.to.use], c(1, 2, 3), mean, na.rm = T)
	score.ss.all.thres[ , , , thres.to.use] <- apply(score.ss.all[ , , , stat.sel.to.use], c(1, 2, 3), mean, na.rm = T)
}
file.str <- "show_threshold_"
for(metr.to.use in metric.lst){
	#av.st <- apply(score.all[metr.to.use, , , ], c(1, 2), mean, na.rm = T)
	av.lt <- score.all.thres[ metr.to.use, -pp.met.to.rem.dex, , ]
	av.lt.ss <- score.ss.all.thres[ metr.to.use, -pp.met.to.rem.dex, ,] 
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, ".pdf")
	plot.arr(arr = av.lt, file = file.fig, col.run.lst = list(run = c("obs"), col = c("#000000")), perm = c(2, 1, 3))
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_sel.pdf")
	plot.arr(arr = av.lt[ pp.inst.sel.lst , , ], file = file.fig, col.run.lst = list(run = c("obs"), col = c("#000000")), perm = c(2, 1, 3))
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_ss.pdf")
	plot.arr(arr = av.lt.ss, file = file.fig, perm = c(2, 1, 3)) 
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_ss_sel.pdf")
	plot.arr(arr = av.lt.ss[ pp.inst.sel.lst , , ], file = file.fig, perm = c(2, 1, 3))
	
	
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_bis.pdf")
	plot.arr(arr = av.lt, file = file.fig, col.run.lst = list(run = c("obs"), col = c("#000000")), perm = c(2, 3, 1))
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_sel_bis.pdf")
	plot.arr(arr = av.lt[ pp.inst.sel.lst , , ], file = file.fig, col.run.lst = list(run = c("obs"), col = c("#000000")), perm = c(2, 3, 1))
	
	file.fig <- paste0(dir.fig, file.str, metr.to.use, "_ss_bis.pdf")
	plot.arr(arr = av.lt.ss, file = file.fig, perm = c(2, 3, 1)) 
	
	file.fig <- paste0(dir.fig, file.str,  metr.to.use, "_ss_sel_bis.pdf")
	plot.arr(arr = av.lt.ss[ pp.inst.sel.lst , , ], file = file.fig, perm = c(2, 3, 1))
}
