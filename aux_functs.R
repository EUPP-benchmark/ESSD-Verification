zero.kelvin <- 273.15
date.init <- as.POSIXlt("1970-01-01")
Sys.setenv(TZ='GMT')
##These are the abbreviations used
#tr: training
#ver: verification (test dataset)
#alt: altitude
#lu: landuse
#obs: (station) observations
#fc: forecast/model
#lt: lead time
#fig: figure
#yr: year
#mon: month
#pp: post-processing
#met: method

make.nc.file.name <- function(pp.met){
	return(paste0(dir.nc, "1_ESSD-benchmark_", pp.met, ".nc"))
}

get.meta <- function(nc.id){ #get metadata based in an id of a netcdf file. This is a function specific for the benchmark
 #get metadata
	meta <- vector("list")
	meta$alt.fc <- ncvar_get(nc.id, "model_altitude")
	meta$alt.obs <- ncvar_get(nc.id, "station_altitude")

	meta$lat.fc <- ncvar_get(nc.id, "model_latitude")
	meta$lat.obs <- ncvar_get(nc.id, "station_latitude")
	meta$lon.fc <- ncvar_get(nc.id, "model_longitude")
	meta$lon.obs <- ncvar_get(nc.id, "station_longitude")
	meta$lu.fc <-  ncvar_get(nc.id, "model_land_usage")
	meta$lu.obs <-  ncvar_get(nc.id, "station_land_usage")
	meta$stat.lst <-  ncvar_get(nc.id, "station_name")
	meta$stat.id.lst <- nc.id$dim[["station_id"]]$vals

	#get forecast-specific metadata
	meta$date.lst <- nc.id$dim[["time"]]$vals
	meta$date.lst <- date.init + meta$date.lst
	meta$date.str.lst <- as.character(meta$date.lst)
	meta$amt.date <- length(meta$date.lst)
	meta$mem.id <- paste0("M", nc.id$dim[["number"]]$vals)
	meta$amt.mem <- length(meta$mem.id)
	meta$yr.lst <- paste0("Y", nc.id$dim[["year"]]$vals)
	meta$lt.lst <- as.numeric(nc.id$dim[["step"]]$vals)
	meta$lt.str.lst <- paste0("step ", meta$lt.lst, "h")
	meta$amt.lt <- length(meta$lt.str.lst)
	return(meta)
}



spr <- function(x, amt.spr, dim = 1) { #function to extend an array along one dimension (dim), amt.spr times
  if(is.vector(x)){
    amt.dims <- 1
    if(dim == 2){
		arr.out <- array(rep(x, amt.spr), c(length(x), amt.spr))
    } else if(dim == 1){
		arr.out <- t(array(rep(x, amt.spr), c(length(x), amt.spr)))
	} else {
		stop(paste0("error in .spr: amt.dims = ",amt.dims," while dim = ",dim))
	}
  } else if(is.array(x)) {
    amt.dims <- length(dim(x))
    if(dim > amt.dims + 1){ 
		stop(paste0("error in .spr: amt.dims = ",amt.dims," while dim = ",dim))
	}
    arr.out <- array(rep(as.vector(x), amt.spr), c(dim(x), amt.spr))
    if(dim != amt.dims + 1){ 
      amt.dims.out <- amt.dims + 1
	  dims.tmp <- seq(1, amt.dims.out)
	  dims.tmp[seq(dim, amt.dims.out)] <- c(amt.dims.out, seq(dim,amt.dims.out-1))
	  arr.out <- aperm(arr.out, dims.tmp)
	}
  } else {
    stop("x is not array nor vector but is ", class(x))
  }
  return(arr.out)
}




#function to name the dimensions of a certain array
set.dim.names <- function(arr, names.dim = NULL, dim.names = NULL){
	if(!is.null(names.dim)){
		names(dim(arr)) <- names.dim
	}
	if(!is.null(dim.names)){
		dimnames(arr) <- dim.names
		names(dimnames(arr)) <- names.dim
	}
	return(arr)
}

array.dim <- function(value = NA, dimnames = NULL){
	arr <- array(value, dim = lengths(dimnames))
	names.dim = names(dimnames)
	if(!is.null(names.dim)){
		names(dim(arr)) <- names.dim
	}
	if(!is.null(dimnames)){
		dimnames(arr) <- dimnames
		names(dimnames(arr)) <- names.dim
	}
	return(arr)
}





comb.dim <- function(arr, dim.to.combine){
	dim.in <- dim(arr)
	name.dim.in <- names(dim.in)
	dim.to.combine.dex <- match(dim.to.combine, name.dim.in)
	amt.comb <- length(dim.to.combine)
	
	if(any(is.na(dim.to.combine.dex))){
		stop("Error in comb.dim: dim.to.combine not present:", 
			dim.to.combine)
	}
	tmp.dexes <- seq(1, length(dim.in))
	new.dexes <- c(tmp.dexes[-dim.to.combine.dex], 
		tmp.dexes[dim.to.combine.dex])
	new.dim.name <- paste(dim.to.combine, collapse = "_")
	new.dims <- c(dim.in[tmp.dexes[-dim.to.combine.dex]], 
								prod(dim.in[dim.to.combine.dex]))
	names(new.dims)[length(new.dims)] <- new.dim.name
	arr.out <- aperm(arr, new.dexes)
	dim(arr.out) <- new.dims
	
	dimnames.comb <- dimnames(arr)
	if(is.null(dimnames.comb)){
		dimnames.comb <- lapply(dim.in, seq)
	}
	dimnames.comb.tmp <- dimnames.comb[[dim.to.combine.dex[1]]]
	for(i.comb in seq(2, amt.comb )){
		dimnames.comb.tmp <- as.vector(outer(
			dimnames.comb.tmp,
			dimnames.comb[[dim.to.combine.dex[i.comb]]],
			paste, sep = " ")) 
	}
	
	dimnames.comb <- dimnames(arr)
	dimnames.comb[[new.dim.name]] <- dimnames.comb.tmp
	for(i.comb in seq(1, amt.comb)){
		dimnames.comb[[dim.to.combine[i.comb]]] <- NULL
	}
	
#	print(dim(arr.out))
#	print(lengths(dimnames.comb))
	
	dim(arr.out) <- lengths(dimnames.comb)
	dimnames(arr.out) <- dimnames.comb
	
	return(arr.out)
}
