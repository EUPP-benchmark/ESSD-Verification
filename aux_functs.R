make.nc.file.name <- function(pp.met){
	return(paste0(dir.nc, "1_ESSD-benchmark_", pp.met, ".nc"))
}

get.meta <- function(nc.id){ #get metadata based in an id of a netcdf file. This is a function specific for the benchmark
 #get metadata
	meta <- vector("list")
	
	#station metadata
	meta$stat.lst <-  ncvar_get(nc.id, "station_name")
	meta$stat.id.lst <- nc.id$dim[["station_id"]]$vals
	names(meta$stat.id.lst) <- meta$stat.lst
	names(meta$stat.lst) <- meta$stat.id.lst
	meta$amt.stat <- length(meta$stat.lst)
	meta$alt.fc <- ncvar_get(nc.id, "model_altitude")
	meta$alt.obs <- ncvar_get(nc.id, "station_altitude")
	meta$lat.fc <- ncvar_get(nc.id, "model_latitude")
	meta$lat.obs <- ncvar_get(nc.id, "station_latitude")
	meta$lon.fc <- ncvar_get(nc.id, "model_longitude")
	meta$lon.obs <- ncvar_get(nc.id, "station_longitude")
	meta$lu.fc <-  ncvar_get(nc.id, "model_land_usage")
	meta$lu.obs <-  ncvar_get(nc.id, "station_land_usage")
	names(meta$alt.fc) <-  meta$stat.lst
	names(meta$alt.obs) <-  meta$stat.lst
	names(meta$lat.fc) <-  meta$stat.lst
	names(meta$lat.obs) <-  meta$stat.lst
	names(meta$lon.fc) <-  meta$stat.lst
	names(meta$lon.obs) <-  meta$stat.lst
	names(meta$lu.fc) <-   meta$stat.lst
	names(meta$lu.obs) <-   meta$stat.lst

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

correct.alt <- function(meta, file.csv){
	alt.new <- read.csv(file.csv)
	dex.alt <- match(meta$stat.id, alt.new$station_id)
	meta$alt.fc[] <- alt.new$orography[dex.alt]
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

calc.score <- function(obs.fc, score){
	#Input should be matrix, first dimension includes observation and then ensemble members
	obs.dex <- which(dimnames(obs.fc)[[1]] %in% c("obs", "OBS"))
	obs <-  obs.fc[obs.dex, , ]
	fc <- obs.fc[-obs.dex, , ]
	amt.met <- dim(fc)[1]
	score.out <- array(NA, c(amt.met))
	for(i.met in seq(1, amt.met)){
		score.out[i.met] <- score(obs, fc[i.met, , ])
	}
	return(score.out)
}

calc.crps2 <- function(obs, fc){ 
	amt.mbr <- dim(fc)[1]
	repmat1.tmp <- spr(fc, amt.mbr, 1) #spr(fc, amt.mbr)
	repmat2.tmp <- aperm(repmat1.tmp, c(2, 1, 3))
	spr.abs <- apply(abs(repmat1.tmp - repmat2.tmp), c(3), mean, na.rm = T)
	crps.out <- mean(apply(abs(obs - fc), c(2), mean, na.rm = T) - spr.abs / 2., na.rm = T)
	return(crps.out)
}

calc.bias <- function(obs, fc){
	return(mean((fc - obs), na.rm = T))
}

calc.mae <- function(obs, fc){
	return(mean(abs(obs - fc), na.rm = T))
}

calc.mae.em <- function(obs, fc){
	return(mean(abs(apply(fc, 2, mean, na.rm = T) - obs[1, ]), na.rm = T))
}


calc.rmse.em <- function(obs, fc){
	return(sqrt(mean((apply(fc, 2, mean, na.rm = T) - obs[1, ])^2, na.rm = T)))
}

calc.sd <- function(obs, fc){
	return(mean(apply(fc, 2, sd, na.rm = T), na.rm = T))
}


calc.s2e <- function(obs, fc){
	return(mean(apply(fc, 2, sd, na.rm = T) / abs(apply(fc, 2, mean, na.rm = T) - obs[1, ]), na.rm = T))
}





plot.2D <- function(arr, 
	file, 
	label.x = NULL, 
	label.y = NULL, 
	col.scale = "YlOrRdTheme",
	amt.col = 30,
	zlim = NULL,
	zlim.hard = NULL,
	exceed.na = T,
	margin.below = 4,
	margin.left = 4){
		
	amt.x <- dim(arr)[1]
	amt.y <- dim(arr)[2]
	
	
	if(is.null(label.x)){
		label.x <- dimnames(arr)[[1]]
	}
	if(is.null(label.y)){
		label.y <- dimnames(arr)[[2]]
	}

	
	
	if(all(is.na(arr))){
		cat("all NA:", file, "\n")
		return()
	}
	min.max <- find.min.max(arr, zlim)
	
	if(!is.null(zlim.hard)){
		min.max$zlim.min <- zlim.hard[1]
		min.max$zlim.max <- zlim.hard[2]
		if(exceed.na){
			arr[arr < min.max$zlim.min] <- NA # min.max$zlim.min
			arr[arr > min.max$zlim.max] <- NA #min.max$zlim.max
		} else {
			arr[arr < min.max$zlim.min] <- min.max$zlim.min
			arr[arr > min.max$zlim.max] <- min.max$zlim.max
		}
	}
	max.min.max <- max(abs(min.max$zlim.min), abs(min.max$zlim.max), na.rm = T)
	
	if((min.max$zlim.min * min.max$zlim.max < 0.)){
		pos.and.neg.msk <- T
	} else {
		pos.and.neg.msk <- F
	}
	

	color <- set.color(pos.and.neg.msk, col.scale, max.min.max, min.max, amt.col)
	col.to.use <- color$col
	theme.to.use <- color$theme
	at.to.use <- c(color$at, color$at[length(color$at)] * 2)
	lake.river.col <- color$lake.river.col
	colorkey.to.use <- list(at = at.to.use, col = col.to.use)
	
	pdf(file = file) #pdf
	par(mar=c(5.1 + margin.below, 4.1 + margin.left, 4.1, 2.1))
	
	
	if(pos.and.neg.msk){
		image.plot(arr, 
			axes = FALSE,
			col = col.to.use,
			breaks = at.to.use,
			lab.breaks = NULL,
			zlim = zlim) #, col = palette
	} else {
		image.plot(arr, 
		axes = FALSE,
		lab.breaks = NULL,
		zlim = zlim) 
	}
	#image(arr, col = palette, axes = FALSE, add = T)
	axis(side = 1, 
		at = seq(0, 1, length = amt.x), 
		las = 2,
		labels = label.x, 
		cex.axis = 1, 
		lwd = 1,
		tck = -.02) #pos=1.15
	axis(side = 2, 
		at = seq(0, 1, length = amt.y), 
		las = 2,
		labels = label.y, 
		cex.axis = 1.2, 
		lwd = 1,
		tck=-.02)  #pos=-0.25
	dev.off()
}	



find.min.max <- function(arr, zlim){
	zlim.min <- min(arr, na.rm = T)
	zlim.max <- max(arr, na.rm = T)
	
	if(!is.null(zlim)){
		if(is.vector(zlim)){
			zlim.min <- min(zlim[1], zlim.min, na.rm = T)
			zlim.max <- max(zlim[2], zlim.max, na.rm = T)
		} else if (is.array(zlim)){
			zlim.min <- min(zlim, min(zlim.min, na.rm = T), na.rm = T)
			zlim.max <- max(zlim, max(zlim.max, na.rm = T), na.rm = T)
		} 
	}
	return(list(zlim.min = zlim.min, zlim.max = zlim.max))
}



set.color <- function(pos.and.neg.msk, 
	col.scale, 
	max.min.max, min.max,
	amt.col){
	lake.river.col <- "lightblue"
	amt.colors.to.use <- amt.col
	if(pos.and.neg.msk){
		amt.colors.to.use <- 22
		if(col.scale == "pr"){
			palette.tmp <- colorRampPalette(
				c(rgb2hex(84, 48, 5), 
					rgb2hex(245, 245, 245), 
					rgb2hex(0, 60, 48)),
				space = "rgb")
			col.pal.tmp <- palette.tmp(amt.colors.to.use)
		} else if(col.scale == "tas"){
			palette.tmp <- colorRampPalette(
				c(rgb2hex(103, 0, 31), 
					rgb2hex(247, 247, 247), 
					rgb2hex(5, 48, 97)),
				space = "rgb")
			col.pal.tmp <- rev(palette.tmp(amt.colors.to.use))
		} else {
			palette.tmp <- colorRampPalette(
				brewer.pal(n = 11, name = "RdBu"), space = "rgb") 
			col.pal.tmp <- palette.tmp(amt.colors.to.use)
			col.pal.tmp <- rev(col.pal.tmp)
		}
		#col.pal.tmp <- brewer.pal(n = amt.colors.to.use, name = "RdBu")		
		#col.pal.tmp <- c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975")
		seq.pal <- seq(- max.min.max, max.min.max, 
			length.out = amt.colors.to.use)
		seq.pal.red.dex <- which(seq.pal >=  min.max$zlim.min & 
			seq.pal <=  min.max$zlim.max)
		
		col.to.use <- col.pal.tmp[seq.pal.red.dex]
		theme.to.use <- rasterTheme(region = col.to.use)
		at.to.use <- seq.pal[seq.pal.red.dex]
		lake.river.col <- "grey"
	#cloudTheme <- rasterTheme(region = brewer.pal(n = 3, "Blues"))
	} else if (col.scale == "pr") {
		palette <- colorRampPalette(
			c(rgb2hex(84, 48, 5), 
			rgb2hex(245, 245, 245), 
			rgb2hex(0, 60, 48)),
			space = "rgb")
		col.to.use <- palette(amt.colors.to.use)
		theme.to.use <- rasterTheme(region = col.to.use)
		at.to.use <- seq(min.max$zlim.min, min.max$zlim.max, 
			length = amt.colors.to.use)
	} else if (col.scale == "tas") {
		palette <- colorRampPalette(
			c(rgb2hex(103, 0, 31), 
			rgb2hex(247, 247, 247), 
			rgb2hex(5, 48, 97)),
			space = "rgb")
		col.to.use <- rev(palette(amt.colors.to.use))
		
		theme.to.use <- rasterTheme(region = col.to.use)
		at.to.use <- seq(min.max$zlim.min, min.max$zlim.max, length = amt.colors.to.use)
	} else if (col.scale == "brown") {
		palette <- colorRampPalette(c("white","brown"), space = "rgb")
		col.to.use <- palette(amt.colors.to.use)
		theme.to.use <- rasterTheme(region = col.to.use)
		at.to.use <- seq(min.max$zlim.min, min.max$zlim.max, length = amt.colors.to.use)
	} else if (col.scale == "white_yellow_red") {
		palette <- colorRampPalette(c("white", "orange", "red"), space = "rgb")
		col.to.use <- palette(amt.colors.to.use)
		theme.to.use <- rasterTheme(region = col.to.use)
		at.to.use <- seq(min.max$zlim.min, min.max$zlim.max, length = amt.colors.to.use)
	} else if (col.scale == "brown_circ") {
		palette <- colorRampPalette(c("darkgrey","lightyellow","brown","darkgrey"), space = "rgb")
		col.to.use <- palette(amt.colors.to.use)
		theme.to.use <- rasterTheme(region = col.to.use)
		at.to.use <- seq(min.max$zlim.min, min.max$zlim.max, length = amt.colors.to.use)
	} else if (col.scale == "grey") {
		grey.palette <- colorRampPalette(c("white","black"), space = "rgb")
		col.to.use <- grey.palette(amt.colors.to.use)
		theme.to.use <- rasterTheme(region = col.to.use)	
		at.to.use <- seq(min.max$zlim.min, min.max$zlim.max, length = amt.colors.to.use)
	} else if (col.scale == "blue") {
		palette <- colorRampPalette(c("white","navyblue"), space = "rgb")
		col.to.use <- palette(amt.colors.to.use)
		theme.to.use <- rasterTheme(region = col.to.use)	
		at.to.use <- seq(min.max$zlim.min, min.max$zlim.max, length = amt.colors.to.use)
	} else if (col.scale %in% c("Set1", "Set2", "Set3", "cat")) {
		#amt.colors.to.use <- 9
		#col.to.use <-  brewer.pal(n = amt.colors.to.use, name = "Set1")
		col.to.use <- color.lst[seq(1, amt.col)]
		theme.to.use <- rasterTheme(region = col.to.use)	
		at.to.use <- seq(min.max$zlim.min, min.max$zlim.max, length = amt.colors.to.use)
		lake.river.col <- "grey"
	} else if (col.scale %in% c("OrPu")) {
		amt.colors.to.use <- 11
		col.to.use <- rev(brewer.pal(n = amt.colors.to.use, "PuOr"))
		theme.to.use <- rasterTheme(region = col.to.use)	
		at.to.use <- seq(min.max$zlim.min, min.max$zlim.max, length = amt.colors.to.use)
		lake.river.col <- "grey"
	} else {
		#col.pal <- colorRampPalette(c("lightyellow", "black"))
		col.pal <- colorRampPalette(c("white","brown"), space = "rgb")
		#col.pal <- colorRampPalette(c('#f0f3ff','#0033BB'))
		col.to.use <- col.pal(amt.colors.to.use)
		theme.to.use <- col.scale
		at.to.use <- seq(min.max$zlim.min, min.max$zlim.max, length = amt.colors.to.use)
	}
	

	list.out <- list(
		col = col.to.use, 
		theme = theme.to.use,
		at = at.to.use, 
		lake.river.col = lake.river.col)
		
	return(list.out)
}





double.panel.matplot <- function(
		arr, 
		file, 
		name.line = NULL, 
		labels.x = NULL, 
		ylim = NULL,
		xlab = NULL,
		ylab = NULL,
		main = NULL,
		col.run.lst = list(run = c("OBS"), col = c("#000000"))
		){
	#Here I make a more elaborate figure and print it to a pdf. 
	pdf(file = file, paper = "special", width = 10, height = 5)
	lty.to.use <- 1 #line type
	lwd.to.use <- 2 #line width
	pch.to.use <- 0 #symbol type
	cex.to.use <- 0.8 #magnification (1 is no magnification)
	type.to.use = "b" #indicator to use both symbols and lines
	if(is.null(ylim)){
		ylim <- c(min(arr, na.rm = T), max(arr, na.rm = T))
	}
	amt.line <- dim(arr)[2]
	amt.x <- dim(arr)[1]
	x.seq <- seq(1, amt.x)
	
	par(mfrow = c(1, 2)) #two windows: one for the legend and one for the figure
	par(xpd = F) 
	#plot legend:
	
	if(is.null(name.line)){
		name.line <- dimnames(arr)[[2]]
	}
	col.to.use <- color.lst[seq(1, amt.line)] #select colors
	lwd.to.use <- rep(lwd.to.use, amt.line)
	amt.col.run <- length(col.run.lst[["run"]])
	if(amt.col.run != 0){
		for(i.col.run in seq(1, amt.col.run)){
			run.sel.tmp <- col.run.lst[["run"]][i.col.run]
			col.sel.tmp <- col.run.lst[["col"]][i.col.run]
			run.dex <- which(grepl(run.sel.tmp, name.line))
			if(length(run.dex) != 0){
				col.to.use[run.dex] <- col.sel.tmp
				lwd.to.use[run.dex] <- lwd.to.use[run.dex] + 2
			}
		}
	}
	
	plot(NULL, xaxt = 'n', yaxt = 'n', bty = 'n', ylab = '', xlab = '', xlim = 0:1, ylim = 0:1)
	
	
	if(is.null(labels.x)){
		labels.x <- dimnames(arr)[[1]]
	}
	if(is.null(xlab) & !is.null(names(dim(arr))[[1]])){
		xlab <- names(dim(arr))[[1]]
	}
	if(is.null(ylab) & !is.null(names(dim(arr))[[2]])){
		ylab <- names(dim(arr))[[2]]
	}
	name.line <- gsub("ECMWF-ERAINT_r1i1p1_", "", name.line)
	name.line <- gsub("ECMWF-ERA5_r1i1p1_", "ERA5", name.line)
	legend("topleft", legend = name.line, col = col.to.use, 
				lty = lty.to.use, pch = pch.to.use,  #lwd = lwd.to.use, 
				cex = cex.to.use, xpd=TRUE, bty="n")			
	par(xpd = F)
	#plot figure:
	 
	matplot(x.seq, arr,
			col = col.to.use,
			lty = lty.to.use, lwd = lwd.to.use, pch = pch.to.use, type = type.to.use,
			ylim = ylim,
			xlab = xlab, ylab = ylab, 
			main = main,
			axes = F)

	axis(side = 1, at = x.seq, labels = labels.x, las = 2, cex.axis = cex.to.use)	
	axis(side = 2, las = 1, cex.axis = cex.to.use)   
	dev.off()
}





plot.arr <- function(arr, 
	file.name.pdf, 
	met.to.use = "raw", 
	type.to.use = "l", 
	perm = NULL, 
	vert.line = NULL, 
	hor.line = NULL, 
	title.add = NULL,
	lim.use = "all",
	col.line = NULL,
	show.av = F,
	ncol.legend = 1,
	cex.legend = 1.2,
	lwd.all = 3,
	col.run.lst = list(run = c("OBS"), col = c("#000000"))){
#arr = scores.mon.stat.cat.series[ , , "ENS_AV", 1, , ]
	
		
	if(all(is.na(arr))){
		cat("warning, empty figure", file.name.pdf, "\n")
		return()
	}
	if(!is.null(perm)){
		arr <- aperm(arr, perm)
	}
	arr[is.infinite(arr)] <- NA
	dim.in <- dim(arr)
	name.dim <- names(dim(arr))
	dimnames.in <- dimnames(arr)
	
	amt.dim <- array(1, c(4))
	name.dim <- array(" ", c(4))
	dimname <- vector("list", 4)
	#name.fig <- array(" ", c(1, 1))
	for (i.dim in seq(1, length(dim.in))){
		amt.dim[i.dim] <- dim.in[i.dim]
		name.dim[i.dim] <- name.dim[i.dim]
		dimname[[i.dim]] <- dimnames.in[[i.dim]]
	}
	amt.dim[3] <- amt.dim[3] + 1
	
	if(length(dim.in) == 3){
		if(dim.in[3] > 4){
			#cat(amt.dim[3], 4 + 1,  ((dim.in[3] - 1) %/% 4) + 1)
			amt.dim[3] <-  4 + 1
			amt.dim[4] <- ((dim.in[3] - 1) %/% 4) + 1
			name.fig <- dimname[[3]]
			#dim(name.dim) <- c(amt.dim[3], amt.dim[4])
			#amt.dim[3] <- 1 + 1
			#amt.dim[4] <-  dim.in[3]
			 #array(dimname[[3]], c(1, dim.in[3]))
		} else {	
			amt.dim[3] <- dim.in[3] + 1
			amt.dim[4] <- 1
			name.fig <- array(dimname[[3]], c(amt.dim[3], 1))
		}
	} else if(length(dim.in) == 4){
		name.fig <- outer(dimname[[3]], dimname[[4]], paste, sep = " ")
	}
	if(is.null(col.line)){
		if(amt.dim[2] < (length(color.lst) + 1)){
			color.lst.now <- color.lst[seq(1, amt.dim[2])]
			lwd.to.use <- rep(lwd.all, amt.dim[2])
			amt.col.run <- length(col.run.lst[["run"]])
			if(amt.col.run != 0){
				for(i.col.run in seq(1, amt.col.run)){
					run.sel.tmp <- col.run.lst[["run"]][i.col.run]
					col.sel.tmp <- col.run.lst[["col"]][i.col.run]
					run.dex <- which(grepl(run.sel.tmp, dimname[[2]]))
					if(length(run.dex) != 0){
						color.lst.now[run.dex] <- col.sel.tmp
						lwd.to.use[run.dex] <- lwd.all + 2
					}
				}
			}
			#lwd.to.use <- 1
		} else {
			color.lst.now <- "#C0C0C0" #
			lwd.to.use <- 1
		}
	} else if(col.line == "grey" | amt.dim[2] > length(color.lst)){ 
		color.lst.now <- "#C0C0C0" #
		lwd.to.use <- 1
	} else {
		stop(paste0("unknown line color choice: ", col.line, "\n"))
	}
	pdf(file.name.pdf, paper = "special", width = 4 * amt.dim[3], height = 4 * amt.dim[4])
	par(mfrow = c(amt.dim[4], amt.dim[3]))
	legend.msk = T
	
	
	lty.to.use <- 1
	pch.to.use <- 0
	cex.to.use <- 1.5
	mod.str.lst <- gsub("ECMWF-ERAINT_r1i1p1_", "", dimname[[2]])
	mod.str.lst <- gsub("ECMWF-ERA5_r1i1p1_", "ERA5_", mod.str.lst)
	if(length(mod.str.lst) > 39){
		ncol.legend <- 1 + (length(mod.str.lst) %/% 41)
		cex.legend <- 0.5
	} else if(length(mod.str.lst) > 35){
		cex.legend <- 0.6
	} else if(length(mod.str.lst) > 30){
		cex.legend <- 0.7
	} else if(length(mod.str.lst) > 25){
		cex.legend <- 0.75
	} else if(length(mod.str.lst) > 20){
		cex.legend <- 0.8
	}
	
	for(i.vert in seq(1, amt.dim[4])){
		plot(NULL, xaxt = 'n', yaxt = 'n', bty = 'n', ylab = '', xlab = '', xlim = 0:1, ylim = 0:1)
		#legend("right", legend = dimname[[2]], col = color.lst.now, 
		#	lty = lty.to.use, lwd = lwd.to.use, pch = pch.to.use, cex = cex.to.use) #, type = type.to.use
		legend("topleft", legend = mod.str.lst, cex = cex.legend,
				   col = color.lst.now, lty = lty.to.use, pch = pch.to.use, lwd = lwd.to.use,
				   xpd=TRUE, bty="n", ncol = ncol.legend)
		for(i.hor in seq(1, amt.dim[3] - 1)){
			par(xpd = F)
			if(length(dim.in) == 2){
				arr.to.plot <- arr
				arr.minmax <- arr
			} else if(length(dim.in) == 3){
				if(i.hor + (amt.dim[3] - 1) * (i.vert - 1) > dim(arr)[3]){
					arr.to.plot <- NA
				} else{
					arr.to.plot <- arr[ , , i.hor + (amt.dim[3] - 1) * (i.vert - 1)]
					if(lim.use == "all"){
						arr.minmax <- arr
					} else if(lim.use == "hor"){
						arr.minmax <- arr[ , , seq(1, amt.dim[3] - 1) + amt.dim[3] * (i.vert - 1)]
					} else if(lim.use == "vert"){
						arr.minmax <- arr[ , , i.hor + (amt.dim[3] - 1) * (seq(1, amt.dim[4]) - 1)]
					} else if(lim.use == "sep"){
						arr.minmax <- arr.to.plot
					}
				}
			} else if(length(dim.in) == 4){
				arr.to.plot <- arr[ , , i.hor, i.vert]
				if(lim.use == "all"){
					arr.minmax <- arr
				} else if(lim.use == "hor"){
					arr.minmax <- arr[ , , , i.vert]
				} else if(lim.use == "vert"){
					arr.minmax <- arr[ , , i.hor, ]
				} else if(lim.use == "sep"){
					arr.minmax <- arr.to.plot
				}
			}
			if(all(is.na(arr.to.plot))){
				plot(0,type='n',axes=FALSE)
			} else {
			
				ylim.min <- min(arr.minmax, hor.line, na.rm = T)
				ylim.max <- max(arr.minmax, hor.line, na.rm = T)
				
				matplot(x = seq(1, amt.dim[1]), y = arr.to.plot,
					type = type.to.use,
					col = color.lst.now, xlab = name.dim[1], 
					ylab = met.to.use, ylim = c(ylim.min, ylim.max),
					cex.main = cex.to.use, cex.lab = cex.to.use,
					lty = lty.to.use, pch = pch.to.use, lwd = lwd.to.use, 
					main = paste0(toupper(name.fig[ i.hor + (amt.dim[3] -1) * (i.vert - 1)]), " ", title.add),
					axes = F)
				if(show.av){
					matplot(x = seq(1, amt.dim[1]), y = apply(arr.to.plot, c(1), mean, na.rm = T),
						type = type.to.use, lwd = 3,
						col = "black", add = T)
				}
				axis(side = 2, las=1, cex.axis = cex.to.use)
				axis(side = 1, at = seq(1, amt.dim[1]), labels = dimname[[1]], las = 2, cex.axis = cex.to.use) #, 
				#text(x = seq(1, amt.xaxis), y = par("usr")[3] - 0.5, 
				#	labels = xaxis.names, srt = 60, adj = 1, xpd = TRUE, cex = 1.2)
					
				if(!is.null(vert.line)){
					for(i.line in seq(1, length(vert.line))){
						abline(v = vert.line[i.line], lwd = 2, lty = 1)
					}
				}
				if(!is.null(hor.line)){
					for(i.line in seq(1, length(hor.line))){
						abline(h = hor.line[i.line], lwd = 2, lty = 2)
					}
				}
			}
		}
	}
	dev.off()	
}
