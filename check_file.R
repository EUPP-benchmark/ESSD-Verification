library(ncdf4)

files <- c(
  paste0("data/v1.0/1_ESSD-benchmark_Bielefeld-University_AR-EMOS_v1.", 1:2, ".nc"), 
  "data/v0.1/1_ESSD-benchmark_Bielefeld-University_AR-EMOS_v1.1.nc"
)

for (file in files) {
  
  nc <- nc_open(file)
  # raw <- nc_open("data/v1.0/ESSD_benchmark_test_data_forecasts.nc")
  # 
  # for (nn in names(nc$dim)) {
  #   if (! identical(nc$dim[[nn]]$vals, raw$dim[[nn]]$vals)) print(nn)
  # }
  # 
  
  t2m <- ncvar_get(nc, "t2m")
  marginals <- lapply(seq_along(dim(t2m)), function(i) apply(t2m, i, mean, na.rm = TRUE))
  
  outfile <- paste0("~/tmp/", gsub("\\.nc", ".png", gsub("data/", "", file)))
  dir.create(dirname(outfile))
  png(outfile, width = 8, height = 5, units = 'in', res = 200)
  dimnames <- sapply(nc$var$t2m$dim, function(x) x$name)
  par(mfrow = c(2,2), oma = c(0, 0, 3, 0))
  for (i in 1:4) {
    if (dimnames[i] == "station_id") {
      hist(marginals[[i]] - 273.15, main = dimnames[i], xlab = "Mean temperature")
    } else {
      plot(marginals[[i]] - 273.15, main = dimnames[i], type = 'b', ylab = "Mean temperature")
    }
  }
  mtext(file, side = 3, outer = TRUE)
  dev.off()
}

