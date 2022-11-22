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

#Universal variables 
zero.kelvin <- 273.15
date.init <- as.POSIXlt("1970-01-01")
Sys.setenv(TZ='GMT')
lapse.rate <- 0.0065

#PARAMETERS TO BE ADAPTED TO YOUR OWN SYSTEM
dir.fig <- "/mnt/netapp/home/bertvs/ARCHIVE_bertvs/R/EUPP/figs/"
dir.nc <- "/mnt/HDS_MEDYCLIM/MEDYCLIM/PREDANTAR/R/EUPP/benchmark/verification/" #directory with all netcdf data
dir.rdata <- "/mnt/HDS_MEDYCLIM/MEDYCLIM/PREDANTAR/R/EUPP/benchmark/rdata/" #directory where all Rdata will be stored
file.alt.correct <- paste0(dir.rdata, "model_orography_on_stations.csv")

#Fixed variables 
thres.val.lst <- seq(0.1, 1, by = 0.2)
thres.str.lst <- paste0("stat_frac_", thres.val.lst * 100)
names(thres.val.lst) <- thres.str.lst
var.ver <-"t2m" #netcdf variable for 2m temperature
metric.lst <- c("bias", "sd", "mae.em", "rmse.em", "crps2", "s2e")
pp.inst.lst <- c("MetOffice",
	"ARSO",
	"KIT",
	"ZAMG",
	"ECMWF",
	"UoH",
	"RMIB",
	"BU",
	"raw")	
pp.met.lst <- c(
	"MetOffice_IMPROVER-reliabilitycalibration-v1.3.1_v1.0",
	"ARSO_ANET_v1.0", #"ARSO_ANET_v0.9", "ARSO_ANET_v1.1",
	"KIT_simple-NN_v1.1",
	"ZAMG_EMOS-v1.1",
	"ECMWF_RepresentativnessWithBiasCorrection_test-v1.1",
	"University-of-Hildesheim_D-Vine-Copula_v1.0",
	"RMIB_Pythie-MBM-AbsCRPSmin-commit21a29a9_seasonal-v1.0",
	"Bielefeld-University_AR-EMOS_v1.0",
	"raw")

pp.met.sel.lst <- c(
	"ARSO_ANET_v1.0", #"ARSO_ANET_v0.9", "ARSO_ANET_v1.1",
	"ZAMG_EMOS-v1.1",
	"ECMWF_RepresentativnessWithBiasCorrection_test-v1.1",
	"University-of-Hildesheim_D-Vine-Copula_v1.0",
	"RMIB_Pythie-MBM-AbsCRPSmin-commit21a29a9_seasonal-v1.0")
pp.inst.sel.lst <- c(
	"ARSO", #"ARSO_ANET_v0.9", "ARSO_ANET_v1.1",
	"ZAMG",
	"ECMWF",
	"UoH",
	"RMIB")
	
pp.met.all.lst <- c(pp.met.lst, "obs")
pp.met.to.rem <- c("Bielefeld-University_AR-EMOS_v1.0")
pp.met.to.rem.dex <- which(pp.met.lst %in% pp.met.to.rem)

abs.min.temp <- -50
abs.max.temp <- 50

#Colors for plotting:
color.lst <- c("#FB1C00", "#1CFF00", "#0016FC", "#F5CACB", "#FC1CE0", "#0DFBF8", "#F6E500", 
	"#166E0D", "#004B7C", "#EC8216", "#94004B", "#D997FF", "#FB0D85", "#40FDAD", "#9A7600", 
	"#38A2FE", "#CA16FF", "#C1DBFF", "#D2EBA4", "#4B4940", "#7E2E78", "#A7E82A", "#FE80D1", 
	"#C52A1C", "#168D85", "#FB9294", "#5826AE", "#00C1EC", "#92554D", "#F9BA35", "#9DF3D3", 
	"#AF00AE", "#F822B1", "#FFC1F9", "#00AB00", "#917F9E", "#9EAD96", "#8791DA", "#FB629B", 
	"#FFD5A0", "#7FA100", "#9D62FC", "#C2729A", "#A73B2A", "#75C87F", "#667022", "#A50D7F", 
	"#C8BC49", "#EEA06A", "#FF5A00", "#FE85F9", "#FE165F", "#F416FE", "#0D5DD7", "#69FF78", 
	"#A342D0", "#6ECFDB", "#B19473", "#00F4CD", "#603B8B", "#608DA8", "#6E3853", "#DFE0E7", 
	"#B40D47", "#FB7173", "#9490FE", "#C871C0", "#D9ED5F", "#B691D2", "#1651A5", "#497E5A", 
	"#0DA378", "#A05A00", "#D10DAD", "#967679", "#E765FF", "#EADEBE", "#454760", "#2285BB", 
	"#DB9787", "#FF89B6", "#AEF286", "#FF4D77", "#87FA00", "#87229B", "#FFAFC4", "#C62682", 
	"#91C5FD", "#91B4B8", "#8A2660", "#32C7B6", "#E25D00", "#87A86C", "#8E65C6", "#326260", 
	"#D5C0E8", "#8700FE", "#BB5666", "#CA9BB7", "#BFE5BE")
	
