library(verification)
library(SpecsVerification)
library(zoo)
library(tibble)
library(dplyr)


compute_scores <- function(file, obs, list_of_functions = NULL) {
  message(paste0("Compute scores on ", basename(file)))
  if (is.null(list_of_functions)) {
    list_of_functions <- list(
      mn = function(ens, obs) rowMeans(ens),
      sd = function(ens, obs) sqrt(rowSums((ens - rowMeans(ens))**2) / (ncol(ens) - 1)),
      crps = function(ens, obs) SpecsVerification::EnsCrps(ens, obs),
      rank = function(ens, obs) rowSums(ens < obs)
    )
  }
  
  df <- read_file(file) %>%
    dplyr::left_join(obs, by = c("step", "time", "station_id"))
  for (nn in names(list_of_functions)) {
    message(paste0("  computing ", nn))
    df[[nn]] <- list_of_functions[[nn]](df$fcst, df$obs)
  }
  df %>%
    dplyr::select(-fcst) %>%
    dplyr::mutate(
      source = basename(file) %>%
        gsub(".*ESSD.benchmark_", "", .) %>%
        gsub(".nc", "", .)
    )
}


verif_for_st_lt <- function(all_data) {
  
  station_list <- all_data$list_fc$station_id_fc_test
  leadtime_list <- all_data$list_fc$lt_fc_test

  mat <- NULL
  
  for(i_st in seq_along(station_list)){
    st <- station_list[i_st]
    print(paste0('Forecast at Station: ', st))
    
    for(i_lt in seq_along(leadtime_list)){
      lt <- leadtime_list[i_lt]
      print(paste0('Forecast at Lead Time: ', lt))
      
      obs <- all_data$list_obs$data_obs_test[which(all_data$list_obs$lt_obs_test == lt), , which(all_data$list_obs$station_id_obs_test == st)]
      time <- all_data$list_obs$fctime_obs_test + lt * 3600
      obs <- cbind(all_data$list_obs$fctime_obs_test, lt, st, obs)
      obs <- zoo(obs, time)
      colnames(obs) <- c('init', 'lt', 'stat', 'obs')
      # 
      fc <- all_data$list_fc$data_fc_test[, which(all_data$list_fc$lt_fc_test == lt), , which(all_data$list_fc$station_id_fc_test == st)]
      ens.mu <- apply(fc, 2, mean)
      ens.md <- apply(fc, 2, median)
      ens.sd <- apply(fc, 2, sd)
      test <- cbind(t(fc), ens.mu, ens.md, ens.sd)
      
      time <- all_data$list_fc$fctime_fc_test + lt * 3600
      test <- zoo(test, time)
      colnames(test)[1] <- c('ens.cr')
      colnames(test)[2:dim(fc)[1]] <- paste0('ens.n',seq(2,dim(fc)[1]))
      
      #After selecting lead-time and station we have the data to verified merged
      data <- merge(obs, test)
      
      scores = verif_from_data(data)
      
      if (is.null(mat)) {
        #Nº stations - Nº lead-time - Nº scores
        mat <- array(0, dim=c(length(station_list),
                              length(leadtime_list),
                              length(names(scores))))
      }
      
      mat[i_st,i_lt,] <- unlist(scores, use.names = FALSE)
  
    }
  }
  
  return(list("station_list" = station_list,
              "leadtime_list" = leadtime_list,
              "scores_list" = names(scores),
              "values" = mat))
}

verif_from_data <- function(data) {

  mae.crtl <- mean(abs(data$obs - data$ens.cr), na.rm = TRUE)
  mae.mu <- mean(abs(data$obs - data$ens.mu), na.rm = TRUE)
  mae.mu <- mean(abs(data$obs - data$ens.md), na.rm = TRUE)
  
  bias.crtl <- mean((data$obs - data$ens.cr), na.rm = TRUE)
  bias.mu <- mean((data$obs - data$ens.mu), na.rm = TRUE)
  bias.md <- mean((data$obs - data$ens.md), na.rm = TRUE)
  
  mse.crtl <- mean((data$obs - data$ens.cr)^2, na.rm = TRUE)
  mse.mu <- mean((data$obs - data$ens.mu)^2, na.rm = TRUE)
  mse.md <- mean((data$obs - data$ens.md)^2, na.rm = TRUE)
  
  #This line will only work with 50 ensemble + control in these columns
  crps.ens <- crpsDecomposition(coredata(data$obs),coredata(data[,5:55]))$CRPS

  return(list("mae.crtl" = mae.crtl,
              "mae.mu" = mae.mu,
              "mae.mu" = mae.mu,
              "bias.crtl" = bias.crtl,
              "bias.mu" = bias.mu,
              "bias.md" = bias.md,
              "mse.crtl" = mse.crtl,
              "mse.mu" = mse.mu,
              "mse.md" = mse.md,
              "crps.ens" = crps.ens))
}

