# Create a set of air temperature metrics averaged over different periods prior to a given date
# Add these to existing metrics of air temperature in local catchment and air temperature in upstream watershed
# for all years in one file instead of annual files
# fitting dataset has everything subset to fitting reaches
# predicting dataset has data for all reaches saved by Huc6

# SETUP ----
# Load packages
library(foreach)

# Directories
prism_path <- "data/PRISM"

# LOAD DATA ----
obs_data <- data.table::fread("data/response/NorWeST_obs.csv", showProgress = T)
reach_id <- unique(obs_data$COMID); rm(obs_data)
COM_HUC <- data.table::fread("data/COMID_to_HUC12.csv")

# COMPUTE ROLLING MEANS FOR FITTING DATASET ----

# Gather relevant fields from annual files
AirT_all_data <- NULL
for(y in 1990:2022){
  print(y)
  AirT_year_data <- fst::read_fst(paste0(prism_path, "/mean_AirT_", y, ".fst"), as.data.table = T)
  AirT_year_data <- AirT_year_data[AirT_year_data$COMID %in% reach_id, c("COMID", "year", "doy", "air_temp", "air_temp_ws")]
  AirT_all_data <- rbind(AirT_all_data, AirT_year_data)
  rm(AirT_year_data)
}
gc()

# Set up parallel processing
cores_available <- parallelly::availableCores()
UseCores <- cores_available - 4

# Register CoreCluster
cl <- parallel::makeCluster(UseCores)
doSNOW::registerDoSNOW(cl)

# Progress bar
pb <- txtProgressBar(min = 1, max = length(reach_id), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Subset and rbind data
print( Sys.time())
start_time <- Sys.time()

# Rollapply means
AirT_metrics <- foreach(i = 1:length(reach_id), .combine = rbind, .options.snow = opts, .packages = 'zoo') %dopar% try({
  library(zoo)
  AirT_results <- AirT_all_data[AirT_all_data$COMID %in% reach_id[i],]
  AirT_results <- AirT_results[with(AirT_results, order(AirT_results$year, AirT_results$doy)),]

  AirT_results$cov.air_temp_3d <- c(rep(NA, 2), zoo::rollapplyr(AirT_results$air_temp, 3, mean))
  AirT_results$cov.air_temp_6d <- c(rep(NA, 5), zoo::rollapplyr(AirT_results$air_temp, 6, mean))
  AirT_results$cov.air_temp_9d <- c(rep(NA, 8), zoo::rollapplyr(AirT_results$air_temp, 9, mean))
  AirT_results$cov.air_temp_12d <- c(rep(NA, 11), zoo::rollapplyr(AirT_results$air_temp, 12, mean))
  AirT_results$cov.air_temp_15d <- c(rep(NA, 14), zoo::rollapplyr(AirT_results$air_temp, 15, mean))
  AirT_results$cov.air_temp_18d <- c(rep(NA, 17), zoo::rollapplyr(AirT_results$air_temp, 18, mean))
  AirT_results$cov.air_temp_21d <- c(rep(NA, 20), zoo::rollapplyr(AirT_results$air_temp, 21, mean))
  AirT_results$cov.air_temp_24d <- c(rep(NA, 23), zoo::rollapplyr(AirT_results$air_temp, 24, mean))
  AirT_results$cov.air_temp_27d <- c(rep(NA, 26), zoo::rollapplyr(AirT_results$air_temp, 27, mean))
  AirT_results$cov.air_temp_30d <- c(rep(NA, 29), zoo::rollapplyr(AirT_results$air_temp, 30, mean))
  AirT_results$cov.air_temp_35d <- c(rep(NA, 34), zoo::rollapplyr(AirT_results$air_temp, 35, mean))
  AirT_results$cov.air_temp_40d <- c(rep(NA, 39), zoo::rollapplyr(AirT_results$air_temp, 40, mean))
  AirT_results$cov.air_temp_50d <- c(rep(NA, 49), zoo::rollapplyr(AirT_results$air_temp, 50, mean))
  AirT_results$cov.air_temp_60d <- c(rep(NA, 59), zoo::rollapplyr(AirT_results$air_temp, 60, mean))
  
  return(AirT_results)
})

print( Sys.time() - start_time)
close(pb)
parallel::stopCluster(cl)

AirT_metrics <- as.data.frame(AirT_metrics)
AirT_metrics$lookup <- paste0(AirT_metrics$COMID, "_", AirT_metrics$year, "_", as.numeric(AirT_metrics$doy))

# RENAME a few variables with prefix
colnames(AirT_metrics)[colnames(AirT_metrics) == "year"] <- "tim.year"
colnames(AirT_metrics)[colnames(AirT_metrics) == "doy"] <- "tim.doy"
colnames(AirT_metrics)[colnames(AirT_metrics) == "air_temp"] <- "cov.air_temp"
colnames(AirT_metrics)[colnames(AirT_metrics) == "air_temp_ws"] <- "cov.air_temp_ws"

# Export
fst::write_fst(AirT_metrics, paste0(prism_path, "/AirT_fitting.fst"), compress = 100)

gc()


# COMPUTE ROLLING MEANS FOR PREDICTION DATASET ----

# For each HUC6
COM_HUC$Huc6 <- substr(COM_HUC$Huc12, 1, 6)
huclist <- sort(unique(COM_HUC$Huc6))

for(huc in huclist){
  gc()
  reach_id <- unique(COM_HUC$COMID[COM_HUC$Huc6 == huc])

  # Gather relevant fields from annual files
  AirT_all_data <- NULL
  for(y in 1990:2022){
    print(y)
    AirT_year_data <- fst::read_fst(paste0(prism_path, "/mean_AirT_", y, ".fst"), as.data.table = T)
    AirT_year_data <- AirT_year_data[AirT_year_data$COMID %in% reach_id, c("COMID", "year", "doy", "air_temp", "air_temp_ws")]
    AirT_all_data <- rbind(AirT_all_data, AirT_year_data)
    rm(AirT_year_data)
  }

  # Add HUC 6 field
  AirT_all_data <- merge(AirT_all_data, COM_HUC, by = "COMID")
  AirT_all_data <- dplyr::select(AirT_all_data, -Huc12)
  
  
  # Set up parallel processing
  cores_available <- parallelly::availableCores()
  UseCores <- cores_available - 4
  
  # Register CoreCluster
  cl <- parallel::makeCluster(UseCores)
  doSNOW::registerDoSNOW(cl)
  
  # Progress bar
  pb <- txtProgressBar(min = 1, max = length(reach_id), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # Subset and rbind data
  print( Sys.time())
  start_time <- Sys.time()
  
  # Rollapply means
  AirT_metrics <- foreach(i = 1:length(reach_id), .combine = rbind, .options.snow = opts, .packages = 'zoo') %dopar% try({
    library(zoo)
    AirT_results <- AirT_all_data[AirT_all_data$COMID %in% reach_id[i],]
    AirT_results <- AirT_results[with(AirT_results, order(AirT_results$year, AirT_results$doy)),]

    AirT_results$cov.air_temp_3d <- c(rep(NA, 2), zoo::rollapplyr(AirT_results$air_temp, 3, mean))
    AirT_results$cov.air_temp_6d <- c(rep(NA, 5), zoo::rollapplyr(AirT_results$air_temp, 6, mean))
    AirT_results$cov.air_temp_9d <- c(rep(NA, 8), zoo::rollapplyr(AirT_results$air_temp, 9, mean))
    AirT_results$cov.air_temp_12d <- c(rep(NA, 11), zoo::rollapplyr(AirT_results$air_temp, 12, mean))
    AirT_results$cov.air_temp_15d <- c(rep(NA, 14), zoo::rollapplyr(AirT_results$air_temp, 15, mean))
    AirT_results$cov.air_temp_18d <- c(rep(NA, 17), zoo::rollapplyr(AirT_results$air_temp, 18, mean))
    AirT_results$cov.air_temp_21d <- c(rep(NA, 20), zoo::rollapplyr(AirT_results$air_temp, 21, mean))
    AirT_results$cov.air_temp_24d <- c(rep(NA, 23), zoo::rollapplyr(AirT_results$air_temp, 24, mean))
    AirT_results$cov.air_temp_27d <- c(rep(NA, 26), zoo::rollapplyr(AirT_results$air_temp, 27, mean))
    AirT_results$cov.air_temp_30d <- c(rep(NA, 29), zoo::rollapplyr(AirT_results$air_temp, 30, mean))
    AirT_results$cov.air_temp_35d <- c(rep(NA, 34), zoo::rollapplyr(AirT_results$air_temp, 35, mean))
    AirT_results$cov.air_temp_40d <- c(rep(NA, 39), zoo::rollapplyr(AirT_results$air_temp, 40, mean))
    AirT_results$cov.air_temp_50d <- c(rep(NA, 49), zoo::rollapplyr(AirT_results$air_temp, 50, mean))
    AirT_results$cov.air_temp_60d <- c(rep(NA, 59), zoo::rollapplyr(AirT_results$air_temp, 60, mean))
    
    return(AirT_results)
  })
  
  print( Sys.time() - start_time)
  close(pb)
  parallel::stopCluster(cl)
  
  AirT_metrics$lookup <- paste0(AirT_metrics$COMID, "_", AirT_metrics$year, "_", as.numeric(AirT_metrics$doy))

  # RENAME a few variables with prefix
  colnames(AirT_metrics)[colnames(AirT_metrics) == "year"] <- "tim.year"
  colnames(AirT_metrics)[colnames(AirT_metrics) == "doy"] <- "tim.doy"
  colnames(AirT_metrics)[colnames(AirT_metrics) == "air_temp"] <- "cov.air_temp"
  colnames(AirT_metrics)[colnames(AirT_metrics) == "air_temp_ws"] <- "cov.air_temp_ws"
  
  # Export
  fst::write_fst(AirT_metrics, paste0(prism_path, "/AirT_", huc, ".fst"), compress = 100)
}


  