# Pull together SWE daily data needed for fitting and predicting

# SETUP ----
swe_path <- "data/SWE"


# COMPILE SWE DATA FOR FITTING DATASET ----
obs_data <- data.table::fread("data/response/NorWeST_obs.csv")
reach_id <- as.data.frame(unique(obs_data$COMID), drop = F); colnames(reach_id) <- "COMID"
rm(obs_data)

SWE_data <- NULL
for(y in 1993:2013){
  SWE_dat <- fst::read_fst(paste0(swe_path, "/mean_SWE_", y, ".fst"), as.data.table = T)
  SWE_dat <- SWE_dat[, c("COMID", "lookup", "SWE", "SWE_ws")]
  colnames(SWE_dat) <- c("COMID", "lookup", "cov.SWE", "cov.SWE_ws")
  SWE_dat <- merge(reach_id, SWE_dat, by = "COMID")
  SWE_data <- rbind(SWE_data, SWE_dat)
  rm(SWE_dat)
}
gc()
fst::write_fst(SWE_data, paste0(swe_path, "/SWE_fitting.fst"), compress = 100)
rm(SWE_data)


# COMPILE SWE DATA BY HUC6 FOR PREDICTION DATASET ----
COM_HUC <- data.table::fread("data/COMID_to_HUC12.csv")
COM_HUC$Huc6 <- substr(COM_HUC$Huc12, 1, 6)
huclist <- sort(unique(COM_HUC$Huc6))

# For each HUC6
for(huc in huclist){
  SWE_data <- NULL
  reach_id <- as.data.frame(unique(COM_HUC$COMID[COM_HUC$Huc6 == huc]), drop = F); colnames(reach_id) <- "COMID"
  for(y in 1990:2021){
    SWE_dat <- fst::read_fst(paste0(swe_path, "/mean_SWE_", y, ".fst"), as.data.table = T)
    SWE_dat <- SWE_dat[, c("COMID", "lookup", "SWE", "SWE_ws")]
    colnames(SWE_dat) <- c("COMID", "lookup", "cov.SWE", "cov.SWE_ws")
    SWE_dat <- merge(reach_id, SWE_dat, by = "COMID")
    SWE_data <- rbind(SWE_data, SWE_dat)
    rm(SWE_dat)
  }
  SWE_data <- SWE_data[, c("lookup", "cov.SWE", "cov.SWE_ws")]
  fst::write_fst(SWE_data, paste0(swe_path, "/SWE_", huc, ".fst"), compress = 100)
  rm(SWE_data, SWE_dat); gc()
}


