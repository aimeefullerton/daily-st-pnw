# Streamflow from NOAA's National Water Model
# Data source: https://water.noaa.gov/about/nwm
# Processed into this format for us by Morgan.Bond@noaa.gov


# SETUP ----

# Directories
nwm_path <- "data/NWM"

# PROCESS NWM FLOWS INTO EXPECTED FORMAT ----
leaplist <- c(1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)

for(y in 1990:2022){
  print(y)
  NWM_year_data <- data.table::fread(paste0(nwm_path, "/WR17_flow_V2_1_", y ,".csv"), showProgress = F)[,-1]
  ifelse(y %in% leaplist,
         NWM_year_data <- tidyr::gather(NWM_year_data, doy, NWD_flow, day_1:day_366, factor_key = T),
         NWM_year_data <- tidyr::gather(NWM_year_data, doy, NWD_flow, day_1:day_365, factor_key = T))
  NWM_year_data$doy <- sub("day_","", NWM_year_data$doy)
  NWM_year_data$year <- y
  NWM_year_data$lookup <- paste0(NWM_year_data$COMID, "_", y, "_", as.numeric(NWM_year_data$doy))
  fst::write_fst(NWM_year_data, paste0(nwm_path, "/NWM_", y, ".fst"), compress = 100)
  rm(NWM_year_data)
}

# COMPILE NWM FLOW DATA FOR FITTING DATASET ----
obs_data <- data.table::fread("data/response/NorWeST_obs.csv")
reach_id <- as.data.frame(unique(obs_data$COMID), drop = F); colnames(reach_id) <- "COMID"
rm(obs_data)

NWM_data <- NULL
for(y in 1993:2013){
  NWM_dat <- fst::read_fst(paste0(nwm_path, "/NWM_", y, ".fst"), as.data.table = T)
  NWM_dat <- NWM_dat[, c("COMID", "lookup", "NWD_flow")]
  colnames(NWM_dat) <- c("COMID", "lookup", "cov.NWM_flow")
  NWM_dat <- merge(reach_id, NWM_dat, by = "COMID")

  # Add Logged Flow
  NWM_dat$cov.NWM_flow[NWM_dat$cov.NWM_flow == 0] <- 0.00001
  NWM_dat$cov.NWM_flow_log <- log(NWM_dat$cov.NWM_flow)
  NWM_data <- rbind(NWM_data, NWM_dat)
  rm(NWM_dat)
}
gc()
fst::write_fst(NWM_data, paste0(nwm_path, "/NWM_fitting.fst"), compress = 100)
rm(NWM_data)


# COMPILE NWM FLOW DATA BY HUC6 FOR PREDICTION DATASET ----
COM_HUC <- data.table::fread("data/COMID_to_HUC12.csv")
COM_HUC$Huc6 <- substr(COM_HUC$Huc12, 1, 6)
huclist <- sort(unique(COM_HUC$Huc6))

# For each HUC6
for(huc in huclist){
  reach_id <- as.data.frame(unique(COM_HUC$COMID[COM_HUC$Huc6 == huc]), drop = F); colnames(reach_id) <- "COMID"
  NWM_data_all <- NULL
  for(y in 1990:2022){
    NWM_dat <- fst::read_fst(paste0(nwm_path, "/NWM_", y, ".fst"), as.data.table = T)
    NWM_dat <- NWM_dat[, c("COMID", "lookup", "NWD_flow")]
    colnames(NWM_dat) <- c("COMID", "lookup", "cov.NWM_flow")
    NWM_dat <- merge(reach_id, NWM_dat, by = "COMID")
    # Add Logged Flow
    NWM_dat$cov.NWM_flow[NWM_dat$cov.NWM_flow == 0] <- 0.00001
    NWM_dat$cov.NWM_flow_log <- log(NWM_dat$cov.NWM_flow)
    NWM_data <- rbind(NWM_data_all, NWM_dat)
    rm(NWM_dat)
  }
  NWM_data_all <- NWM_data_all[, c("lookup", "cov.NWM_flow", "cov.NWM_flow_log")]
  fst::write_fst(NWM_data, paste0(nwm_path, "/NWM_", huc, ".fst"), compress = 100)
  gc()
}


