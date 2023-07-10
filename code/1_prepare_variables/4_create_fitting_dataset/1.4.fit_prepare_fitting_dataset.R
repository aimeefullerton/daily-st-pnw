# Create dataset of reach/day combinations used for fitting models by sequentially merging in components of previously prepared covariates


# SETUP ----
prism_path <- "data/PRISM"
swe_path <- "data/SWE"
nwm_path <- "data/NWM"

# LOAD NorWeST OBSERVATIONS ----
obs_data <- data.table::fread("data/response/NorWeST_obs.csv")
comids <- sort(unique(obs_data$COMID))
yrs_data <- range(obs_data$tim.year)
rm(obs_data)

# ADD AIR TEMPERATURE DATA ----
AirT_data <- fst::read_fst(paste0(prism_path, "/AirT_fitting.fst"), as.data.table = T)
master_data <- AirT_data[AirT_data$COMID %in% comids,-6]
rm(AirT_data)
master_data <- master_data[master_data$tim.year >= yrs_data[1] & master_data$tim.year <= yrs_data[2]]

# ADD SWE DATA ----
SWE_data <- fst::read_fst(paste0(swe_path, "/SWE_fitting.fst"), as.data.table = T)
scols <- c("lookup", "cov.SWE", "cov.SWE_ws"); SWE_data <- SWE_data[, ..scols]
# Ensure lookups are in the correct format (this can be likely removed for future iterations)
SWE_data$lookup <- gsub("_00", "_", SWE_data$lookup); SWE_data$lookup <- gsub("_0", "_", SWE_data$lookup)
master_data <- merge(master_data, SWE_data[, ..scols], by = "lookup", all.x = T)
rm(SWE_data, scols)

# ADD NWM FLOW DATA ----
NWM_data <- fst::read_fst(paste0(nwm_path, "/NWM_fitting.fst"), as.data.table = T)
qcols <- c("lookup", "cov.NWM_flow", "cov.NWM_flow_log"); NWM_data <- NWM_data[, ..qcols]
master_data <- merge(master_data, NWM_data, by = "lookup", all.x = T)
rm(NWM_data, qcols)

# ADD DAYLIGHT ----
Huc_daylight <- read.csv("data/HUC_daylight_seconds.csv")
Huc_daylight <- tidyr::gather(Huc_daylight, doy, Day_length, X001:X365, factor_key = T)
Huc_daylight$doy <- as.numeric(substr(Huc_daylight$doy, 2, 4))
Huc_daylight$HUC_doy <- paste0(Huc_daylight$HUC12, "_", Huc_daylight$doy)
colnames(Huc_daylight)[colnames(Huc_daylight) == "Day_length"] <- "cov.daylength"
COM_HUC <- data.table::fread("data/COMID_to_HUC12.csv")
master_data <- merge(master_data, COM_HUC, by = "COMID", all.x = T)
master_data$HUC_doy <- paste0(master_data$Huc12, "_", ifelse(master_data$tim.doy == 366, 365, master_data$tim.doy ))
master_data <- merge(master_data, dplyr::select(Huc_daylight, HUC_doy, cov.daylength), by = "HUC_doy", all.x = T)
master_data$cov.daylength_hours <- master_data$cov.daylength / 3600
master_data$cov.daylength[master_data$cov.daylength < 0] <- NA
master_data$cov.daylength_hours[master_data$cov.daylength_hours < 0] <- NA
rm(Huc_daylight, COM_HUC)

# ADD ANNUAL SWE METRICS ----
SWE_metrics <- fst::read_fst(paste0(swe_path, "/SWE_annual_metrics.fst")) 
master_data$COMID_year <- paste0(master_data$COMID, "_", master_data$tim.year)
scols <- c("COMID_year", "cov.SWE_mean_year", "cov.SWE_1Apr", "cov.SWE_last_doy")
SWE_metrics <- SWE_metrics[,scols]
master_data <- merge(master_data, SWE_metrics, by = "COMID_year", all.x = T) 
rm(SWE_metrics)

# ADD SPATIAL DATA ----
spatial_data <- data.table::fread("data/spatial_data.csv")
cols2keep <- c("COMID", "cov.lat_v", "cov.elev_mean_smo", "cov.elev_diff", "cov.BFI_cat", "cov.area_km2_ws", "cov.slope", 
               "cov.pct_wet_all_ws", "cov.pct_ice_ws", "cov.pct_ow_ws", "cov.pct_for_all_cat_rip100m", "cov.canopy_line", 
               "cov.pct_urb_all_ws", "cov.pct_extru_vol_ws", "cov.precip_cat",  "cov.air_temp_range_cat", "cov.area_km2_ws_log", 
               "cov.air_temp_mean_cat", "cov.elev_ws", "cov.proportion_dam_influenced")
spatial_data <- spatial_data[,..cols2keep]
master_data <- merge(master_data, spatial_data, by = "COMID", all.x = T)
rm(spatial_data)

# ADD OBSERVATIONS ----
obs_data <- data.table::fread("data/response/NorWeST_obs.csv")
obs_data <- obs_data[,c("NorWeST_ID", "cov.NorWeST_region", "lookup", "obs.stream_temp_daily_mean")]
summary(as.factor(obs_data$cov.NorWeST_region))
master_data <- merge(obs_data, master_data, by = "lookup", all.x = T)
rm(obs_data)

# EXPORT ----
fst::write_fst(master_data, "data/fitting_data.fst", compress = 80)


