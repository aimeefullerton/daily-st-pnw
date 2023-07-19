# Compile all the data needed for making predictions into a single file

# SETUP ----
prism_path <- "data/PRISM"
swe_path <- "data/SWE"
nwm_path <- "data/NWM"
dir.create("data/huc")
data_path <- "data/huc"

source("code/0_functions/fncExtractLookup.R")

# Load data
COM_HUC <- data.table::fread("data/COMID_to_HUC12.csv")
COM_HUC$Huc6 <- substr(COM_HUC$Huc12, 1, 6)
COM_HUC$Huc10 <- substr(COM_HUC$Huc12, 1, 10)
huclist <- sort(unique(COM_HUC$Huc6))
Huc_daylight <- read.csv("data/HUC_daylight_seconds.csv")
Huc_daylight <- tidyr::gather(Huc_daylight, doy, Day_length, X001:X365, factor_key = T)
Huc_daylight$doy <- as.numeric(substr(Huc_daylight$doy, 2, 4))
Huc_daylight$Huc10 <- substr(Huc_daylight$HUC12, 1, 10)
Huc_daylight$HUC_doy <- paste0(Huc_daylight$HUC12, "_", Huc_daylight$doy)
colnames(Huc_daylight)[colnames(Huc_daylight) == "Day_length"] <- "cov.daylength"

# PROCESS BY HUC ----
for(huc in huclist){
  AirT_metrics <- fst::read_fst(paste0(prism_path, "/AirT_", huc, ".fst"), as.data.table = T)
  SWE_data <- fst::read_fst(paste0(swe_path, "/SWE_", huc, ".fst"), as.data.table = T)
  NWM_data <- fst::read_fst(paste0(nwm_path, "/NWM_", huc, ".fst"), as.data.table = T)
  # Ensure lookups are in the correct format (this can be likely removed for future iterations; we had some lookups with leading zeroes for doy)
  SWE_data$lookup <- gsub("_00", "_", SWE_data$lookup); SWE_data$lookup <- gsub("_0", "_", SWE_data$lookup)
  

  huc10lst <- sort(unique(COM_HUC$Huc10[COM_HUC$Huc6 %in% huc]))
  for(huc10 in huc10lst){
    
    # ADD AIR TEMPERATURE ----
    comids <- COM_HUC$COMID[COM_HUC$Huc10 == huc10]
    at_cols <- c("COMID", "lookup", paste0("cov.air_temp", c("", "_ws", "_3d", "_6d", "_9d", "_12d", "_15d", "_18d", "_21d", "_24d", "_27d", "_30d", "_35d", "_40d", "_50d", "_60d")))
    huc_data <- AirT_metrics[AirT_metrics$COMID %in% comids, ..at_cols]
    
    # ADD HUC INFO & INDICES
    huc_data <- huc_data[!is.na(huc_data$COMID),]
    if(nrow(huc_data) == 0) next
    huc_data <- merge(huc_data, COM_HUC, by = "COMID", all.x = T)
    huc_data <- fncExtractLookup(huc_data); huc_data[,comid:=NULL]
    huc_data$HUC_doy <- paste0(huc_data$Huc12, "_", ifelse(huc_data$tim.doy == 366, 365, huc_data$tim.doy ))
    
    # ADD SWE ----
    scols <- c("lookup", "cov.SWE", "cov.SWE_ws")
    huc_data <- merge(huc_data, SWE_data[, ..scols], by = "lookup", all.x = T)
    
    # ADD NWM FLOW ----
    huc_data <- merge(huc_data, NWM_data, by = "lookup", all.x = T)
    
    # ADD DAYLIGHT ----
    huc_data <- unique(huc_data)
    huc_data <- merge(huc_data, Huc_daylight[,c("HUC_doy", "cov.daylength")], by = "HUC_doy", all.x = T)
    huc_data$cov.daylength_hours <- huc_data$cov.daylength / 3600
    huc_data$cov.daylength[huc_data$cov.daylength < 0] <- NA
    huc_data$cov.daylength_hours[huc_data$cov.daylength_hours < 0] <- NA
    
    # ADD SNOWPACK METRICS ----
    SWE_metrics <- fst::read_fst(paste0(swe_path, "/SWE_annual_metrics.fst"), as.data.table = T)
      #note: these were calculated over water years not calendar years
    huc_data$COMID_year <- paste0(huc_data$COMID, "_", huc_data$tim.year)
    SWE_metrics <- SWE_metrics[,COMID:=NULL]
    huc_data <- merge(huc_data, SWE_metrics, by = "COMID_year", all.x = T) 
    
    # REDUCE COLUMNS TO SAVE
    cols2rmv <- which(colnames(huc_data) %in%c("COMID_year", "HUC_doy", "Huc12", "Huc6", "water_year"))
    huc_data <- huc_data[,-..cols2rmv]
    
    # EXPORT ----
    fst::write_fst(huc_data, paste0(data_path, "/huc_", huc10, ".fst"), compress = 90)
    gc()
  }
}
