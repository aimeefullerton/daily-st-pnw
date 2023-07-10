# Calculate SWE Metrics used later to classify hydrological year types
#
# NOTE: SWE data netCDF files are in water year format (1 Oct through 30 Sep)


# SETUP ----

# Directories
plot.dir <- "plots"
swe_path <- "data/SWE"

# Load packages
library(dplyr)

# Year range
min_year <- 1990
max_year <- 2021

# PROCESS ----

# Create dataframe for SWE metric results
SWE_all <- as.data.frame(matrix(nrow = 0, ncol = 7))
colnames(SWE_all) <- c("COMID", "water_year","cov.SWE_mean_year", "cov.SWE_1Apr", "cov.SWE_last_doy")

# Loop over years
start <- Sys.time()
for(y in min_year:max_year){
  print(y)
  SWE_year_data <- fst::read_fst(paste0(swe_path, "/mean_SWE_", y, ".fst"))
  # Create water year variables starting on October 1st of prior year and ending on September 30th
  SWE_year_data$doy_wy <- ifelse(SWE_year_data$doy > min(subset(SWE_year_data, year == y - 1)$doy) - 1, 
                                 SWE_year_data$doy - min(subset(SWE_year_data, year == y - 1)$doy) + 1, 
                                 SWE_year_data$doy + max(SWE_year_data$doy) - min(subset(SWE_year_data, year == y - 1)$doy) + 1)
  # Fix leap year
  SWE_year_data$doy_wy[SWE_year_data$doy_wy == 1 & SWE_year_data$doy == 274] <- 366
  SWE_results_df <- as.data.frame(matrix(nrow = length(unique(SWE_year_data$COMID)), ncol = 2))
  colnames(SWE_results_df) <- c("COMID", "water_year")
  SWE_results_df$COMID <- unique(SWE_year_data$COMID)
  SWE_results_df$water_year <- y
  
  # Mean annual SWE
  Mean_SWE_df <- as.data.frame.table(tapply(SWE_year_data$SWE_ws, SWE_year_data$COMID, mean, na.rm = T))
  SWE_results_df <- merge(SWE_results_df, Mean_SWE_df, by.x = "COMID", by.y = "Var1", all.x = T)
  
  # April 1 SWE
  SWE_results_df <- merge(SWE_results_df, dplyr::select(subset(SWE_year_data, doy == 91), COMID, SWE_ws), by = "COMID", all.x = T)
 
  # Last day of SWE
  SWE_year_data_wSnow <- subset(SWE_year_data, SWE_ws > 0)
  SWE_year_data_doywsMAX <- as.data.frame.table(tapply(SWE_year_data_wSnow$doy_wy, SWE_year_data_wSnow$COMID, max, na.rm = T))
  SWE_results_df <- merge(SWE_results_df, dplyr::select(SWE_year_data_doywsMAX, Var1, Freq), by.x = "COMID", by.y = "Var1", all.x = T) 
  rm(SWE_year_data_wSnow, SWE_year_data_doywsMAX)
  colnames(SWE_results_df) <- c("COMID", "water_year","cov.SWE_mean_year", "cov.SWE_1Apr","cov.SWE_last_doy")
  
  SWE_all <- rbind(SWE_all, SWE_results_df)
}
Sys.time() - start 

SWE_all$COMID_year <- paste0(SWE_all$COMID, "_", SWE_all$water_year)
fst::write_fst(SWE_all, paste0(swe_path, "/SWE_annual_metrics.fst"), compress = 100)
