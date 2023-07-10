# Add dam information to spatial dataset

# Data sources:
# GRanD large dam database: https://sedac.ciesin.columbia.edu/data/set/grand-v1-dams-rev01
# National Inventory of Dams: https://nid.sec.usace.army.mil/#/


# SETUP ----
library(dplyr)

# LOAD DATA ----
spatial_data <- data.table::fread('data/spatial_data.csv')

# Load data from GRanD and NID dams databases (pre-processed to omit overlap)
dams <- sf::st_read("data/dams", "PNW_DAMS")

# Load NHD flowline attributed
flowline_attributed <- sf::st_read("data/shapefiles", "NHD_flowline_attributed")


# PROCESS ----

# Loop through all dams ----
dams_results <- NULL
for(d in 1:nrow(dams)){
  start_comid <- dams$COMID[d]
  if(start_comid %in% flowline_attributed$COMID){ #some dams are outside HR17
    coords <- as.numeric(dams$geometry[dams$COMID == start_comid][[1]])
    start_point <- sf::st_sfc(sf::st_point(coords))
    dscomids <- nhdplusTools::get_DM(flowline_attributed, start_comid)
    uptribs <- nhdplusTools::get_UT(flowline_attributed, start_comid)
    allcomids <- c(dscomids, uptribs)
    dams_temporary_data <- flowline_attributed[flowline_attributed$COMID %in% dscomids,c("COMID", "LENGTHKM", "Pathlength", "AreaSqKM", "TotDASqKM")]
    # distance_below_dam is the distance in km downstream from a dam
    dams_temporary_data$distance_below_dam <- flowline_attributed$Pathlength[flowline_attributed$COMID == start_comid] - dams_temporary_data$Pathlength
    # proportion_dam_influenced is the proportion of area draining to the reach that is above the dam
    dams_temporary_data$proportion_dam_influenced <- flowline_attributed$TotDASqKM[flowline_attributed$COMID == start_comid] / dams_temporary_data$TotDASqKM
    # Other attributes from dams database
    dams_temporary_data$dam_name <- dams$DAM_NAME[dams$COMID == start_comid]
    dams_temporary_data$dam_hgt_m <- dams$DAM_HGT_M[dams$COMID == start_comid]
    dams_temporary_data$dam_area_km2 <- dams$AREA_SKM[dams$COMID == start_comid]
    dams_temporary_data$dam_cap_mcm <- dams$CAP_MCM[dams$COMID == start_comid]
    dams_temporary_data$dam_dis_avg_ls <- dams$DIS_AVG_LS[dams$COMID == start_comid]
    dams_temporary_data$dam_main_use <- dams$MAIN_USE[dams$COMID == start_comid]
    dams_temporary_data <- dams_temporary_data[order(dams_temporary_data$distance_below_dam),]
    row.names(dams_temporary_data) <- NULL
    dams_results <- rbind(dams_results, dams_temporary_data)
  }
}
dams_results <- as.data.frame(dams_results); dams_results <- dams_results[,-which(colnames(dams_results) == "geometry")]

# Find reaches influenced by multiple upstream dams
# and select/keep only the record for the dam with the max proportion_dam_influenced
duplicates <- dams_results[duplicated(dams_results$COMID),]
duplicates$rowid <- row.names(duplicates)
most_influenced <- duplicates %>% group_by(COMID) %>% slice(which.max(proportion_dam_influenced)) 
dam_influenced <- dams_results[-which(dams_results$COMID %in% duplicates$COMID),]
dam_influenced <- rbind(dam_influenced, most_influenced[-ncol(most_influenced)])
dam_influenced <- as.data.frame(dam_influenced)


# Add dams data to fitting dataset ----
spatial_data <- merge(spatial_data, dplyr::select(dam_influenced, COMID, dam_name, proportion_dam_influenced, distance_below_dam,
                      dam_hgt_m, dam_main_use), by = "COMID", all.x = T)
damcolnames <- c("dam_name", "proportion_dam_influenced", "distance_below_dam", "dam_hgt_m", "dam_main_use")
colnames(spatial_data)[colnames(spatial_data) %in% damcolnames] <- paste0("cov.",damcolnames )

# EXPORT ----
write.csv(dams_results, "data/dams/dam_influenced_all.csv", row.names = F)
write.csv(dam_influenced, "data/dams/dam_influenced.csv", row.names = F)
file.copy("data/spatial_data.csv", "data/spatial_data_bu.csv", overwrite = T) #back up original file
data.table::fwrite(spatial_data, file = 'data/spatial_data.csv') # overwrites existing file




