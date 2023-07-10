# Function summarizes PRISM data for each reach on a daily basis

fncSummarizeAirTDaily <- function(i, filepath, shps, PRISM_files){

  WBD17_outline <- shps[[1]]
  PRISM_raster <- raster::raster(filepath)
  PRISM_raster <- raster::crop(PRISM_raster, WBD17_outline)
  PRISM_raster <- raster::mask(PRISM_raster, WBD17_outline)
  
  # Summarize by points for smaller RCAs (>32km^2)
  NHDv2_polygons_small <- shps[[2]]
  daily_mean_AirT <- matrix(ncol = 5, nrow = length(unique(NHDv2_polygons_small$FEATUREID)))
  colnames(daily_mean_AirT) <- c("COMID", "area_km2", "year", "date", "air_temp")
  daily_mean_AirT[,1] <- as.character(NHDv2_polygons_small$FEATUREID)
  daily_mean_AirT[,2] <- as.character(NHDv2_polygons_small$AreaSqKM)
  daily_mean_AirT[,3] <- substr(PRISM_files[i], 26, 29)
  daily_mean_AirT[,4] <- paste0(substr(PRISM_files[i], 26, 29), "-", substr(PRISM_files[i], 30, 31), "-", substr(PRISM_files[i], 32, 33))
  daily_mean_AirT[,5] <- raster::extract(PRISM_raster, NHDv2_polygons_small)
  
  # Summarize by polygon for larger RCAs (>32km^2)
  NHDv2_polygons_large <- shps[[3]]
  daily_mean_AirT_large <- matrix(ncol = 5, nrow = length(unique(NHDv2_polygons_large$FEATUREID)))
  colnames(daily_mean_AirT_large) <- c("COMID", "area_km2", "year", "date", "air_temp")
  daily_mean_AirT_large[,1] <- as.character(NHDv2_polygons_large$FEATUREID)
  daily_mean_AirT_large[,2] <- as.character(NHDv2_polygons_large$AreaSqKM)
  daily_mean_AirT_large[,3] <- substr(PRISM_files[i], 26, 29)
  daily_mean_AirT_large[,4] <- paste0(substr(PRISM_files[i], 26, 29), "-", substr(PRISM_files[i], 30, 31), "-", substr(PRISM_files[i], 32, 33))
  daily_mean_AirT_large[,5] <- sapply(raster::extract(PRISM_raster, NHDv2_polygons_large), mean)
  
  # Bring large and small polygons back together
  daily_mean_AirT <- rbind(daily_mean_AirT, daily_mean_AirT_large)
  
  return(daily_mean_AirT)
  
}

