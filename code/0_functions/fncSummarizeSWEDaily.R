# Function summarizes SWE data for each reach on a daily basis

fncSummarizeSWEDaily <- function(swe_path, i, y, shps){
  
  # Open annual data for whole country
  SWE_data <- ncdf4::nc_open(paste0(swe_path, "/4km_SWE_Depth_WY", y, "_v01.nc"))
  
  # Extract variables: lat, lon, SWE, time
  lon <- ncdf4::ncvar_get(SWE_data, varid = "lon")
  lat <- ncdf4::ncvar_get(SWE_data, varid = "lat")
  tas_time <- ncdf4.helpers::nc.get.time.series(SWE_data, v = "tas", time.dim.name = "time")
  time_index <- which(format(tas_time, "%Y-%m-%d") == tas_time[i])
  
  if(length(time_index) > 0){ # needed for leap years
    date <- tas_time[i]
    year <- as.numeric(substr(date, 1,4))
    jd <- as.numeric(julian(date, origin = as.POSIXct(paste0(year, "-01-01")))) + 1
    SWE <- ncdf4.helpers::nc.get.var.subset.by.axes(SWE_data, "SWE", axis.indices = list(T = time_index))[,,1]
  
    # Create raster grid from extracted values
    grid <- expand.grid(lon, lat)
    colnames(grid) <- c("lon", "lat")
    grid$lon <- ifelse(grid$lon > 180, -(360 - grid$lon), grid$lon)
    grid$SWE <- as.vector(SWE)
  
    # Create raster
    dummy_raster <- raster::raster(nrow = length(unique(grid$lat)), ncol = length(unique(grid$lon)),
                                   xmx = max(grid$lon), xmn = min(grid$lon), ymn =  min(grid$lat), ymx =  max(grid$lat),
                                   crs = "+proj=longlat +datum=NAD83 +no_defs")
    SWE_raster <- raster::rasterize(grid[,1:2], dummy_raster, grid[,3], fun = mean)
  
    # Cut out a rectangle of WBD17
    WBD17_outline <- shps[[1]]
    SWE_raster_crop <- raster::crop(SWE_raster, WBD17_outline)
  
  
    # Summarize by points for smaller RCAs (<32km^2)
    NHDv2_polygons_small <- shps[[2]]
    daily_mean_SWE <- matrix(ncol = 5, nrow = length(unique(NHDv2_polygons_small$FEATUREID)))
    colnames(daily_mean_SWE) <- c("COMID", "area_km2", "year", "doy", "SWE")
    daily_mean_SWE[,1] <- as.character(NHDv2_polygons_small$FEATUREID)
    daily_mean_SWE[,2] <- as.character(NHDv2_polygons_small$AreaSqKM)
    daily_mean_SWE[,3] <- year
    daily_mean_SWE[,4] <- jd
    daily_mean_SWE[,5] <- raster::extract(SWE_raster_crop, NHDv2_polygons_small)
  
    # Summarize by polygon for larger RCAs (>32km^2)
    NHDv2_polygons_large <- shps[[3]]
    daily_mean_SWE_large <- matrix(ncol = 5, nrow = length(unique(NHDv2_polygons_large$FEATUREID)))
    colnames(daily_mean_SWE_large) <- c("COMID", "area_km2", "year", "doy", "SWE")
    daily_mean_SWE_large[,1] <- as.character(NHDv2_polygons_large$FEATUREID)
    daily_mean_SWE_large[,2] <- as.character(NHDv2_polygons_large$AreaSqKM)
    daily_mean_SWE_large[,3] <- year
    daily_mean_SWE_large[,4] <- jd
    daily_mean_SWE_large[,5] <- sapply(raster::extract(SWE_raster_crop, NHDv2_polygons_large), mean)
  
    # Bring large and small polygons back together
    daily_mean_SWE <- rbind(daily_mean_SWE, daily_mean_SWE_large)
  
    return(daily_mean_SWE)
  }
  ncdf4::nc_close(SWE_data)
  
}

