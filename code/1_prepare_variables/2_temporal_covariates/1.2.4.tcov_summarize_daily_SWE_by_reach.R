# Summarize Snow Water Equivalent (SWE) by NHDPlusV2 reach-contributing area
# data from SNODAS https://nsidc.org/data/nsidc-0719/versions/1
# collated versions downloaded from the following (requires the user to register):
# https://cmr.earthdata.nasa.gov/search/concepts/C1609344161-NSIDCV0.html; DOI: 10.5067/0GGPB220EX6A

# SETUP ----
# Load packages
library(foreach)

# Directories
swe_path <- "data/SWE"
prism_path <- "data/PRISM" # needed to get coordinates
plot.dir <- "plots"

# LOAD DATA ----

# Load region 17 for clipping rasters
WBD17_outline <- rgdal::readOGR("data/shapefiles/WBD17_outline.shp")

# Load spatial polygons for summarizing rasters
NHDv2_polygons  <- rgdal::readOGR("data/shapefiles/Catchment.shp")

# PREPARE SPATIAL DATA ----

# Ensure that polygons are in same coordinate reference system as rasters
PRISM_raster <- raster::raster(paste0(prism_path, "/PRISM_tmean_stable_4kmD2_20000101_20001231_bil/PRISM_tmean_stable_4kmD2_20000101_bil.bil"))
crs <- PRISM_raster@crs
NHDv2_polygons <- sp::spTransform(NHDv2_polygons, crs) #changing to match raster
WBD17_outline <- sp::spTransform(WBD17_outline, crs) #ditto

# Simplify NHD polygon for fast processing
NHDv2_polygons <- NHDv2_polygons[,c("FEATUREID", "AreaSqKM")]
NHDv2_simplified <- sp::coordinates(NHDv2_polygons)
NHDv2_simplified <- sp::SpatialPointsDataFrame(coords = NHDv2_simplified, data = NHDv2_polygons@data)
NHDv2_simplified@proj4string <- sp::CRS(projargs = "+proj=longlat +datum=NAD83 +no_defs")

# Use point layer for smaller reach contributing areas (RCAs) and polygon-summarization for larger RCAs
# cutoff is 32 km^2, which represents 2 low resolution pixel layers
NHDv2_polygons_small <- NHDv2_simplified[NHDv2_simplified$AreaSqKM < 32,]
NHDv2_polygons_large <- NHDv2_simplified[NHDv2_simplified$AreaSqKM >= 32,]

# PARALLEL PROCESSING ----

## * Set up parallel processing
cores_available <- parallelly::availableCores()

# Increase memory limits
raster::rasterOptions(maxmemory = 3e+9)
raster::rasterOptions(chunksize = 2e+9)

# Define how many cores to use
UseCores <- cores_available -2

# Set year range
min_year <- 1990
max_year <- 2021

# * ANNUAL LOOP ----

# Register CoreCluster
cl <- parallel::makeCluster(UseCores)
doParallel::registerDoParallel(cl)

for(y in min_year:max_year){
  print(y)
  start <- Sys.time()
  print(start)
  
  # ** Daily loop ----
  daily_mean_SWE <- foreach(i = 1:366) %dopar% {
    source("code/0_functions/fncSummarizeSWEDaily.R")
    fncSummarizeSWEDaily(swe_path, i, y, shps = list(WBD17_outline, NHDv2_polygons_small, NHDv2_polygons_large))
  }
  
  # Combine list of tables to one annual table
  last <- ifelse(is.null(dim(daily_mean_SWE[[366]])), 365, 366)
  j <- 1
  mean_SWE_table <- matrix(ncol = 5, nrow = nrow(daily_mean_SWE[[1]]) * last)
  for(i in 1:last){
    mean_SWE_table[j: (j + nrow(daily_mean_SWE[[i]]) - 1),] <- daily_mean_SWE[[i]]
    j <- j + nrow(daily_mean_SWE[[i]])
  }
  colnames(mean_SWE_table) <- c("COMID", "area_km2", "year", "doy", "SWE")
  
  # Add lookup
  mean_SWE_table <- as.data.frame(mean_SWE_table)
  mean_SWE_table$date <- as.Date(as.numeric(mean_SWE_table$doy) - 1, origin = as.Date(paste0(mean_SWE_table$year, "-01-01")))
  mean_SWE_table$lookup <- paste0(mean_SWE_table$COMID, "_", mean_SWE_table$year, "_", as.numeric(mean_SWE_table$doy)) #strip extra 0 at start of doy
  
  # Save
  fst::write_fst(mean_SWE_table, paste0(swe_path, "/mean_SWE_", y, ".fst"), compress = 100)
  
  end <- Sys.time()
  print(difftime(end, start))
    
}

parallel::stopCluster(cl)


# VISUAL EVALUATION ----
# Make plots to ensure that the process worked
my.palette <- (RColorBrewer::brewer.pal(n = 9, name = "YlGnBu"))

# Choose year to examine
y <- 2011
mean_SWE_y <- fst::read_fst(paste0(swe_path, "/mean_SWE_", y, ".fst"), as.data.table = T)

# Subset to specific day
doy <- 40
mean_SWE_y_doy <- mean_SWE_y[mean_SWE_y$doy == doy,]

# Merge subset with the spatial polygons
NHDv2_polygons2 <- sp::merge(NHDv2_polygons, mean_SWE_y_doy, by.x = "FEATUREID", by.y = "COMID", all.x = T)
NHDv2_polygons2 <- NHDv2_polygons2[!is.na(NHDv2_polygons2$SWE),]
NHDv2_polygons2$SWE <- as.numeric(as.character(NHDv2_polygons2$SWE))

png(file = paste0(plot.dir, "/SWE_byHUC_", y, "_", doy, ".png"), height = 5, width = 6, units = "in", res = 400)
 sp::spplot(NHDv2_polygons2, "SWE", main = "SWE summarized by polygons", col.regions = my.palette, cuts = 7, col = "transparent")
dev.off()


# Plot from original raster for comparison
SWE_data <- ncdf4::nc_open(paste0(swe_path, "/4km_SWE_Depth_WY", y, "_v01.nc"))
lon <- ncdf4::ncvar_get(SWE_data, varid = "lon")
lat <- ncdf4::ncvar_get(SWE_data, varid = "lat")
tas_time <- ncdf4.helpers::nc.get.time.series(SWE_data, v = "tas", time.dim.name = "time")
time_index <- doy
SWE <- ncdf4.helpers::nc.get.var.subset.by.axes(SWE_data, "SWE", axis.indices = list(T = time_index))[,,1]
grid <- expand.grid(lon, lat)
colnames(grid) <- c("lon", "lat")
grid$lon <- ifelse(grid$lon > 180, -(360 - grid$lon), grid$lon)
grid$SWE <- as.vector(SWE)
dummy_raster <- raster::raster(nrow = length(unique(grid$lat)), ncol = length(unique(grid$lon)),
                               xmx = max(grid$lon), xmn = min(grid$lon), ymn =  min(grid$lat), ymx =  max(grid$lat),
                               crs = "+proj=longlat +datum=NAD83 +no_defs")
SWE_raster <- raster::rasterize(grid[,1:2], dummy_raster, grid[,3], fun = mean)
SWE_raster_crop <- raster::crop(SWE_raster, WBD17_outline)
SWE_raster_crop <- raster::mask(SWE_raster_crop, WBD17_outline)

png(file = paste0(plot.dir, "/SWE_SNODAS_", y, "_", doy, ".png"), height = 5, width = 6, units = "in", res = 400)
  sp::spplot(SWE_raster_crop, col.regions = my.palette, cuts = 7, col = "transparent")
dev.off()


