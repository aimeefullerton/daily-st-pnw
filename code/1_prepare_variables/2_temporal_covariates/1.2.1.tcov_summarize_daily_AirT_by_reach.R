# Summarize Air Temperature (AirT) by NHDPlusV2 reach-contributing area
# data from PRISM; https://prism.oregonstate.edu/

# SETUP ----
# Load packages
library(foreach)

# Directories
prism_path <- "data/PRISM"
plot.dir <- "plots"

# LOAD DATA ----

# Load region 17 for clipping rasters
WBD17_outline <- rgdal::readOGR("data/shapefiles/WBD17_outline.shp") # from NHD Plus version 2

# Load spatial polygons for summarizing rasters
NHDv2_polygons  <- rgdal::readOGR("data/shapefiles/Catchment.shp") # from NHD Plus version 2

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
UseCores <- cores_available -1

# Set year range
min_year <- 1990
max_year <- 2022

# * ANNUAL LOOP ----

# Register CoreCluster
cl <- parallel::makeCluster(UseCores)
doParallel::registerDoParallel(cl)

for(y in min_year:max_year){
  print(y)
  start <- Sys.time()
  print(start)

  # Create list of files in year folder for annual loop
  PRISM_files <- list.files(path = paste0(prism_path, "/PRISM_tmean_stable_4kmD2_", y, "0101_", y, "1231_bil/"), pattern = "bil.bil")
  PRISM_files <- PRISM_files[- grep(".aux.xml", PRISM_files)]

  # ** Daily loop
    daily_mean_AirT <- foreach(i = 1:length(PRISM_files)) %dopar% {
      source("code/Full_region_17/0_functions/fncSummarizeAirTDaily.R")
      fncSummarizeAirTDaily(i, filepath = paste0(prism_path, "/PRISM_tmean_stable_4kmD2_", y, "0101_", y, "1231_bil/", PRISM_files[i]), 
                          shps = list(WBD17_outline, NHDv2_polygons_small, NHDv2_polygons_large), PRISM_files)
    }

  # Combine list of tables to one annual table
  j <- 1
  mean_AirT_table <- matrix(ncol = 5, nrow = nrow(daily_mean_AirT[[1]]) * length(daily_mean_AirT))
  for(i in 1:length(daily_mean_AirT)){
    mean_AirT_table[j: (j + nrow(daily_mean_AirT[[i]]) - 1),] <- daily_mean_AirT[[i]]
    j <- j + nrow(daily_mean_AirT[[i]])
    }
  colnames(mean_AirT_table) <- c("COMID", "area_km2", "year", "date", "air_temp")
  
  # Add date and lookup
  mean_AirT_table <- as.data.frame(mean_AirT_table)
  mean_AirT_table$doy <- format(as.Date(mean_AirT_table$date, format = "%Y-%m-%d"), format = "%j")
  mean_AirT_table$lookup <- paste0(mean_AirT_table$COMID,"_", mean_AirT_table$year, "_", as.numeric(mean_AirT_table$doy)) #strip extra 0 at start of doy
  
  # Save file
  fst::write_fst(mean_AirT_table, paste0(prism_path, "/mean_AirT_", y, ".fst"), compress = 100)
  
  end <- Sys.time()
  print(difftime(end, start))
  
}

parallel::stopCluster(cl)


# VISUAL EVALUATION ----
# Make plots to ensure that the process worked
my.palette <- rev(RColorBrewer::brewer.pal(n = 11, name = "Spectral"))

# Choose year to examine
y <- 1990
mean_AirT_y <- fst::read_fst(paste0(prism_path, "/mean_AirT_", y, ".fst"), as.data.table = T)

# Subset to specific day
doy <- 210
mean_AirT_y_doy <- mean_AirT_y[mean_AirT_y$doy == doy,]

# Merge subset with the spatial polygons
NHDv2_polygons2 <- sp::merge(NHDv2_polygons, mean_AirT_y_doy, by.x = "FEATUREID", by.y = "COMID", all.x = T)
NHDv2_polygons2 <- NHDv2_polygons2[!is.na(NHDv2_polygons2$air_temp),]
NHDv2_polygons2$air_temp <- as.numeric(NHDv2_polygons2$air_temp)

png(file = paste0(plot.dir, "/AirT_byHUC_", y, "_", doy, ".png"), height = 5, width = 6, units = "in", res = 300)
  sp::spplot(NHDv2_polygons2, "air_temp", main = "AirT sumamrized by polygons", col.regions = my.palette, cuts = 10, col = "transparent")
dev.off()

# Plot from original raster for comparison
PRISM_files <- list.files(path = paste0(prism_path, "/PRISM_tmean_stable_4kmD2_", y, "0101_", y, "1231_bil/"), pattern = "bil.bil")
PRISM_files <- PRISM_files[- grep(".aux.xml", PRISM_files)]
PRISM_raster <- raster::raster(paste0(prism_path, "/PRISM_tmean_stable_4kmD2_", y, "0101_", y, "1231_bil/", PRISM_files[doy]))
PRISM_raster <- raster::crop(PRISM_raster, WBD17_outline)
PRISM_raster <- raster::mask(PRISM_raster, WBD17_outline)

png(file = paste0(plot.dir, "/AirT_PRISM_", y, "_", doy, ".png"), height = 5, width = 6, units = "in", res = 600)
  sp::spplot(PRISM_raster, col.regions = my.palette, cuts = 10, col = "transparent")
dev.off()
