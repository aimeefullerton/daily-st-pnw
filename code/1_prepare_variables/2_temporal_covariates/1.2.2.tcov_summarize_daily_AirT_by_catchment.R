# Summarize Air Temperature (AirT, from PRISM) in upstream catchment from each reach

# The accumulation script used below was produced by:
# Marc Weber, Rick Debbout, Ryan Hill, Julia Ortiz, & dthor. (2023). USEPA/StreamCat: StreamCat DOI (v.2.1). Zenodo. 
# https://doi.org/10.5281/zenodo.8141137. See also: https://github.com/USEPA/StreamCat/blob/master/README.md


# NOTE: better to run each year individually than to use the loop due to memory bogging


# SETUP ----
# Load packages
library(foreach)

# Directories
prism_path <- "data/PRISM"
plot.dir <- "plots"

# LOAD ACCUMULATION FILES ----
  # The reticulate package is an interface to Python scripts (Python needs to be installed locally)
np <- reticulate::import("numpy") 
zone <- '17'
accum <- np$load(paste0("data/StreamCat/accum_", zone, "_new.npz"))
accum$files
comids <- accum$f[["comids"]] #List of all COMIDs in region 17
lengths <- accum$f[["lengths"]] #List of how many catchments that are upstream of each comid
upstream <- accum$f[["upstream"]] #Long list of all upstream catchments for each comid
mapped <- rep(comids, times = lengths) #Create vector with focal comid mapped to locations of upstream comids

# PARALLEL PROCESSING ----

## * Set up parallel processing
cores_available <- parallelly::availableCores()

# Increase memory limits
raster::rasterOptions(maxmemory = 1e+9) #3e+9
raster::rasterOptions(chunksize = 1e+5) #2e+9

# Define how many cores to use
UseCores <- cores_available - 4

# Set year range
min_year <- 1990
max_year <- 2022

# * ANNUAL LOOP ----

# Register CoreCluster
cl <- parallel::makeCluster(UseCores)
#doParallel::registerDoParallel(cl)
doSNOW::registerDoSNOW(cl)

for(y in min_year:max_year){
  print(y)
  start_time <- Sys.time()
  print(start_time)
  
  # Read in year of data
  mean_AirT_table <- fst::read_fst(paste0(prism_path, "/mean_AirT_", y, ".fst"), as.data.table = T)
  
  mean_AirT_table$year_doy <- paste0(mean_AirT_table$year, "_", mean_AirT_table$doy)
  num_days <- length(unique(mean_AirT_table$year_doy))
  days_list <- unique(mean_AirT_table$year_doy)
  
  # Progress bar
  pb <- txtProgressBar(min = 1, max = num_days, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # ** Daily loop ----
  daily_mean_AirT_ws <- foreach(i = 1:num_days, .combine = rbind, .options.snow = opts) %dopar% {
    library(dplyr)
    setTxtProgressBar(pb, i) #initialize progress bar
    daily_mean_AirT_i <- subset(mean_AirT_table, year_doy == days_list[i]) #subset year file to day i
    locs <- match(upstream, daily_mean_AirT_i$COMID) #Match vector addresses of catchment COMIDs to full vector of upstream catchments
    comid_locs <- match(comids, daily_mean_AirT_i$COMID) #Match vector addresses from focal catchment COMIDs to table of catchment summaries
    upstream_areas <- daily_mean_AirT_i$area_km2[locs] #Use locs to replace upstream catchment IDs with catchment areas
    upstream_vals <- daily_mean_AirT_i$air_temp[locs] #Use locs to replace upstream catchment IDs with catchment means
    upstream_weighted <- upstream_areas * upstream_vals #Weigh catchment means by area to create area weighted average
    upstream_areas[is.na(upstream_weighted)] <- NA
    comid_vals <- daily_mean_AirT_i$air_temp[comid_locs] #Vector of values that match order of comids vector
    rm(comid_locs, upstream_vals, locs)
    
    # Create initial data
    in_df <- data.frame(COMID = mapped, areas = upstream_areas, wtd_values = upstream_weighted)
    
    # Create sum of catchment areas and sum of weighted catchment means
    grped_df <- in_df %>% dplyr::group_by(COMID)
    out_df <- grped_df %>% dplyr::summarise(total_area = sum(areas, na.rm = T), wtd_sum = sum(wtd_values, na.rm = T))
    
    # Alternate ways - seeing what is fastest:
    #out_df <- aggregate(in_df, list(in_df$COMID), sum, na.rm = T)
      #out_df <- out_df[,-1]; colnames(out_df) <- c("COMID", "total_area", "wtd_sum")

    #areas_sum <- aggregate(in_df["areas_sum"], in_df["COMID"], sum, na.rm = T)
      #wtd_values_sum <- aggregate(in_df["wtd_values"], in_df["COMID"], sum, na.rm = T)
      #out_df <- merge(areas_sum, wtd_values_sum, by = "COMID", suffixes = c("_sum"))
    
    # Calculate weighted mean from result
    out_df$air_temp_ws <- out_df$wtd_sum / out_df$total_area
    
    # Merge back to original data for day i
    daily_mean_AirT_i <- merge(daily_mean_AirT_i, out_df, by = 'COMID', all.x = T)
    
    return(daily_mean_AirT_i)
  }

  # * Save result
  fst::write_fst(daily_mean_AirT_ws, paste0(prism_path, "/mean_AirT_", y, ".fst"), compress = 100)
  
  print(Sys.time() - start_time) #duration spent processing the year
  rm(mean_AirT_table, daily_mean_AirT_ws); gc()
  close(pb)
}

parallel::stopCluster(cl)


# VISUAL EVALUATION ----
# Load spatial polygons
NHDv2_polygons  <- rgdal::readOGR("data/shapefiles/Catchment.shp")

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

# Plot
png(file = paste0(plot.dir, "/AirT_RCA_", y, "_", doy, ".png"), height = 5, width = 6, units = "in", res = 600)
  sp::spplot(NHDv2_polygons2, "air_temp", main = "Air temperature summarized by RCA", 
         col.regions = my.palette, cuts = 10, col = "transparent")
dev.off()

png(file = paste0(plot.dir, "/AirT_watershed_", y, "_", doy, ".png"), height = 5, width = 6, units = "in", res = 600)
  sp::spplot(NHDv2_polygons2, "air_temp_ws", main = "Air temperature summarized by watershed", 
         col.regions = my.palette, cuts = 10, col = "transparent")
dev.off()

