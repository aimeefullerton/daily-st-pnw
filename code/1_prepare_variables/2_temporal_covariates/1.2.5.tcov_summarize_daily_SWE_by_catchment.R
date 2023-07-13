# Summarize Snow Water Equivalent (SWE) in upstream catchment from each reach

# The accumulation script used below was produced by:
# Marc Weber, Rick Debbout, Ryan Hill & Darren Thornbrugh (2023). USEPA/StreamCat: StreamCat DOI (v.2.1). Zenodo. 
# https://doi.org/10.5281/zenodo.8141137. See also: https://github.com/USEPA/StreamCat/blob/master/README.md

# NOTE: better to run each year individually than to use the loop due to memory bogging


# SETUP ----
# Load packages
library(foreach)

# Directories
swe_path <- "data/SWE"
plot.dir <- "plots"

# LOAD ACCUMULATION FILES ----
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
max_year <- 2021

# * Annual Loop ----

# Register CoreCluster
cl <- parallel::makeCluster(UseCores)
#doParallel::registerDoParallel(cl)
doSNOW::registerDoSNOW(cl)

for(y in min_year:max_year){
  print(y)
  start_time <- Sys.time()
  print(start_time)
  
  # Read in year of data
  mean_SWE_table <- fst::read_fst(paste0(swe_path, "/mean_SWE_", y, ".fst"), as.data.table = T)
  # Get data types set correctly
  foo.mat <- as.matrix(mean_SWE_table)
  foo.num <- apply(foo.mat[, c("COMID", "area_km2", "year", "doy", "SWE")], 2, as.numeric)
  mean_SWE_table <- cbind.data.frame(foo.num, foo.mat[,c("date", "lookup")])
  mean_SWE_table$date <- as.Date(mean_SWE_table$date)
  rm(foo.mat, foo.num)
  scols <- c("COMID", "area_km2", "year", "doy", "SWE", "lookup")
  mean_SWE_table <- mean_SWE_table[,..scols]
  mean_SWE_table$year_doy <- paste0(mean_SWE_table$year, "_", mean_SWE_table$doy)
  
  num_days <- length(unique(mean_SWE_table$year_doy))
  days_list <- unique(mean_SWE_table$year_doy)
  
  # Progress bar
  pb <- txtProgressBar(min = 1, max = num_days, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # ** Daily loop ----
  
  # Parallel process each year
  daily_mean_SWE_ws <- foreach(i = 1:num_days, .combine = rbind, .options.snow = opts) %dopar% {
    library(dplyr)
    setTxtProgressBar(pb, i) #initialize progress bar
    mean_SWE_table_i <- subset(mean_SWE_table, year_doy == days_list[i]) #subset year file to day i
    locs <- match(upstream, mean_SWE_table_i$COMID) #Match vector addresses of catchment COMIDs to full vector of upstream catchments
    comid_locs <- match(comids, mean_SWE_table_i$COMID) #Match vector addresses from focal catchment COMIDs to table of catchment summaries
    upstream_areas <- mean_SWE_table_i$area_km2[locs] #Use locs to replace upstream catchment IDs with catchment areas
    upstream_vals <- mean_SWE_table_i$SWE[locs] #Use locs to replace upstream catchment IDs with catchment means
    upstream_weighted <- upstream_areas * upstream_vals #Weigh catchment means by area to create area weighted average
    upstream_areas[is.na(upstream_weighted)] <- NA
    comid_vals <- mean_SWE_table_i$SWE[comid_locs] #Vector of values that match order of comids vector
    rm(comid_locs, upstream_vals, locs)
    
    # Create initial data
    in_df <- data.frame(COMID = mapped, areas = upstream_areas, wtd_values = upstream_weighted)
    
    # Create data frame with groupings
    grped_df <- in_df %>% dplyr::group_by(COMID)
    
    # Create sum of catchment areas and sum of weighted catchment means
    out_df <- grped_df %>% dplyr::summarise(total_area = sum(areas, na.rm = T), wtd_sum = sum(wtd_values, na.rm = T))
    
    # Calculate weighted mean from result
    out_df$SWE_ws <- out_df$wtd_sum / out_df$total_area
    out_df$COMID <- as.character(out_df$COMID)
    
    # Merge back to original data for day i
    mean_SWE_table_i <- merge(mean_SWE_table_i, out_df, by = 'COMID', all.x = T)
    
    return(mean_SWE_table_i)
  }
  # * Save result
  fst::write_fst(daily_mean_SWE_ws, paste0(swe_path, "/mean_SWE_", y, ".fst"), compress = 100)
  
  print(Sys.time() - start_time) #duration spent processing the year
  rm(mean_SWE_table, daily_mean_SWE_ws); gc()
  close(pb)
}

parallel::stopCluster(cl)


# VISUAL EVALUATION ----
# Load spatial polygons
NHDv2_polygons  <- rgdal::readOGR("data/shapefiles/Catchment.shp")

my.palette <- (brewer.pal(n = 9, name = "YlGnBu"))

# Choose year to examine
y <- 2011
mean_SWE_table <- fst::read_fst(paste0(swe_path, "/mean_SWE_", y, ".fst"), as.data.table = T)

# Subset to specific day
doy <- 40
mean_SWE_table_doy <- subset(mean_SWE_table, doy == doy)

# Merge subset with the spatial polygons
NHDv2_polygons2 <- merge(NHDv2_polygons, mean_SWE_table_doy, by.x = "FEATUREID", by.y = "COMID", all.x = T)
NHDv2_polygons2$SWE <- as.numeric(as.character(NHDv2_polygons2$SWE))
NHDv2_polygons2$SWE_ws <- as.numeric(as.character(NHDv2_polygons2$SWE_ws))

# Plot
png(file = paste0(plot.dir, "/SWE_RCA_", y, "_", doy, ".png"), height = 5, width = 6, units = "in", res = 600)
  spplot(NHDv2_polygons2, "SWE", main = "SWE summarized by RCA", 
         col.regions = my.palette,cuts = 8, col = "transparent")
dev.off()

png(file = paste0(plot.dir, "/SWE_watershed_", y, "_", doy, ".png"), height = 5, width = 6, units = "in", res = 600)
  spplot(NHDv2_polygons2, "SWE_ws", main = "SWE summarized by watershed", 
         col.regions = my.palette,cuts = 8, col = "transparent")
dev.off()



