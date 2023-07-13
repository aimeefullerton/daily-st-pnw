# Add Canopy (NLCD) & summarized canopy in upstream watershed to spatial dataset

# Bocinsky RK (2023). FedData: Functions to Automate Downloading Geospatial Data Available from Several Federated Data Sources. 
# R package version 3.0.4.9000, https://CRAN.R-project.org/package=FedData.

# The accumulation script used below was produced by:
# Marc Weber, Rick Debbout, Ryan Hill, Julia Ortiz, & dthor. (2023). USEPA/StreamCat: StreamCat DOI (v.2.1). Zenodo. 
# https://doi.org/10.5281/zenodo.8141137. See also: https://github.com/USEPA/StreamCat/blob/master/README.md



# SETUP ----

# Load packages
devtools::install_github("ropensci/FedData")

# LOAD DATA ----
spatial_data <- data.table::fread('data/spatial_data.csv')

# Load spatial data for summarizing rasters
NHDv2_lines <- sf::st_read("data/nhdv2/NHDPlusPN/NHDPlus17/NHDSnapshot/Hydrography", "NHDFlowline") #, type = 7)
WBD17_outline <- rgdal::readOGR("data/shapefiles/WBD17_outline.shp")
NHDv2_polygons  <- rgdal::readOGR("data/shapefiles/Catchment.shp")
NLCD <- FedData::get_nlcd(template = WBD17_outline, label='R17')
NLCD_canopy <- FedData::get_nlcd(template = WBD17_outline, label='R17', year = 2011, dataset = c("canopy"))

# PROCESS ----
# Transform projection
WBD17_outline<- spTransform(WBD17_outline, CRS(proj4string(NLCD_canopy)))
NHDv2_lines<- spTransform(NHDv2_lines, CRS(proj4string(NLCD_canopy)))
NHDv2_polygons<- spTransform(NHDv2_polygons, CRS(proj4string(NLCD_canopy)))
NLCD<- spTransform(NLCD, CRS(proj4string(NLCD_canopy)))

# Crop
NHDv2_lines_crop <-  raster::crop(NHDv2_lines, WBD17_outline)
NLCD_canopy_crop <- raster::crop(NLCD_canopy, WBD17_outline)
NLCD_crop <- raster::crop(NLCD, WBD17_outline)

# Extract values
NLCD_canopy_crop_coarse <- aggregate(NLCD_canopy_crop, fact = 100, fun = max)

NLCD_poly <- sapply(raster::extract(NLCD_canopy_crop_coarse, NHDv2_polygons), mean, na.rm = T)
NLCD_line <- sapply(raster::extract(NLCD_canopy_crop_coarse, NHDv2_lines_crop), mean, na.rm = T)

# Merge
canopy_lookup <- as.data.frame(cbind(NHDv2_polygons@data$FEATUREID, NLCD_poly)); colnames(canopy_lookup) <- c("COMID", "CANOPY_rca")
canopy_line_lookup <- as.data.frame(cbind(NHDv2_lines_crop@data$COMID, NLCD_line)); colnames(canopy_line_lookup) <- c("COMID", "CANOPY_line")
canopy_lookup <- merge(canopy_lookup, canopy_line_lookup, by = "COMID", all.x = T)
canopy_lookup$AreaSqKM <- NHDv2_polygons@data$AreaSqKM

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Read accumulation files 
np <- reticulate::import("numpy")
zone <- '17'
accum <- np$load(paste0("StreamCat/accum_", zone, "_new.npz"))
accum$files
comids <- accum$f[["comids"]] #List of all COMIDs in region 17
lengths <- accum$f[["lengths"]] #List of how many catchments that are upstream of each comid
upstream <- accum$f[["upstream"]] #Long list of all upstream catchments for each comid
mapped <- rep(comids, times=lengths) #Create vector with focal comid mapped to locations of upstream comids

locs <- match(upstream, canopy_lookup$COMID) #Match vector addresses of catchment COMIDs to full vector of upstream catchments
comid_locs <- match(comids, canopy_lookup$COMID) #Match vector addresses from focal catchment COMIDs to table of catchment summaries
upstream_areas <- canopy_lookup$AreaSqKM[locs] #Use locs to replace upstream catchment IDs with catchment areas
upstream_vals <- canopy_lookup$CANOPY_rca[locs] #Use locs to replace upstream catchment IDs with catchment means
upstream_weighted <- upstream_areas * upstream_vals #Weight catchment means by area to create area weighted average
upstream_areas[is.na(upstream_weighted)] <- NA
comid_vals <- canopy_lookup$CANOPY_rca[comid_locs] #Vector of values that match order of cominds vector

# Create initial data
in_df <- data.frame(COMID = mapped, areas = upstream_areas, wtd_values = upstream_weighted)
#Create data frame with groupings
grped_df <- in_df %>% dplyr::group_by(COMID)
#Create sum of catchment areas and sum of weighted catchment means
out_df <- grped_df %>% dplyr::summarise(total_area = sum(areas, na.rm = T), wtd_sum = sum(wtd_values, na.rm = T))
#Calculate weighted mean from result
out_df$canopy_ws <- out_df$wtd_sum / out_df$total_area
#merge back to original data for day i
canopy_lookup <- merge(canopy_lookup,  dplyr::select(out_df, COMID, canopy_ws))

colnames(canopy_lookup) <- c("COMID", "cov.canopy_rca", "cov.canopy_line", "cov.area_sqkm", "cov.canopy_ws")

# ADD TO DATASET ----
spatial_data <- merge(spatial_data, canopy_lookup, by = "COMID", all.x = T)

# EXPORT ----
data.table::fwrite(canopy_lookup, file = 'data/canopy/canopy_data.csv')
file.copy("data/spatial_data.csv", "data/spatial_data_bu.csv", overwrite = T) #back up original file
data.table::fwrite(spatial_data, file = 'data/spatial_data.csv') # overwrites existing file




   
   