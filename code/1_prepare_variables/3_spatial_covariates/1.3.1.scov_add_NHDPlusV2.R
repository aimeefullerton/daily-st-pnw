# Create spatial dataset beginning with NHDPlus verion 2 attributes

# Source data: https://nhdplus.com/NHDPlus/NHDPlusV2_data.php. Here, we use Region 17 (Pacific Northwest)

# NHD data can be downloaded from above URL. Alternatively: 
# nhdplusTools::download_nhdplusv2("data/nhdv2") 7.6 GB, needs to be unzipped once downloaded
# Blodgett, D., Johnson, J.M., 2022, nhdplusTools: Tools for Accessing and Working with the NHDPlus, https://doi.org/10.5066/P97AS8JD.


# LOAD DATA ----

# Load flowline & value attribute table
flowline <- sf::st_read("data/nhdv2/NHDPlusPN/NHDPlus17/NHDSnapshot/Hydrography", "NHDFlowline") #, type = 7)
vaa <- sf::st_read("data/nhdv2/NHDPlusPN/NHDPlus17/NHDPlusAttributes", "PlusFlowlineVAA"); colnames(vaa)[1] <- "COMID"

# Join attributes to flowline
flowline_attributed <- dplyr::left_join(flowline, vaa, by = "COMID")
flowline_attributed <- flowline_attributed[!flowline_attributed$FTYPE == "Coastline",] # remove coastline reaches

# Load NHDPlus Version 2 variables
ElevSlope_NHD <- foreign::read.dbf("data/nhdv2/NHDPlusPN/NHDPlus17/NHDPlusAttributes/ElevSlope.dbf", as.is = FALSE)
Sinuosity_NHD <- read.delim("data/nhdv2/Sinuousity_CONUS/Sinuousity_CONUS.txt", header = TRUE, sep = ",", dec = ".")
Lat_NHD <- read.delim("data/nhdv2/IncrLat.txt", header = TRUE, sep = ",", dec = ".")
  
# ADD TO DATASET ----
spatial_data <- flowline_attributed  #initiate spatial_data
spatial_data <- merge(spatial_data, ElevSlope_NHD, by = "COMID", all.x = TRUE)
spatial_data <- merge(spatial_data, dplyr::select(Sinuosity_NHD, COMID, sinuosity), by = "COMID", all.x = TRUE)
spatial_data <- merge(spatial_data, dplyr::select(Lat_NHD, FeatureID, LatV), by.x = "COMID", by.y = "FeatureID", all.x = TRUE)

# RENAME VARIABLES ----
colnames(spatial_data)[colnames(spatial_data) == "TotDASqKM"] = "cov.tot.da.sqkm"
colnames(spatial_data)[colnames(spatial_data) == "MINELEVSMO"] = "cov.elev_min_smo"
colnames(spatial_data)[colnames(spatial_data) == "MAXELEVSMO"] = "cov.elev_max_smo"
colnames(spatial_data)[colnames(spatial_data) == "SLOPE"] = "cov.slope"
colnames(spatial_data)[colnames(spatial_data) == "LatV"] = "cov.lat_v"
colnames(spatial_data)[colnames(spatial_data) == "sinuosity"] = "cov.sinuosity"
colnames(spatial_data)[colnames(spatial_data) == "LENGTHKM"] = "cov.length_km"
colnames(spatial_data)[colnames(spatial_data) == "StreamOrde"] = "cov.stream_order"
colnames(spatial_data)[colnames(spatial_data) == "Pathlength"] = "cov.path_length"

# COMPUTE SOME VARIABLES ----
spatial_data$cov.elev_mean_smo <- ((spatial_data$cov.elev_min + spatial_data$cov.elev_max_smo) / 2) / 100
spatial_data$cov.slope <- ifelse(spatial_data$cov.slope < 0, median(spatial_data$cov.slope), spatial_data$cov.slope)

# VARIABLES TO KEEP ----
spatial_data <- as.data.frame(spatial_data)
var2keep <- c("COMID", "cov.length_km", "cov.stream_order", "cov.path_length", "cov.tot.da.sqkm", "cov.elev_max_smo", 
              "cov.elev_min_smo", "cov.slope", "cov.sinuosity", "cov.lat_v", "cov.elev_mean_smo")
spatial_data <- spatial_data[,var2keep]

# EXPORT ----
data.table::fwrite(spatial_data, 'data/spatial_data.csv')
sf::st_write(obj = flowline_attributed, dsn = "data/shapefiles", layer = "NHD_flowline_attributed.shp", driver = "ESRI Shapefile")
# NOTE!: error messages for COMIDs beginning 9470101XX - number larger than accepted field width;
# however, all appear to still be in the dataset.
