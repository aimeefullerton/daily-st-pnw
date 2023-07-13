# Add StreamCat variables to the spatial dataset

# Source data: https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset
# Hill, R. A., M. H. Weber, S. G. Leibowitz, A. R. Olsen, and D. J. Thornbrugh, 2016. 
  # The Stream-Catchment (StreamCat) Dataset:  A Database of Watershed Metrics for the Conterminous United States. 
  # Journal of the American Water Resources Association (JAWRA) 52:120-128. DOI: 10.1111/1752-1688.12372.
# StreamCat scripts are available at https://github.com/USEPA/StreamCat/blob/master/README.md
  # cited as Marc Weber, Rick Debbout, Ryan Hill, Julia Ortiz, & dthor. (2023). USEPA/StreamCat: StreamCat DOI (v.2.1). Zenodo. 
  # https://doi.org/10.5281/zenodo.8141137
# Note: we did not use this, but there is now an R interface for accessing these data: https://github.com/USEPA/StreamCatTools

# LOAD DATA ----
spatial_data <- data.table::fread('data/spatial_data.csv')

BFI_region17 <- data.table::fread("data/StreamCat/BFI_region17.csv")
CanalDensity_Region17 <- data.table::fread("data/StreamCat/CanalDensity_Region17.csv")
ImperviousSurfaces2001_Region17 <- data.table::fread("data/StreamCat/ImperviousSurfaces2001_Region17.csv")
ImperviousSurfacesRipBuf100_Region17 <- data.table::fread("data/StreamCat/ImperviousSurfacesRipBuf100_Region17.csv")
NLCD2008_Region17 <- data.table::fread("data/StreamCat/NLCD2008_Region17.csv")
NLCD2008RipBuf100_Region17 <- data.table::fread("data/StreamCat/NLCD2008RipBuf100_Region17.csv")
PRISM_1981_2010_Region17 <- data.table::fread("data/StreamCat/PRISM_1981_2010_Region17.csv")
STATSGO_Set2_Region17 <- data.table::fread("data/StreamCat/STATSGO_Set2_Region17.csv")
Elevation_Region17 <- data.table::fread("data/StreamCat/Elevation_Region17.csv")
Dams_Region17 <- data.table::fread("data/StreamCat/Dams_Region17.csv")
Lithology_Region17 <- data.table::fread("data/StreamCat/Lithology_Region17.csv")


# ADD TO DATASET ----
spatial_data <- merge(spatial_data, dplyr::select(BFI_region17, COMID, WsAreaSqKm, BFICat, BFIWs), by = "COMID", all.x = T)
spatial_data <- merge(spatial_data, dplyr::select(CanalDensity_Region17, COMID, CanalDensCat, CanalDensWs), by = "COMID", all.x = T)
spatial_data <- merge(spatial_data, dplyr::select(Dams_Region17, COMID, DamNrmStorCat, DamNrmStorWs, DamDensCat, DamDensWs), by = "COMID", all.x = T)
spatial_data <- merge(spatial_data, dplyr::select(Elevation_Region17, COMID, ElevCat, ElevWs), by = "COMID", all.x = T)
spatial_data <- merge(spatial_data, dplyr::select(ImperviousSurfaces2001_Region17, COMID, PctImp2001Cat, PctImp2001Ws), by = "COMID", all.x = T)
spatial_data <- merge(spatial_data, dplyr::select(ImperviousSurfacesRipBuf100_Region17, COMID, PctImp2001CatRp100, PctImp2001WsRp100), by = "COMID", all.x = T)
spatial_data <- merge(spatial_data, dplyr::select(Lithology_Region17, COMID, PctExtruVolCat, PctExtruVolWs), by = "COMID", all.x = T)
spatial_data <- merge(spatial_data, dplyr::select(NLCD2008_Region17, COMID, PctWdWet2008Cat, PctWdWet2008Ws, PctHbWet2008Cat, PctHbWet2008Ws, 
                PctIce2008Ws, PctOw2008Ws, PctBl2008Ws, PctUrbHi2008Ws, PctUrbMd2008Ws, PctUrbLo2008Ws, PctUrbOp2008Ws), by = "COMID", all.x = T)
spatial_data <- merge(spatial_data, dplyr::select(NLCD2008RipBuf100_Region17, COMID, PctDecid2008CatRp100, PctConif2008CatRp100, 
                PctMxFst2008CatRp100, PctWdWet2008CatRp100, PctWdWet2008WsRp100, PctHbWet2008CatRp100, PctHbWet2008WsRp100), by = "COMID", all.x = T)
spatial_data <- merge(spatial_data, dplyr::select(PRISM_1981_2010_Region17, COMID, PrecipCat, TmaxCat, TmeanCat, TminCat, PrecipWs, TmaxWs, TmeanWs, TminWs), by = "COMID", all.x = T)
spatial_data <- merge(spatial_data, dplyr::select(STATSGO_Set2_Region17, COMID, OmCat, PermCat, RckDepCat, WtDepCat, OmWs, PermWs, RckDepWs,  WtDepWs), by = "COMID", all.x = T)

# RENAME VARIABLES ----
colnames(spatial_data)[colnames(spatial_data) == "WsAreaSqKm"] ="cov.area_km2_ws"
colnames(spatial_data)[colnames(spatial_data) == "BFICat"] ="cov.BFI_cat"
colnames(spatial_data)[colnames(spatial_data) == "BFIWs"] ="cov.BFI_ws"
colnames(spatial_data)[colnames(spatial_data) == "CanalDensCat"] ="cov.canal_dens_cat"
colnames(spatial_data)[colnames(spatial_data) == "CanalDensWs"] ="cov.canal_dens_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctImp2001Cat"] ="cov.pct_imp_cat"
colnames(spatial_data)[colnames(spatial_data) == "PctImp2001Ws"] ="cov.pct_imp_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctDecid2008CatRp100"] ="cov.pct_decid_cat_rip100m"
colnames(spatial_data)[colnames(spatial_data) == "PctConif2008CatRp100"] ="cov.pct_conif_cat_rip100m"
colnames(spatial_data)[colnames(spatial_data) == "PctMxFst2008CatRp100"] ="cov.pct_mx_fst_cat_rip100m"
colnames(spatial_data)[colnames(spatial_data) == "ElevCat"] ="cov.elev_cat"
colnames(spatial_data)[colnames(spatial_data) == "ElevWs"] ="cov.elev_ws"
colnames(spatial_data)[colnames(spatial_data) == "PrecipCat"] ="cov.precip_cat"
colnames(spatial_data)[colnames(spatial_data) == "PrecipWs"] ="cov.precip_ws"
colnames(spatial_data)[colnames(spatial_data) == "TmaxCat"] ="cov.air_temp_max_cat"
colnames(spatial_data)[colnames(spatial_data) == "TmeanCat"] ="cov.air_temp_mean_cat"
colnames(spatial_data)[colnames(spatial_data) == "TminCat"] ="cov.air_temp_min_cat"
colnames(spatial_data)[colnames(spatial_data) == "TmaxWs"] ="cov.air_temp_max_ws"
colnames(spatial_data)[colnames(spatial_data) == "TmeanWs"] ="cov.air_temp_mean_ws"
colnames(spatial_data)[colnames(spatial_data) == "TminWs"] ="cov.air_temp_min_ws"
colnames(spatial_data)[colnames(spatial_data) == "OmCat"] ="cov.om_cat"
colnames(spatial_data)[colnames(spatial_data) == "OmWs"] ="cov.om_ws"
colnames(spatial_data)[colnames(spatial_data) == "PermCat"] ="cov.perm_cat"
colnames(spatial_data)[colnames(spatial_data) == "PermWs"] ="cov.perm_ws"
colnames(spatial_data)[colnames(spatial_data) == "RckDepCat"] ="cov.rck_dep_cat"
colnames(spatial_data)[colnames(spatial_data) == "RckDepWs"] ="cov.rck_dep_ws"
colnames(spatial_data)[colnames(spatial_data) == "WtDepCat"] ="cov.wt_dep_cat"
colnames(spatial_data)[colnames(spatial_data) == "WtDepWs"] ="cov.wt_dep_ws"
colnames(spatial_data)[colnames(spatial_data) == "DamNrmStorCat"] ="cov.dam_nrm_stor_cat"
colnames(spatial_data)[colnames(spatial_data) == "DamNrmStorWs"] ="cov.dam_nrm_stor_ws"
colnames(spatial_data)[colnames(spatial_data) == "DamDensCat"] ="cov.dam_dens_cat"
colnames(spatial_data)[colnames(spatial_data) == "DamDensWs"] ="cov.dam_dens_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctIce2008Ws"] ="cov.pct_ice_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctOw2008Ws"] ="cov.pct_ow_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctBl2008Ws"] ="cov.pct_bl_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctExtruVolCat"] ="cov.extru_vol_cat"
colnames(spatial_data)[colnames(spatial_data) == "PctExtruVolWs"] ="cov.pct_extru_vol_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctWdWet2008Cat"] ="cov.pct_wd_wet_cat"
colnames(spatial_data)[colnames(spatial_data) == "PctWdWet2008Ws"] ="cov.pct_wd_wet_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctHbWet2008Cat"] ="cov.pct_hb_wet_cat"
colnames(spatial_data)[colnames(spatial_data) == "PctHbWet2008Ws"] ="cov.pct_hb_wet_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctWdWet2008CatRp100"] ="cov.pct_wd_wet_cat_rip100m"
colnames(spatial_data)[colnames(spatial_data) == "PctWdWet2008WsRp100"] ="cov.pct_wd_wet_ws_rip100m"
colnames(spatial_data)[colnames(spatial_data) == "PctHbWet2008CatRp100"] ="cov.pct_hb_wet_cat_rip100m"
colnames(spatial_data)[colnames(spatial_data) == "PctHbWet2008WsRp100"] ="cov.pct_hb_wet_ws_rip100m"
colnames(spatial_data)[colnames(spatial_data) == "PctImp2001CatRp100"] ="cov.pct_imp_cat_rip100m"
colnames(spatial_data)[colnames(spatial_data) == "PctImp2001WsRp100"] ="cov.pct_imp_ws_rip100m"
colnames(spatial_data)[colnames(spatial_data) == "PctUrbHi2008Ws"] ="cov.pct_urb_hi_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctUrbMd2008Ws"] ="cov.pct_urb_md_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctUrbLo2008Ws"] ="cov.pct_urb_lo_ws"
colnames(spatial_data)[colnames(spatial_data) == "PctUrbOp2008Ws"] ="cov.pct_urb_op_ws"

        
# COMPUTE SOME VARIABLES ----
spatial_data$cov.area_km2_ws_log <- log(spatial_data$cov.area_km2_ws)
spatial_data$cov.elev_diff  <- spatial_data$cov.elev_ws - spatial_data$cov.elev_mean_smo
spatial_data$cov.air_temp_range_cat <- spatial_data$cov.air_temp_max_cat - spatial_data$cov.air_temp_min_cat
spatial_data$cov.pct_for_all_cat_rip100m <- spatial_data$cov.pct_decid_cat_rip100m + spatial_data$cov.pct_conif_cat_rip100m + spatial_data$cov.pct_mx_fst_cat_rip100m
spatial_data$cov.pct_wet_all_ws <- spatial_data$cov.pct_wd_wet_ws + spatial_data$cov.pct_hb_wet_ws
spatial_data$cov.pct_urb_all_ws <- spatial_data$cov.pct_urb_hi_ws + spatial_data$cov.pct_urb_md_ws + spatial_data$cov.pct_urb_lo_ws + spatial_data$cov.pct_urb_op_ws


# CHECK
nrow(spatial_data[is.na(spatial_data$cov.air_temp_mean_cat),]) / nrow(spatial_data) # ~15% of records with no StreamCat attributes

# EXPORT ----
file.copy("data/spatial_data.csv", "data/spatial_data_bu.csv", overwrite = T) #back up original file
data.table::fwrite(spatial_data, file = 'data/spatial_data.csv') # overwrites existing file



