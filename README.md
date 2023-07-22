**PNW Stream Temperature 0.1.0**

A statistical model predicting daily stream temperatures in Pacific Northwest USA reaches*, presented in:

Siegel, J. E., A. H. Fullerton, A. M. FitzGerald, D. Holzer, and C. E. Jordan. 2023. Daily stream temperature predictions for free-flowing streams in the Pacific Northwest, USA.

_*reaches for hydroregion 17, as defined in the National Hydrography Dataset version 2, medium resolution_

Contacts: aimee.fullerton@noaa.gov & derajlegeis@gmail.com. 
  
--------------------------------------------------------------------------------

STEP I: Set up.

Follow these steps to download software, model input files, and packages required to replicate our process. 

We used a PC with a 64-bit Windows operating system, 12 Intel and Xeon CPU cores (3.60 GHz) and 192 GB RAM. We used around 2 TB for data storage (and more for virtual RAM). On this system, processing took hours to days for certain steps, especially those involving preparation of temporal covariates. Memory and disk read/write speed seemed to be bigger issues than processors for us. The user will need to run scripts sequentially as indicated by file names and monitor progress for stalls, possibly incorporating some restarts to refresh memory as needed.

1.	Download and install R and RStudio or other GUI.
2.	Clone or download this repository as a new project.
3.	Package libraries called from our scripts that may need to be installed prior to running include: data.table, sf, raster, dplyr, readxl, rgdal, sp, RColorBrewer, visreg, lubridate, parallelly, parallel, doParallel, foreach, reticulate, doSNOW, ncdf4, mgcv, GGally, cowplot, ggpubr, gridExtra.
4.	Acquire the necessary data, as described at the top of each script. Some of these require pre-processing not included in our scripts.
   
--------------------------------------------------------------------------------

STEP II: Run pre-processing scripts to prepare data that will be needed for modeling. 

All scripts are found in the folder ‘code’. The subfolder ‘code/0_functions’ contains functions that are sourced from within scripts; no need to load separately. Each of the following subfolders has a series of scripts that need to be run sequentially, as indicated by their filenames because they build on one another. We kept them separate in the event that only certain pieces might need to be updated in the future. Begin by running scripts in the 1_response_data/ folder, and then running scripts in the 2_temporal_covariates/ folder, and so on. Please scroll down to the end of this file for more information about what data are read in and written out by each script. After running scripts, there will be new directories and files in the ‘data’ and ‘plots’ folders. Some of the figures used in the paper are generated from functions called during data preparation and model fitting. Maps were produced using external software.

code/1_prepare_variables/
 * 1_response_data/
 * 2_temporal_covariates/
 * 3_spatial_covariates/
 * 4_create_fitting_dataset/
 * 5_create_prediction_dataset/
 * 6_covariate_evaluation/
 
--------------------------------------------------------------------------------

Data sources are cited as they are first encountered at the top of each script. Some independent preparation of data downloaded from public sources may be necessary before running these scripts, particularly if you are applying the model in a new geographic location. 

Data that cannot be downloaded from the public domain are available either (1) in the ‘data’ directory of this repository and may need to be unzipped locally, or (2) for those too large for Git LFS (2 shapefiles), files are provided in the location where our processed predictions (and eventually the processed covariates) are stored.

Accumulation scripts used in several steps are from Marc Weber, Rick Debbout, Ryan Hill & Darren Thornbrugh. (2023). USEPA/StreamCat: StreamCat DOI (v.2.1). Zenodo. https://doi.org/10.5281/zenodo.8141137. The associated file 'accum_17.npz' needed for processing some covariates is available here: https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/NHEERL/WED/.


Expected structure of the ‘data’ directory is below.

* canopy/ tree canopy cover processed from NLCD
* dams/  data about large dams from GRaND and National Inventory of Dams
* nhdv2/ streamlines and attributes from National Hydrography Plus version 2
* NWM/ stream flow predicted by NOAA’s National Water Model
* PRISM/ gridded air temperature from PRISM
* response/ empirical stream temperature observations provided by USFS’s NorWeST project
* shapefiles/ NHDPlus_v2-derived shapefiles for the Columbia River basin: "Catchment.shp" (flowline reach contributing areas) and dissolved as "WBD17_outline.shp"
* StreamCat/ EPA’s Stream Catchment dataset & accumulation script
* SWE/ Snow-Water Equivalent data from NOAA’s National Snow and Ice Data Center

* COMID_to_HUC12.csv – a cross-walk between reach and HUC identifiers for region 17
* HUC_daylight_seconds.csv – day length summarized from DayMet

--------------------------------------------------------------------------------

STEP III: Run scripts to fit models and make predictions and summarize results.

code/2_modeling/
* 2.1.mod_fit_models.R – the script used to fit models using the fitting dataset prepared above. Model fit evaluation and spatial and temporal cross-validation are included here.
* 2.2.mod_make_predictions.R – the script used to predict results in all unsampled locations and dates using the huc-level prediction datasets prepared above.

Results and pre-processed covariates for the Pacific Northwest are available at https://riverscapes.net/Data_Warehouses/.

--------------------------------------------------------------------------------

List of scripts for preparing data and fitting stream temperature models in R. For each script, the data files that are read and written are listed. Data sources are cited as they are first encountered at the top of each script.

**1_prepare_variables**

**/1_response_data**

_**1.1.res_prepare_observation_data.R**_

READS:
* data/response/NorWeST_ObservedStreamTempDailySummaries_OregonCoast_AllDays.txt
* data/response/NorWest_OrCoast_ObsPoints.txt (for each processing unit in: OrCoast, MidColumbia, UCYak, WaCoast, Clear, SouthCentOre, Salmon, SnakeBear, SpoKoot, MidSnake)
* data/response/NorWest_COMID_Xreference.txt

WRITES:
* data/response/NorWeST_obs.csv (1993-2013)



**/2_temporal_covariates**

_**1.2.1.tcov_summmarize_daily_AirT_by_reach.R**_

READS:
* data/shapefiles/WBD17_outline.shp
* data/shapefiles/Catchment.shp
* data/PRISM/PRISM_tmean_stable_4kmD2_20000101_20001231_bil/PRISM_tmean_stable_4kmD2_20000101_bil.bil

WRITES:
* data/PRISM/mean_AirT_yyyy.fst (1990 - 2022)


_**1.2.2.tcov_summarize_daily_AirT_by_catchment.R**_

READS:
* data/shapefiles/Catchment.shp
* data/StreamCat/accum_17_new.npz
* data/PRISM/mean_AirT_yyyy.fst (1990 - 2022)

WRITES:
* data/PRISM/mean_AirT_yyyy.fst (1990 - 2022; overwrites existing file)


_**1.2.3.tcov_compute_daily_AirT_mov_avg_metrics.R**_

READS:
* data/PRISM/mean_AirT_yyyy.fst (1990 - 2022)

WRITES:
* data/PRISM/AirT_fitting.fst
* data/PRISM/AirT_huc6.fst (for each of the 22 huc6 codes)


_**1.2.4.tcov_summarize_daily_SWE_by_reach.R**_

READS:
* data/shapefiles/WBD17_outline.shp
* data/shapefiles/Catchment.shp
* data/PRISM/PRISM_tmean_stable_4kmD2_20000101_20001231_bil/PRISM_tmean_stable_4kmD2_20000101_bil.bil
* data/SWE/4km_SWE_Depth_WYyyyy_v01.nc (1990 - 2021)

WRITES:
* data/SWE/mean_SWE_yyyy.fst (1990 - 2021)


_**1.2.5.tcov_summarize_daily_SWE_by_catchment.R**_

READS:
* data/shapefiles/Catchment.shp
* data/StreamCat/accum_17_new.npz
* data/SWE/mean_SWE_yyyy.fst (for all years)

WRITES:
* data/SWE/mean_SWE_yyyy.fst (1990 - 2021; overwrites existing file)
* data/PRISM/SWE_fitting.fst
* data/PRISM/SWE_huc6.fst (for each of the 22 huc6 codes)


_**1.2.6.tcov_compute_annual_SWE_metrics.R**_

READS:
* data/SWE/mean_SWE_yyyy.fst (1990 - 2021)
* data/shapefiles/Catchment.shp

WRITES:
* data/SWE/SWE_annual_metrics.fst


_**1.2.7.tcov_prepare_daily_NWM_flow_by_reach.R**_

READS:
* data/NWM/CRB_flow_V2_yyyy.fst (1990 - 2022)

WRITES:
* data/NWM/NWM_yyyy.fst (1990 - 2022)
* data/NWM/NWM_fitting.fst
* data/NWM/NWM_huc6.fst (for each of the 22 huc6 codes)


**/3_spatial_covariates**

_**1.3.1.scov_add_NHDPlusV2.R**_

READS:
* data/nhdv2/NHDPlusPN/NHDPlus17/NHDSnapshot/Hydrography/Flowline.shp
* data/nhdv2/NHDPlusPN/NHDPlus17/NHDPlusAttributes/PlusFlowlineVAA.dbf
* data/nhdv2/NHDPlusPN/NHDPlus17/NHDPlusAttributes/elevslope.dbf
* data/nhdv2/Sinuousity_CONUS/Sinuousity_CONUS.txt
* data/nhdv2/IncrLat.txt

WRITES:
* data/spatial_data.csv


_**1.3.2.scov_add_StreamCat_variables.R**_

READS:
* data/spatial_data.csv
* data/StreamCat/BFI_region17.csv
* data/StreamCat/CanalDensity_Region17.csv
* data/StreamCat/ImperviousSurfaces2001_Region17.csv
* data/StreamCat/ImperviousSurfacesRipBuf100_Region17.csv
* data/StreamCat/NLCD2008_Region17.csv
* data/StreamCat/NLCD2008RipBuf100_Region17.csv
* data/StreamCat/PRISM_1981_2010_Region17.csv
* data/StreamCat/STATSGO_Set2_Region17.csv
* data/StreamCat/Elevation_Region17.csv
* data/StreamCat/Dams_Region17.csv	
* data/StreamCat/Lithology_Region17.csv

WRITES:
* data/spatial_data.csv (overwrites same file)


_**1.3.3.scov_add_canopy_data.R**_

READS:
* data/spatial_data.csv
* data/shapefiles/WBD17_outline.shp
* data/shapefiles/Catchment.shp
* data/shapefiles/NHD_flowline_attributed.shp
* data/StreamCat/accum_17_new.npz

WRITES:
* data/canopy_data.csv
* data/spatial_data.csv (overwrites same file)


_**1.3.4.scov_add_dams_data.R**_

READS:
* data/spatial_data.csv
* data/nhdv2/NHDPlusPN/NHDPlus17/NHDSnapshot/Hydrography/NHDFlowline.shp
* data/nhdv2/NHDPlusPN/NHDPlus17/NHDPlusAttributes/PlusFlowlineVAA.dbf
* data/dams/PNW_DAMS.shp

WRITES:
* data/dams/dam_influenced_all.csv
* data/dams/dam_influenced.csv
* data/spatial_data.csv (overwrites same file)


**/4_create_fitting_dataset**

_**1.4.fit_prepare_fitting_dataset.R**_

READS:
* data/response/NorWeST_obs.csv
* data/spatial_data.csv
* data/HUC_daylight_seconds.csv
* data/COMID_to_HUC12.csv
* data/PRISM/AirT_fitting.fst
* data/SWE/SWE_fitting.fst
* data/NWM/NWM_fitting.fst
* data/SWE/SWE_annual_metrics.fst

WRITES:
* data/fitting_data.fst


**/5_create_prediction_dataset**

_**1.5_prd_compile_dataset.R**_

READS:
* data/spatial_data.csv
* data/HUC_daylight_seconds.csv
* data/COMID_to_HUC12.csv
* data/PRISM/AirT_huc6.fst (for each of the 22 huc6 codes)
* data/SWE/SWE_huc6.fst (for each of the 22 huc6 codes)
* data/NWM/NWM_huc6.fst (for each of the 22 huc6 codes)
* data/SWE/SWE_annual_metrics.csv

WRITES:
* data/huc/huc_huc10.fst (for each huc10 code)


**/6_covariate_evaluation**

_**1.6.1_site_characteristics_Fig3.R**_

READS:
* data/fitting_data.fst (freeflow_data.fst)

_**1.6.2_covariate_ordinations_Fig5.R**_

READS:
* data/fitting_data.fst (freeflow_data.fst)
* data/huc/huc_huc10.fst

_**1.6.3_covariate_correlations_FigS1.R**_


READS:
* data/fitting_data.fst (freeflow_data.fst)


**2_modeling**

_**2.1.mod_fit_models.R**_

READS:
* data/fitting_data.fst

WRITES:
* data/antec_air_temp_duration_models.RData
* data/freeflow_data.fst
* data/fitted_model.RData
* data/freeflow_data.fst
* data/freeflow_Txv.fst
* data/freeflow_Sxv.fst
* data/freeflow_data_results_summary.csv


_**2.2.mod_make_predictions.R**_

READS:
* data/COMID_to_HUC12.csv
* data/huc/huc_huc10.fst (for each huc10 code)
* data/antec_air_temp_duration_models.RData
* data/fitted_model.RData

WRITES:
* data/st_pred_huc10.csv (for each huc10 code)



