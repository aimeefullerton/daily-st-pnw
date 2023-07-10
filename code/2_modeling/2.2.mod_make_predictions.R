# This script predicts stream temperatures in free-flowing reaches in the PNW (region 17) by HUC

# SETUP ----

# Directories
huc_path <- "data/huc"
models_path <- "data/results/full"
dir.create("data/predictions")
prediction_path <- "data/predictions"

# Load packages
library(dplyr)
library(mgcv)

# Functions
source("code/Full_region_17/0_functions/fncStandardizedFlow.R")
source("code/Full_region_17/0_functions/fncHydroRegion.R")
source("code/Full_region_17/0_functions/fncAntecedentPeriod.R")
source("code/Full_region_17/0_functions/fncRoundLags.R")
source("code/Full_region_17/0_functions/fncAntecedentAirTemp.R")

# LOAD DATA ----
COM_HUC <- data.table::fread("data/COMID_to_HUC12.csv")
COM_HUC$Huc6 <- substr(COM_HUC$Huc12, 1, 6)
COM_HUC$Huc10 <- substr(COM_HUC$Huc12, 1, 10)
huclist <- sort(unique(COM_HUC$Huc6))
huc10list <- sort(unique(COM_HUC$Huc10))
spatial_data <- data.table::fread("data/spatial_data.csv")

# To run a subset
# huc10list <- sort(unique(COM_HUC$Huc10[COM_HUC$Huc6 == "170900"]))

# LOAD FITTED MODELS ----
load(paste0(models_path, "/fitted_model_full.RData"))
load(paste0(models_path, "/antec_air_temp_duration_models_full.RData"))

#Make model more efficient
strip_glm = function(cm) {
  cm$y = c()
  cm$model = c()
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  return(cm)
}
stream_temp_model <- strip_glm(stream_temp_model)
rm(strip_glm)

# PREDICT ----
start_time <- Sys.time()
print(start_time)
pb <- txtProgressBar(min = 1, max = length(huc10list), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

for(i in 1:length(huc10list)){
  huc10 <- huc10list[i]
  #print(huc10)
  setTxtProgressBar(pb, i) #initialize progress bar
  
  # Load pre-processed covariate data
  huc_data <- fst::read_fst(paste0(huc_path, "/huc_", huc10, ".fst"), as.data.table = T)
  
  # Add area column from spatial dataset that's needed in the next step
  huc_data <- merge(huc_data, spatial_data[,c("COMID", "cov.area_km2_ws")], by = "COMID", all.x = T)
  
  # Add standardized flow
  huc_data <- fncStandardizedFlow(the_data = huc_data)
  
  # Remove really large watersheds
  huc_data <- subset(huc_data, cov.area_km2_ws < 20000)
  
  # Split by hydrological region ----
  hyd_reg <- fncHydroRegion(the_data = huc_data)
  huc_data_rain <- hyd_reg[["rain"]]
  huc_data_trans <- hyd_reg[["trans"]]
  huc_data_snow <- hyd_reg[["snow"]]
  rm(hyd_reg)
  
  # Predict best lag for antecedent air temperature
  huc_data_rain$pred_lag <- predict(rain_lag_model, newdata = huc_data_rain)
  huc_data_trans$pred_lag <- predict(trans_lag_model, newdata = huc_data_trans)
  huc_data_snow$pred_lag <- predict(snow_lag_model, newdata = huc_data_snow)
  huc_data_rain$pred_lag <- fncRoundLags(the_data = huc_data_rain)
  huc_data_trans$pred_lag <- fncRoundLags(the_data = huc_data_trans)
  huc_data_snow$pred_lag <- fncRoundLags(the_data = huc_data_snow)
 
  # Recombine hydroregion data subsets
  huc_data <- rbind(huc_data_rain, huc_data_trans, huc_data_snow)
  rm(huc_data_rain, huc_data_trans, huc_data_snow)
  
  # Predict antecedent air temperature based on optimal window size
  huc_data <- fncAntecedentAirTemp(the_data = huc_data)
  
  # ADD SPATIAL COVARIATES ----
  huc_data <- merge(huc_data, spatial_data, by = "COMID", all.x = T)

  # Predict ----
  huc_data$prd.stream_temp <-  predict(stream_temp_model, newdata = huc_data)
  huc_data$prd.stream_temp <- ifelse(huc_data$prd.stream_temp < 0, 0, huc_data$prd.stream_temp)

  # Export predictions
  huc_data$tim.date <- as.Date(huc_data$tim.doy, origin = as.Date(paste0(huc_data$tim.year, "-01-01")))
  huc_data <- huc_data[order(huc_data$tim.date),]
  huc_data <- huc_data[, c("lookup", "COMID", "tim.date", "cov.antec_air_temp", "cov.std_mean_flow", "prd.stream_temp")]
  data.table::fwrite(huc_data, file = paste0(prediction_path, "/st_pred_", huc10, ".csv"))
}
close(pb)
print(Sys.time() - start_time) #duration spent processing

