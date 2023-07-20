# This script fits a stream temperature model for free-flowing reaches in the PNW (region 17)
# NOTE: there is an interactive portion for subsetting on line 28; type answer in the console when prompted.
# If anything other than "full" is chosen, there will be another choice around line 40 

# SETUP ----

# Directories
plot.dir <- "plots"
dir.create(plot.dir, showWarnings = F)
plot_it <- T
dir.create("data/results", showWarnings = F)

# Load packages
library(dplyr)
library(mgcv)

# Functions
source("code/0_functions/fncChoose.R")
source("code/0_functions/fncFreeFlowing.R")
source("code/0_functions/fncSubsetBy.R")
source("code/0_functions/fncStandardizedFlow.R")
source("code/0_functions/fncHydroRegion.R")
source("code/0_functions/fncAntecedentPeriod.R")
source("code/0_functions/fncPlotAntecedentLag.R")
source("code/0_functions/fncAntecedentAirTemp.R")
source("code/0_functions/fncFitModel.R")
source("code/0_functions/fncPlotConditionalEffects.R")
source("code/0_functions/fncPlotResidsByCovars.R")

# Choose subset if applicable
mod_subset_name <- fncChoose(choices = c("full", "season", "region", "season-region"))
models_path <- paste0("data/results/", mod_subset_name)
dir.create(models_path, showWarnings = F)

# LOAD DATA ----
fitting_data <- fst::read_fst("data/fitting_data.fst", as.data.table = T)

# SUBSET DATA ----

# Subset to free-flowing reaches
freeflow_data <- fncFreeFlowing(the_data = fitting_data, PDI = 0.05)
rm(fitting_data)

# Subset to season, region, or season-region if applicable
subset_out <- fncSubsetBy(the_data = freeflow_data, split_by = mod_subset_name, julian_day_to_split = 210)
if(!is.null(subset_out)){
  print(names(subset_out))
  ans<- fncChoose(choices = names(subset_out))
  freeflow_data <- subset_out[[ans]]
  rm(subset_out)
}
  
# ADD STANDARDIZED FLOW ----
freeflow_data <- fncStandardizedFlow(the_data = freeflow_data)

# Remove really large watersheds
freeflow_data <- subset(freeflow_data, cov.area_km2_ws < 20000)

# SPLIT INTO HYDROLOGICAL REGIONS ----
hyd_reg <- fncHydroRegion(the_data = freeflow_data)
freeflow_data_rain <- hyd_reg[["rain"]]
freeflow_data_trans <- hyd_reg[["trans"]]
freeflow_data_snow <- hyd_reg[["snow"]]
rm(hyd_reg, freeflow_data)

# Evaluate: summarize mean across years for each class
# count is total n observations
# n_COMIDS is number of unique reaches, by year

r <- summarise(group_by(freeflow_data_rain, tim.year),  count = n(), n_COMIDs = length(unique(COMID)), SWE = mean(cov.SWE_mean_year), SWE_sd = sd(cov.SWE_mean_year))
t <- summarise(group_by(freeflow_data_trans, tim.year), count = n(), n_COMIDs = length(unique(COMID)), SWE = mean(cov.SWE_mean_year), SWE_sd = sd(cov.SWE_mean_year))
s <- summarise(group_by(freeflow_data_snow, tim.year),  count = n(), n_COMIDs = length(unique(COMID)), SWE = mean(cov.SWE_mean_year), SWE_sd = sd(cov.SWE_mean_year))

(mean(r$n_COMIDs)); (range(r$n_COMIDs))
(mean(t$n_COMIDs)); (range(t$n_COMIDs))
(mean(s$n_COMIDs)); (range(s$n_COMIDs))

# PREDICT ANTECEDENT AIR TEMPERATURE ----

# Get optimal antecedent period duration
rain_out <- fncAntecedentPeriod(the_data = freeflow_data_rain)
trans_out <- fncAntecedentPeriod(the_data = freeflow_data_trans)
snow_out <- fncAntecedentPeriod(the_data = freeflow_data_snow)

# Add predicted optimal antecedent duration
freeflow_data_rain$pred_lag <- rain_out[[1]]
freeflow_data_trans$pred_lag <- trans_out[[1]]
freeflow_data_snow$pred_lag <- snow_out[[1]]

# Save optimal antecedent duration models
assign("rain_lag_model", rain_out[[2]])
assign("trans_lag_model", trans_out[[2]])
assign("snow_lag_model", snow_out[[2]])

# Correlation categories
assign("rain_corr_categs", rain_out[[3]])
assign("trans_corr_categs", trans_out[[3]])
assign("snow_corr_categs", snow_out[[3]])

# Plots to evaluate lags
if(plot_it == T) fncPlotAntecedentLag(rain_lag_model, trans_lag_model, snow_lag_model, rain_corr_categs, trans_corr_categs, snow_corr_categs,
                freeflow_data_rain, freeflow_data_trans, freeflow_data_snow, mod_subset_name)

# Predict antecedent air temperature based on optimal window durations for rain, trans, and snow subsets
freeflow_data_rain <- fncAntecedentAirTemp(the_data = freeflow_data_rain)
freeflow_data_trans <- fncAntecedentAirTemp(the_data = freeflow_data_trans)
freeflow_data_snow <- fncAntecedentAirTemp(the_data = freeflow_data_snow)

# Recombine hydroregion data subsets
freeflow_data <- rbind(freeflow_data_rain, freeflow_data_trans, freeflow_data_snow)

# EXPORT DATA WITH ANTECEDENT AIR INFO & LAG MODEL ----
ifelse(mod_subset_name == "full", subsname <- "full", subsname <- paste0(mod_subset_name, "_", ans))
save(rain_lag_model, trans_lag_model, snow_lag_model, file = paste0(models_path, "/antec_air_temp_duration_models.RData"))
fst::write_fst(freeflow_data, paste0("data/freeflow_data_", subsname, ".fst"), compress = 80)
rm(freeflow_data_rain, freeflow_data_trans, freeflow_data_snow, rain_corr_categs, trans_corr_categs, snow_corr_categs,
   rain_out, trans_out, snow_out, r, t, s, rain_lag_model, trans_lag_model, snow_lag_model)
gc()

# FIT MODEL ----
stream_temp_model <- fncFitModel(the_data = freeflow_data)
summary(stream_temp_model)
sum(stream_temp_model$edf)
save(stream_temp_model, file = paste0(models_path, "/fitted_model.RData"))

# PREDICT ----
freeflow_data$model_pred <-  predict(stream_temp_model, newdata = freeflow_data)
freeflow_data$model_pred <- ifelse(freeflow_data$model_pred < 0, 0, freeflow_data$model_pred)
freeflow_data$model_resid <- freeflow_data$obs.stream_temp_daily_mean - freeflow_data$model_pred

# COMPUTE MAE and RMSE ----

# Create container for holding summary results
results_summary <- matrix(ncol = 3, nrow = 5)
colnames(results_summary) <- c("Full", "Txv", "Sxv")
row.names(results_summary) <- c("RMSE_daily", "MAE_daily", "RMSE_monthly", "MAE_monthly", "N")

# Add MAE and RMSE to results summary
results_summary[1,1] <- (mean((freeflow_data$model_resid) ^ 2,na.rm = T)) ^ .5
results_summary[2,1] <- mean(abs(freeflow_data$model_resid), na.rm = T)

# View results subsetted by hydrological region
mean(abs(subset(freeflow_data, cov.SWE_mean_year < 20)$model_resid), na.rm = T)
mean(abs(subset(freeflow_data, cov.SWE_mean_year >= 20 & cov.SWE_mean_year < 100)$model_resid), na.rm = T)
mean(abs(subset(freeflow_data, cov.SWE_mean_year >= 100)$model_resid), na.rm = T)

# Monthly residuals; add to summary file
freeflow_data$tim.date <- as.Date(freeflow_data$tim.doy, origin = as.Date(paste0(freeflow_data$tim.year, "-01-01")))
freeflow_data$tim.month <- lubridate::month(freeflow_data$tim.date)
freeflow_data$COMID_year_month <- paste0(freeflow_data$COMID, "_", freeflow_data$tim.year, "_", freeflow_data$tim.month)
pred_mean_monthly <- as.data.frame.table(tapply(abs(freeflow_data$model_pred), freeflow_data$COMID_year_month, mean, na.rm = T))
obs_mean_monthly <- as.data.frame.table(tapply(abs(freeflow_data$obs.stream_temp_daily_mean), freeflow_data$COMID_year_month, mean, na.rm = T))
compare_monthly <- merge(obs_mean_monthly, pred_mean_monthly, by = "Var1", all.x = T)
colnames(compare_monthly) <- c("COMID_year_month", "obs_mean_monthly", "pred_mean_monthly")
plot(obs_mean_monthly ~ pred_mean_monthly, data = compare_monthly, pch = "."); abline(0, 1, col = "red")
compare_monthly$resid  <- compare_monthly$obs_mean_monthly - compare_monthly$pred_mean_monthly
results_summary[3,1] <- (mean((compare_monthly$resid) ^ 2, na.rm = T)) ^ .5
results_summary[4,1] <- mean(abs(compare_monthly$resid ), na.rm = T)
results_summary[5,1] <- nrow(freeflow_data)

# Residuals by site
resid_by_site <- as.data.frame.table(tapply(freeflow_data$model_resid, freeflow_data$NorWeST_ID, mean), na.rm = T) #or PERMA_FID instead of COMID
colnames(resid_by_site) <- c("NorWeST_ID", "mean_resid")

# PLOTS ----
 
  # Conditional Effects
  if(plot_it == T) {set.seed(123); fncPlotConditionalEffects(the_data = dplyr::sample_n(freeflow_data, 10000), the_model = stream_temp_model)}

  # Residuals by Covariates
  if(plot_it == T) {set.seed(123); fncPlotResidsByCovars(the_data = dplyr::sample_n(freeflow_data, 10000))}

# TEMPORAL CROSS-VALIDATION (LOYOCV; Leave One Year Out) ----

# Initialize container dataframe
freeflow_Txv <- freeflow_data[0,]
freeflow_Txv$pred_Txv <- NA

# Loop
for(y in 1993:2013){ #years with response data
  print(y)
  # Separate data into fitting and testing subsets
  freeflow_test <- freeflow_data[freeflow_data$tim.year == y, ]
  freeflow_fit <- freeflow_data[freeflow_data$tim.year != y, ]

  region17_model_Txv <- fncFitModel(the_data = freeflow_fit)
  
  freeflow_test$pred_Txv <- predict(region17_model_Txv, newdata = freeflow_test)
  freeflow_Txv <- rbind(freeflow_Txv, freeflow_test)
}

# Add residuals to results file
freeflow_Txv$pred_Txv  <- ifelse(freeflow_Txv$pred_Txv < 0, 0, freeflow_Txv$pred_Txv)
freeflow_Txv$resid_Txv <- freeflow_Txv$obs.stream_temp_daily_mean - freeflow_Txv$pred_Txv

# Add MAE and RMSPE stats to the results summary table
results_summary[1,2] <- (mean((freeflow_Txv$resid_Txv) ^ 2, na.rm = T)) ^ .5
results_summary[2,2] <- mean(abs(freeflow_Txv$resid_Txv ), na.rm = T)

# View results summarized by year and month
tapply(abs(freeflow_Txv$resid_Txv), freeflow_Txv$tim.year, mean, na.rm = T)
tapply(abs(freeflow_Txv$resid_Txv), freeflow_Txv$tim.month, mean, na.rm = T)

# Monthly Txv residuals; add to summary file
freeflow_Txv$COMID_year_month <- paste0(freeflow_Txv$COMID, "_", freeflow_Txv$tim.year, "_", freeflow_Txv$tim.month)
pred_mean_monthly <- as.data.frame.table(tapply(abs(freeflow_Txv$pred_Txv), freeflow_Txv$COMID_year_month, mean, na.rm = T))
obs_mean_monthly <- as.data.frame.table(tapply(abs(freeflow_Txv$obs.stream_temp_daily_mean), freeflow_Txv$COMID_year_month, mean, na.rm = T))
compare_monthly <- merge(obs_mean_monthly, pred_mean_monthly, by = "Var1", all.x = T)
colnames(compare_monthly) <- c("COMID_year_month", "obs_mean_monthly", "pred_mean_monthly")
plot(obs_mean_monthly ~ pred_mean_monthly, data = compare_monthly, pch = "."); abline(0, 1, col = "red")
compare_monthly$resid  <- compare_monthly$obs_mean_monthly - compare_monthly$pred_mean_monthly
results_summary[3,2] <- (mean((compare_monthly$resid) ^ 2, na.rm = T)) ^ .5
results_summary[4,2] <- mean(abs(compare_monthly$resid ), na.rm = T)
results_summary[5,2] <- nrow(freeflow_Txv)

fst::write_fst(freeflow_Txv, paste0("data/freeflow_Txv_", subsname, ".fst"), compress = 100)


# SPATIAL CROSS-VALIDATION (LOROCV; Leave One Region Out) ----

if(!mod_subset_name %in% c("region", "season-region")){
# Initialize container dataframe
freeflow_Sxv <- freeflow_data[0,]
freeflow_Sxv$pred_Sxv <- NA

# Loop
for(r in unique(freeflow_data$cov.NorWeST_region)){
  print(r)
  # Separate data into fitting and testing subsets
  freeflow_test <- freeflow_data[freeflow_data$cov.NorWeST_region == r, ]
  freeflow_fit <- freeflow_data[freeflow_data$cov.NorWeST_region != r, ]
  
  region17_model_Sxv <- fncFitModel(the_data = freeflow_fit)
  
  freeflow_test$pred_Sxv <- predict(region17_model_Sxv, newdata = freeflow_test)
  freeflow_Sxv <- rbind(freeflow_Sxv, freeflow_test)
}

# Add residuals to results file
freeflow_Sxv$pred_Sxv  <- ifelse(freeflow_Sxv$pred_Sxv < 0, 0, freeflow_Sxv$pred_Sxv)
freeflow_Sxv$resid_Sxv <- freeflow_Sxv$obs.stream_temp_daily_mean - freeflow_Sxv$pred_Sxv

# Add MAE and RMSPE stats to the results summary table
results_summary[1,3] <- (mean((freeflow_Sxv$resid_Sxv) ^ 2,na.rm = T)) ^ .5
results_summary[2,3] <- mean(abs(freeflow_Sxv$resid_Sxv ), na.rm = T)

# View results summarized by region
tapply(abs(freeflow_Sxv$resid_Sxv), freeflow_Sxv$cov.NorWeST_region, mean, na.rm = T)

# Monthly Sxv residuals; add to summary file
freeflow_Sxv$COMID_year_month <- paste0(freeflow_Sxv$COMID, "_", freeflow_Sxv$tim.year, "_", freeflow_Sxv$tim.month)
pred_mean_monthly <- as.data.frame.table(tapply(abs(freeflow_Sxv$pred_Sxv), freeflow_Sxv$COMID_year_month, mean, na.rm = T))
obs_mean_monthly <- as.data.frame.table(tapply(abs(freeflow_Sxv$obs.stream_temp_daily_mean), freeflow_Sxv$COMID_year_month, mean, na.rm = T))
compare_monthly <- merge(obs_mean_monthly, pred_mean_monthly, by = "Var1", all.x = T)
colnames(compare_monthly) <- c("COMID_year_month", "obs_mean_monthly", "pred_mean_monthly")
plot(obs_mean_monthly ~ pred_mean_monthly, data = compare_monthly, pch = "."); abline(0, 1, col = "red")
compare_monthly$resid  <- compare_monthly$obs_mean_monthly - compare_monthly$pred_mean_monthly
results_summary[3,3] <- (mean((compare_monthly$resid) ^ 2, na.rm = T)) ^ .5
results_summary[4,3] <- mean(abs(compare_monthly$resid ), na.rm = T)
results_summary[5,3] <- nrow(freeflow_Sxv)
results_summary <- cbind.data.frame("Result" = row.names(results_summary), results_summary)

fst::write_fst(freeflow_Sxv, paste0("data/freeflow_Sxv_", subsname, ".fst"), compress = 100)
}

# EXPORT FIT STATISTICS ----
write.csv(results_summary, paste0("data/freeflow_data_results_summary_", subsname, ".csv"))
write.csv(resid_by_site, paste0("data/resid_site_alldata_", subsname, ".csv"))
fst::write_fst(freeflow_data, paste0("data/freeflow_data_", subsname, ".fst"), compress = 100)
