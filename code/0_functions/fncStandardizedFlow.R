# Function predicts standardized flow

# param1 & param2 are from a previously-established relationship (Siegel et al. 2021)
# ws_area_too_small = drainage area below which to exclude as too small
# extreme log outlier values beyond which to exclude

fncStandardizedFlow <- function(the_data, param1 = -4.1, param2 = 0.93, outliers_exclude = -5){

  # Predict flow from log drainage area 
  the_data$cov.area_mean_flow  <- param1 + param2 * log(the_data$cov.area_km2_ws)
  
  # Check that there are non-NA data
  if(any(!is.na(the_data$cov.NWM_flow)) == F) {
    the_data$cov.NWM_flow <- 0.00001
    the_data$cov.NWM_flow_log <- log(the_data$cov.NWM_flow)
  }
  
  # Calculate standardized flow (i.e., residuals)
  the_data$cov.std_mean_flow <- the_data$cov.NWM_flow_log - the_data$cov.area_mean_flow
  
  # Exclude extreme outliers
  the_data$cov.std_mean_flow <- ifelse(the_data$cov.std_mean_flow < outliers_exclude, outliers_exclude, the_data$cov.std_mean_flow)
  
return(the_data)
}

