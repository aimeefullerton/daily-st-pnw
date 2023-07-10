# Function for fitting the stream temperature model

fncFitModel <- function(the_data = freeflow_data){
  
# Fit the model using tensor smoothers (ti) and linear effects for spatial covariates

model <- mgcv::gam(obs.stream_temp_daily_mean ~ 
                     ti(cov.antec_air_temp, k = 6) + 
                     ti(cov.air_temp_ws, k = 3) + 
                     ti(cov.daylength_hours, k = 4) + 
                     ti(cov.std_mean_flow, k = 4) + 
                     ti(cov.SWE_ws, k = 6) + 
                     ti(cov.air_temp_ws, cov.antec_air_temp, k = c(3, 6)) + 
                     ti(cov.daylength_hours, cov.antec_air_temp, k = c(4, 6)) + 
                     ti(cov.std_mean_flow, cov.antec_air_temp, k = c(4, 6)) + 
                     ti(cov.SWE_ws, cov.air_temp_ws, k = c(6, 6)) +
                     cov.lat_v * cov.antec_air_temp + 
                     cov.elev_mean_smo * cov.antec_air_temp + 
                     cov.area_km2_ws_log * cov.antec_air_temp + 
                     cov.BFI_cat * cov.antec_air_temp + 
                     cov.elev_diff * cov.antec_air_temp + 
                     cov.slope * cov.antec_air_temp + 
                     cov.pct_wet_all_ws * cov.antec_air_temp +
                     cov.pct_ice_ws * cov.antec_air_temp +
                     cov.pct_for_all_cat_rip100m * cov.antec_air_temp + 
                     cov.canopy_line * cov.antec_air_temp +
                     cov.pct_urb_all_ws * cov.antec_air_temp + 
                     cov.pct_extru_vol_ws * cov.antec_air_temp + 
                     cov.precip_cat * cov.antec_air_temp + 
                     cov.air_temp_range_cat * cov.antec_air_temp +
                     cov.SWE_1Apr * tim.doy +
                     cov.elev_mean_smo * cov.area_km2_ws_log,
                   data = the_data)

  return(model)
  
}

