# Produce plots of error by covariates used in the stream temperature model

fncPlotResidsByCovars <- function(the_data, subsname = "full"){
  
  library(mgcv)
  library(visreg)

  plot.dir <- "plots"
  ylb <- expression("Absolute residual" * " (\u00B0C)")
  
  the_data$tim.month <- as.factor(the_data$tim.month)
  
  png(height = 5,5, width = 12, units = "in", res = 400, file = paste0(plot.dir, "/Fig5_resids_vs_covariates_", subsname, ".png"))
  par(mfrow = c(2,4), mar = c(4,4,1,1), ps = 10, cex = 1, oma = c(0,4,0,0))
    
    # temporal
    visreg(gam(abs(model_resid) ~ s(cov.antec_air_temp, k = 5), data = the_data), ylim = c(0,3), xlim = c(-15,30), ylab = "", xlab = expression("Tl" * " (\u00B0C)"))
    visreg(gam(abs(model_resid) ~ s(cov.daylength_hours, k = 5), data = the_data), ylim = c(0,3), ylab = "", xlab = "DL (hours)")
    visreg(gam(abs(model_resid) ~ s(cov.std_mean_flow, k = 5) , data = the_data), ylim = c(0,3), xlim = c(-5,4), ylab = "", xlab = "Qr (log(m3/s))")
    visreg(gam(abs(model_resid) ~ s(cov.SWE_ws, k = 5) , data = the_data), ylim = c(0,3), xlim = c(0,1300), ylab = "", xlab = "Sws (mm)")
    
    # spatial
    visreg(gam(abs(model_resid) ~ s(cov.BFI_cat, k = 5) , data = the_data), ylim = c(0,3), ylab = "", xlab = "BFI (%)")
    visreg(gam(abs(model_resid) ~ s(cov.canopy_line, k = 5) , data = the_data), ylim = c(0,3), ylab = "", xlab = "C (%)")
    visreg(gam(abs(model_resid) ~ s(cov.precip_cat, k = 5) , data = the_data), ylim = c(0,3), xlim = c(0, 4000), ylab = "", xlab = "P (mm)")
    visreg(gam(abs(model_resid) ~ s(cov.air_temp_range_cat, k = 5) , data = the_data), ylim = c(0,3), ylab = "", xlab = expression("R" * " (\u00B0C)"))
    
    mtext(text = ylb, side = 2, line = 2, outer = T, cex = 1.2) 
  
  dev.off()
  
  
  png(height = 12, width = 12, units = "in", res = 400, file = paste0(plot.dir, "/FigS2_resids_vs_covariates_", subsname, ".png"))
  par(mfrow = c(4,4), mar = c(4,4,1,1), ps = 10, cex = 1)
  
    # temporal
    visreg(gam(abs(model_resid) ~ s(cov.air_temp_ws, k = 5) , data = the_data), ylim = c(0,3), xlim = c(-20,30), ylab = ylb, xlab = expression("Tws" * " (\u00B0C)"))
    visreg(gam(abs(model_resid) ~ s(cov.SWE_1Apr, k = 5) , data = the_data), ylim = c(0,3), xlim = c(0,2000), ylab = ylb, xlab = "Qr (log(m3/s))")
    visreg(gam(abs(model_resid) ~ s(tim.doy, k = 5) , data = the_data), ylim = c(0,3), ylab = ylb, xlab = "SA1 (mm)")

    # spatial
    visreg(gam(abs(model_resid) ~ s(cov.lat_v, k = 5) , data = the_data), ylim = c(0,3), ylab = ylb, xlab = "L (degrees)")
    visreg(gam(abs(model_resid) ~ s(cov.elev_mean_smo, k = 5) , data = the_data), ylim = c(0,3), xlim = c(0,2500), ylab = ylb, xlab = "E (m)")
    visreg(gam(abs(model_resid) ~ s(cov.area_km2_ws_log, k = 5) , data = the_data), ylim = c(0,3), xlim = c(0,10), ylab = ylb, xlab = "A (log(km2))")
    visreg(gam(abs(model_resid) ~ s(cov.elev_diff, k = 5) , data = the_data), ylim = c(0,3), xlim = c(0,1300), ylab = ylb, xlab = "Ed (m)")
    visreg(gam(abs(model_resid) ~ s(cov.slope, k = 5) , data = the_data), ylim = c(0,3), xlim = c(0,0.4), ylab = ylb, xlab = "S (gradient)")
    visreg(gam(abs(model_resid) ~ s(cov.pct_ice_ws, k = 5) , data = the_data), ylim = c(0,3), ylab = ylb, xlab = "I (%)")
    visreg(gam(abs(model_resid) ~ s(cov.pct_for_all_cat_rip100m, k = 5) , data = the_data), ylim = c(0,3), ylab = ylb, xlab = "F (%)")
    visreg(gam(abs(model_resid) ~ s(cov.pct_urb_all_ws, k = 5) , data = the_data), ylim = c(0,3), ylab = ylb, xlab = "U (%)")
    visreg(gam(abs(model_resid) ~ s(cov.pct_extru_vol_ws, k = 5) , data = the_data), ylim = c(0,3), ylab = ylb, xlab = "V (%)")
    visreg(gam(abs(model_resid) ~ s(cov.pct_wet_all_ws, k = 5) , data = the_data), ylim = c(0,3), xlim = c(0,20), ylab = ylb, xlab = "W (%)")

  dev.off()
}
# usage: fncPlotResidsByCovars(the_data = freeflow_data[sample(nrow(freeflow_data), 10000),])
  
  
  
