# Create Plots to Evaluate Lag for Antecedent Air Temperature Covariate

fncPlotAntecedentLag <- function(rain_lag_model, trans_lag_model, snow_lag_model, rain_corr_categs, trans_corr_categs, snow_corr_categs,
                        freeflow_data_rain, freeflow_data_trans, freeflow_data_snow, mod_subset_name = "full"){
  
# Colors for plots
blue1 <- rgb(0, 0, 200, max = 255, alpha = 170, names = "blue50")
blue2 <- rgb(0, 0, 255, max = 255, alpha = 50, names = "blue50")
green1 <- rgb(0, 150, 0, max = 255, alpha = 170, names = "blue50")
green2 <- rgb(0, 255, 0, max = 255, alpha = 50, names = "blue50")
red1 <- rgb(200, 0, 0, max = 255, alpha = 170, names = "blue50")
red2 <- rgb(255, 0, 0, max = 255, alpha = 50, names = "blue50")

# 3- Paneled plot  
png(height = 3.25, width = 9, units = "in", res = 400, file = paste0(plot.dir, "/Fig4_AirTlag_Stdflow_byHydroregion_", mod_subset_name, ".png"))
par(mfrow = c(1,3), mar = c(4, 4, 3, 1), ps = 11, cex = 1)

visreg::visreg(gam(cov.std_mean_flow ~ s(tim.doy), data = freeflow_data_rain), line = list(lty = 2, col = red1), 
       ylim = c(-2.5,2), xlim = c(0,365), partial = FALSE, rug = FALSE, 
       ylab = "Standardized log flow (CMS)", xlab = "Julian day", fill = list(col = red2)); abline(h = 0)

par(new = T)
visreg::visreg(gam(cov.std_mean_flow ~ s(tim.doy), data = freeflow_data_trans), line = list(lty = 3, col = green1), 
       ylim = c(-2.5,2), xlim = c(0,365),  partial = FALSE, rug = FALSE, xlab = "", ylab = "", 
       fill = list(col = green2)); abline(h = 0)

par(new = T)
visreg::visreg(gam(cov.std_mean_flow ~ s(tim.doy), data = freeflow_data_snow), line = list(lty = 1, col = blue1), 
       ylim = c(-2.5,2),xlim = c(0,365),   partial = FALSE, rug = FALSE, xlab = "", ylab = "", 
       fill = list(col = blue2)); abline(h = 0)
text(4, 1.92,"A", cex = 1.1)


visreg::visreg(snow_lag_model, data = snow_corr_categs, ylim = c(0,65), xlim = c(-4.5,3), fill = list(col = blue2), jitter = T, 
       line = list(lty = 1,col = blue1), ylab = "Antecedent air temperature window (days)", 
       xlab = "Standardized log flow (CMS)", partial = FALSE, rug = FALSE)

par(new = T)
visreg::visreg(rain_lag_model, data = rain_corr_categs, ylim = c(0,65), xlim = c(-4.5,3), fill = list(col = red2), jitter = T, 
       line = list(lty = 2,col = red1), ylab = "", xlab = "", partial = FALSE, rug = FALSE)

par(new = T)
visreg::visreg(trans_lag_model, data = trans_corr_categs, ylim = c(0,65), xlim = c(-4.5,3), fill = list(col = green2), jitter = T, 
       line = list(lty = 3,col = green1), ylab = "", xlab = "", partial = FALSE, rug = FALSE)
abline(h = 0)
text(-4.3, 64,"B", cex = 1.1)

points(jitter(lag_choice) ~ cov.std_mean_flow, data  = snow_corr_categs, cex = .3, pch = 1, col = blue2)
points(jitter(lag_choice) ~ cov.std_mean_flow, data  = rain_corr_categs, cex = .3, pch = 2, col = red2)
points(jitter(lag_choice) ~ cov.std_mean_flow, data  = trans_corr_categs, cex = .3, pch = 3, col = green2)

legend("left", c("Snow", "Transitional", "Rain"), lty = c(1, 3, 2), col = c(blue1, green1, red1), bty = "n", lwd = 2)

visreg::visreg(gam(cor_max ~ s(cov.std_mean_flow, k = 6), data = snow_corr_categs), ylim = c(.7,0.92), xlim = c(-4.5,3),
       fill = list(col = blue2), line = list(lty = 1, col = blue1), ylab = "Max correlation", xlab = "Standardized log flow (CMS)", 
       partial = FALSE, rug = FALSE)

par(new = T)
visreg::visreg(gam(cor_max ~ s(cov.std_mean_flow, k = 6), data = rain_corr_categs), ylim = c(.7,0.92), xlim = c(-4.5,3),
       fill = list(col = red2), line = list(lty = 2,col = red1), ylab = "Max correlation", xlab = "Standardized log flow (CMS)", 
       partial = FALSE, rug = FALSE)

par(new = T)
visreg::visreg(gam(cor_max ~ s(cov.std_mean_flow, k = 6), data = trans_corr_categs), ylim = c(.7,0.92), xlim = c(-4.5,3),
       fill = list(col = green2), line = list(lty = 3, col = green1), ylab = "Max correlation", xlab = "Standardized log flow (CMS)", 
       partial = FALSE, rug = FALSE)

text(-4.3,0.915,"C", cex = 1.1)

points((cor_max) ~ cov.std_mean_flow, data  = snow_corr_categs, cex = .3, pch = 1, col = blue2)
points((cor_max) ~ cov.std_mean_flow, data  = rain_corr_categs, cex = .3, pch = 2, col = red2)
points((cor_max) ~ cov.std_mean_flow, data  = trans_corr_categs, cex = .3, pch = 3, col = green2)

dev.off()
}




