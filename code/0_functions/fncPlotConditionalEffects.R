# Make conditional effects plots

fncPlotConditionalEffects <- function(the_data, the_model, subsname = "full"){

library(mgcv)
library(visreg)
library(ggpubr)
library(gridExtra)

l <-  list(visreg::visreg2d(the_model, x = "cov.daylength_hours", y = "cov.antec_air_temp", plot.type = "gg",
             xlab = expression(italic("DL") * " (hours)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
             scale_fill_viridis_c(limits = c(0, 19), option = "magma", name = expression(italic("Tw") * " (\u00B0C)")) +
             geom_point(aes(cov.daylength_hours, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") + 
             geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(ylim = c(-10,28)) +
             theme(legend.text = element_text(size = 14))) 
leg <- ggpubr::get_legend(l[[1]]) 

# Figure 7
g1 <- list(
  visreg::visreg2d(the_model, x = "cov.air_temp_ws", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("Tws") * " (\u00B0C)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.air_temp_ws, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") + 
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(xlim = c(-10,28), ylim = c(-10,28)) +  
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),  
    plot.tag = element_text(size = 24)) +  labs(tag = "  A"),
  
  visreg::visreg2d(the_model, x = "cov.daylength_hours", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("DL") * " (hours)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.daylength_hours, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") + 
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(ylim = c(-10,28)) +  
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),  
    plot.tag = element_text(size = 24)) +  labs(tag = "  B"),
  
  visreg::visreg2d(the_model, x = "cov.std_mean_flow", y = "cov.antec_air_temp", plot.type = "gg", 
    xlab = expression(italic("Qr") * " (log(CMS))"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.std_mean_flow, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") + 
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(xlim = c(-4,3), ylim = c(-10,28)) +  
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),  
    plot.tag = element_text(size = 24)) +  labs(tag = "  C"),
  
  visreg::visreg2d(the_model, x = "cov.SWE_ws", y = "cov.air_temp_ws", plot.type = "gg", 
    xlab = expression(italic("Sws") * " (mm)"), ylab = expression(italic("Tws") * " (\u00B0C)")) + 
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.SWE_ws,  cov.air_temp_ws), data = the_data, col = 'grey85', pch = ".") + 
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(xlim = c(0, 1000), ylim = c(-10,28)) + 
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),  
    plot.tag = element_text(size = 24)) +  labs(tag = "  D"),
  
  visreg::visreg2d(the_model, x = "cov.elev_mean_smo", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("E") * " (m)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.elev_mean_smo, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(xlim = c(0, 2100), ylim = c(-10, 28)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  E"),
  
  visreg::visreg2d(the_model, x = "cov.elev_diff", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("Ed") * " (m)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.elev_diff, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(xlim = c(0, 1100), ylim = c(-10, 28)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  F"),
  
  visreg::visreg2d(the_model, x = "cov.area_km2_ws_log", y = "cov.antec_air_temp", plot.type = "gg", 
    xlab = expression(italic("A") * " (log(km^2))"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.area_km2_ws_log, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") + 
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(xlim = c(0, 7.5), ylim = c(-10, 28)) + 
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),  
    plot.tag = element_text(size = 24)) +  labs(tag = "  G"),
  
  visreg::visreg2d(the_model, x = "cov.BFI_cat", y = "cov.antec_air_temp", plot.type = "gg", 
    xlab = expression(italic("BFI") * " (proportion)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.BFI_cat, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") + 
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(ylim = c(-10, 28)) +  
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),  
    plot.tag = element_text(size = 24)) +  labs(tag = "  H"),
  
  visreg::visreg2d(the_model, x = "cov.pct_for_all_cat_rip100m", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("F") * " (% forested in 100m buffer)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.pct_for_all_cat_rip100m, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") + 
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(ylim = c(-10, 28)) +  
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16), 
    plot.tag = element_text(size = 24)) +  labs(tag = "  I"),
  
  visreg::visreg2d(the_model, x = "tim.doy", y = "cov.SWE_1Apr", plot.type = "gg",
    xlab = expression(italic("D") * " (day of year)"), ylab = expression(italic("SA1") * " (mm)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(tim.doy, cov.SWE_1Apr), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) +  coord_cartesian(ylim = c(0, 1800)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  J"),
  
  visreg::visreg2d(the_model, x = "cov.elev_mean_smo", y = "cov.area_km2_ws_log", plot.type = "gg", 
    xlab = expression(italic("E") * " (m)"), ylab = expression(italic("A") * " (log(km^2))")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.elev_mean_smo, cov.area_km2_ws_log), data = the_data, col = 'grey85', pch = ".") + 
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(xlim = c(0, 2100)) +  
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),  
    plot.tag = element_text(size = 24)) +  labs(tag = "  K"),
  
  leg
)

png(height = 16, width = 13, units = "in", res = 400, file = paste0(plot.dir, "/Fig7_Conditional_effects_", subsname, ".png"))
grid.arrange(
  grobs = g1,
  widths = c(2.5, 2.5, 2.5, 0.25),
  layout_matrix = rbind(c(1, 2, 3),
                        c(4, 5, 6),
                        c(7, 8, 9), 
                        c(10, 11, 12))
)
dev.off()

# Figure S3
g2 <- list(
  visreg::visreg2d(the_model, x = "cov.slope", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("S") * " (degrees)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.slope, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(xlim = c(0, 0.25), ylim = c(-10, 28)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  A"),

  visreg::visreg2d(the_model, x = "cov.lat_v", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("Latitude")), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.lat_v, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(ylim = c(-10, 28)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  B"),
  
  visreg::visreg2d(the_model, x = "cov.canopy_line", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("C") * " (% canopy cover)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.canopy_line, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(ylim = c(-10, 28)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  C"),
  
  visreg::visreg2d(the_model, x = "cov.pct_wet_all_ws", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("W") * " (%)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.pct_wet_all_ws, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(ylim = c(-10, 28)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  D"),
  
  visreg::visreg2d(the_model, x = "cov.pct_ice_ws", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("I") * " (% ice)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.pct_ice_ws, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(xlim = c(0, 10), ylim = c(-10, 28)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  E"),

  visreg::visreg2d(the_model, x = "cov.pct_extru_vol_ws", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("V") *" (%)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.pct_extru_vol_ws, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(ylim = c(-10, 28)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  F"),

  visreg::visreg2d(the_model, x = "cov.pct_urb_all_ws", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("U") *" (% urban)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.pct_urb_all_ws, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(ylim = c(-10, 28)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  G"),
  
  visreg::visreg2d(the_model, x = "cov.air_temp_range_cat", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("R") * " (\u00B0C)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.air_temp_range_cat, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(ylim = c(-10, 28)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  H"),
  
  visreg::visreg2d(the_model, x = "cov.precip_cat", y = "cov.antec_air_temp", plot.type = "gg",
    xlab = expression(italic("P") * " (mm)"), ylab = expression(italic("Tl") * " (\u00B0C)")) +
    scale_fill_viridis_c(limits = c(0, 19), option = "magma") +
    geom_point(aes(cov.precip_cat, cov.antec_air_temp), data = the_data, col = 'grey85', pch = ".") +
    geom_contour(aes(z = z), color = "grey40", binwidth = 1) + coord_cartesian(xlim = c(150, 3500), ylim = c(-10, 28)) +
    theme(legend.position  = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 16),
    plot.tag = element_text(size = 24)) +  labs(tag = "  I"),

  leg
)
png(height = 12, width = 13, units = "in", res = 400, file = paste0(plot.dir, "/FigS3_conditional_effects_", subsname, ".png"))
grid.arrange(
  grobs = g2,
  widths = c(2.4, 2.4, 2.4, 0.5),
  layout_matrix = rbind(c(1, 2, 3, NA),
                        c(4, 5, 6, 12),
                        c(7, 8, 9, NA))
)
dev.off()

}
