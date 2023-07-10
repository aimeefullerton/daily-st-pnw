# Site characteristics plot

# SETUP ----

# Load packages
library(visreg)

# Directories
plot.dir <- "plots"

# LOAD DATA ----
freeflow_data <- fst::read_fst("data/freeflow_data.fst", as.data.table = T)
freeflow_data$COMID_year <- paste0(freeflow_data$COMID, "_", freeflow_data$tim.year)


# PROCESS DATA FOR PLOTS ----
Site_summary <- as.data.frame.table(tapply(freeflow_data$cov.SWE_mean_year, freeflow_data$COMID_year, mean, na.rm = T))
colnames(Site_summary) <- c("COMID_year", "cov.SWE_mean_year")
at <- as.data.frame.table(tapply(freeflow_data$cov.air_temp_mean_cat, freeflow_data$COMID_year, mean, na.rm = T))
colnames(at) <- c("COMID_year", "cov.air_temp_mean_cat")
pp <- as.data.frame.table(tapply(freeflow_data$cov.precip_cat, freeflow_data$COMID_year, mean, na.rm = T))
colnames(pp) <- c("COMID_year", "cov.precip_cat")
qq <- as.data.frame.table(tapply(freeflow_data$cov.std_mean_flow, freeflow_data$COMID_year, mean, na.rm = T))
colnames(qq) <- c("COMID_year", "cov.std_mean_flow")
em <- as.data.frame.table(tapply(freeflow_data$cov.elev_mean_smo, freeflow_data$COMID_year, mean, na.rm = T))
colnames(em) <- c("COMID_year", "cov.elev_mean_smo")
ew <- as.data.frame.table(tapply(freeflow_data$cov.elev_ws, freeflow_data$COMID_year, mean, na.rm = T))
colnames(ew) <- c("COMID_year", "cov.elev_ws")
aw <- as.data.frame.table(tapply(freeflow_data$cov.area_km2_ws_log, freeflow_data$COMID_year, mean, na.rm = T))
colnames(aw) <- c("COMID_year", "cov.area_km2_ws_log")
bf <- as.data.frame.table(tapply(freeflow_data$cov.BFI_cat, freeflow_data$COMID_year, mean, na.rm = T))
colnames(bf) <- c("COMID_year", "cov.BFI_cat")

# Summarize 
Site_summary <- merge(Site_summary, at, by = "COMID_year", all.x = T)
Site_summary <- merge(Site_summary, pp, by = "COMID_year", all.x = T)
Site_summary <- merge(Site_summary, qq, by = "COMID_year", all.x = T)
Site_summary <- merge(Site_summary, em, by = "COMID_year", all.x = T)
Site_summary <- merge(Site_summary, ew, by = "COMID_year", all.x = T)
Site_summary <- merge(Site_summary, aw, by = "COMID_year", all.x = T)
Site_summary <- merge(Site_summary, bf, by = "COMID_year", all.x = T)

Site_summary$Site_type <- ifelse(Site_summary$cov.SWE_mean < 20, "Rain", ifelse(Site_summary$cov.SWE_mean < 100, "Trans.", "Snow"))
Site_summary$Site_type <- factor(Site_summary$Site_type)
Site_summary$Site_type  <- factor(Site_summary$Site_type  , levels = c("Rain", "Trans.", "Snow"))

# MAKE BOXPLOTS ----
the_colors <- c("lightcoral","green3", "dodgerblue")
png(filename = paste0(plot.dir, "/Fig3_Site_characteristics.png"), width = 8.5, height = 4, units = "in", res = 400)
  par(mfrow = c(2,4), mar = c(2,4,1,1), ps = 11, las = 1)
  
  boxplot(cov.SWE_mean_year ~ Site_type, data = Site_summary, col = the_colors, ylab = "", xlab =  NULL, pch = ".")
  title(ylab = "Mean annual watershed SWE (mm)", line = 3, cex.lab = 1)
  legend("topleft", "A", bty = "n")
  
  boxplot(cov.std_mean_flow ~ Site_type, data = Site_summary, col = the_colors, ylab = "", xlab =  NULL, pch = ".")
  title(ylab = "Mean annual flow log(CMS)", line = 3, cex.lab = 1)
  legend("bottomright", "B", bty = "n")
  
  boxplot(cov.air_temp_mean_cat ~ Site_type, data = Site_summary, col = the_colors, ylab = "", xlab =  NULL, pch = ".")
  title(ylab = "Mean annual temperature (°C)", line = 3, cex.lab = 1)
  legend("bottomleft", "C", bty = "n")
  
  boxplot(cov.precip_cat ~ Site_type, data = Site_summary, col = the_colors, ylab = "", xlab =  NULL, pch = ".")
  title(ylab = "Mean annual precipitation (mm)", line = 3, cex.lab = 1)
  legend("topleft", "D", bty = "n")
  
  boxplot(cov.area_km2_ws_log ~ Site_type, data = Site_summary, col = the_colors, ylab = "", xlab =  NULL, pch = ".")
  title(ylab = "Watershed area log(Km2)", line = 3, cex.lab = 1)
  legend("topleft", "E", bty = "n")
 
  boxplot(cov.elev_mean_smo ~ Site_type, data = Site_summary, col = the_colors, ylab = "", xlab =  NULL, pch = ".")
  title(ylab = "Elevation at reach (m)", line = 3, cex.lab = 1)
  legend("topleft", "F", bty = "n")
  
  boxplot(cov.elev_ws ~ Site_type, data = Site_summary, col = the_colors, ylab = "", xlab =  NULL, pch = ".")
  title(ylab = "Mean elevation in watershed (m)", line = 3, cex.lab = 1)
  legend("topleft", "G", bty = "n")
  
  boxplot(cov.BFI_cat ~ Site_type, data = Site_summary, col = the_colors, ylab = "", xlab =  NULL, pch = ".")
  title(ylab = "Baseflow index", line = 3, cex.lab = 1)
  legend("bottomright", "H", bty = "n")
  
dev.off()
