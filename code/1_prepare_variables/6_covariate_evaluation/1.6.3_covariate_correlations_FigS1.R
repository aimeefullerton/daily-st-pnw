#PEARSON'S CORRELATION TEST ON STANDARDIZED DATA

# SETUP ----
# Libraries
library(GGally) #for nice correlation plots
library(cowplot)

# Local directories
plot.dir <- "plots"

# Load data produced in covariate ordination plot
fit.df <- fst::read_fst("data/freeflow_data.fst", as.data.table = T)

scols <- c("COMID", "cov.lat_v", "cov.elev_mean_smo", "cov.area_km2_ws_log", "cov.BFI_cat", "cov.elev_diff",
           "cov.slope", "cov.pct_wet_all_ws", "cov.pct_ice_ws", "cov.pct_for_all_cat_rip100m", "cov.canopy_line", 
           "cov.pct_urb_all_ws", "cov.precip_cat", "cov.air_temp_range_cat", "cov.pct_extru_vol_ws")
spat.df <- fit.df[,..scols]
spat.std <- vegan::decostand(spat.df[,2:ncol(spat.df)], method = "rrank", MARGIN = 2, na.rm = T)
spat.df <- cbind.data.frame("COMID" = spat.df$COMID, spat.std); rm(spat.std)

tcols <- c("COMID", "cov.antec_air_temp", "cov.air_temp_ws", "cov.SWE_ws", "cov.daylength_hours", "cov.std_mean_flow", "cov.SWE_1Apr", "tim.doy")
temp.df <- fit.df[,..tcols]
temp.std <- vegan::decostand(temp.df[,2:ncol(temp.df)], method = "rrank", MARGIN = 2, na.rm = T)
temp.df <- cbind.data.frame("COMID" = temp.df$COMID, temp.std); rm(temp.std)

ycols <- c("tim.year", "tim.doy")
times.df <- fit.df[,..ycols]
times.df$tim.date <- as.Date(times.df$tim.doy, origin = as.Date(paste0(times.df$tim.year, "-01-01")))
times.df$tim.month <- lubridate::month(times.df$tim.date)
times.df$tim.season <- NA
times.df$tim.season[times.df$tim.month %in% 1:3] <- "winter"
times.df$tim.season[times.df$tim.month %in% 4:6] <- "spring"
times.df$tim.season[times.df$tim.month %in% 7:9] <- "summer"
times.df$tim.season[times.df$tim.month %in% 10:12] <- "fall"
summary(as.factor(times.df$tim.season))

dat.std <- cbind.data.frame("COMID" = temp.df$COMID, times.df, temp.df[,tcols[-1]], spat.df)
dat.std <- unique(dat.std); dat.std <- dat.std[,-14]

# Rename mat.std colnames for plotting
colnames(dat.std) <- c("COMID", "year", "doy", "date", "month", "season",
                      "Tl","Tws","Sws","DL","Qr","SA1","D","Lat","E","A","BFI","Ed","S","W","I","F","C","U","P","R","V")

thecols <- c("Tl","Tws","Sws","DL","Qr","SA1","D","Lat","E","A","BFI","Ed","S","W","I","F","C","U","P","R","V")

# Correlation plots showing values ----
g1 <- list(
cor.sp <- ggcorr(subset(dat.std, season == "spring")[,thecols], method = c("pairwise", "pearson"), label_size = 3, label = T, label_alpha = T) +
  ggtitle("Spring") + theme(plot.title = element_text(size = 22)),
cor.su <- ggcorr(subset(dat.std, season == "summer")[,thecols], method = c("pairwise", "pearson"), label_size = 3, label = T, label_alpha = T) +
  ggtitle("Summer") + theme(plot.title = element_text(size = 22)),
cor.f  <- ggcorr(subset(dat.std, season == "fall")[,thecols],   method = c("pairwise", "pearson"), label_size = 3, label = T, label_alpha = T) +
  ggtitle("Fall") + theme(plot.title = element_text(size = 22)),
cor.w  <- ggcorr(subset(dat.std, season == "winter")[,thecols], method = c("pairwise", "pearson"), label_size = 3, label = T, label_alpha = T) +
  ggtitle("Winter") + theme(plot.title = element_text(size = 22))
)
#legend.position  = "none"

png(height = 12, width = 12, units = "in", res = 400, file = paste0(plot.dir, "/FigS1_correl_values_", mod_subset_name, ".png"))
grid.arrange(
  grobs = g1,
  widths = c(3, 3),
  layout_matrix = rbind(c(1, 2), c(3, 4))
)
dev.off()

cor.all <- ggcorr(subset(dat.std)[,thecols], method = c("pairwise", "pearson"), label_size = 4, label = T, label_alpha = T) + ggtitle("Year-round")
png(paste0(plot.dir, "/correl_values_year-round.png"), width = 9.5, height = 9.5, units = "in", res = 300)
plot(cor.all)
dev.off()
