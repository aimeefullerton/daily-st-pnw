# Produce PCAs of covariate space for spatial and temporal variables used to fit stream temperature models
# note: the temporal part takes a long time to run!

# SETUP ----
library(vegan)
library(dplyr)
library(tidyr)

options (future.globals.maxSize = 4000 * 1024^5)

plot.dir <- "plots"

t_col <- function(color, percent = 50, name = NULL) {
  # color = color name
  # percent = % transparency
  # name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3], max = 255, alpha = (100 - percent) * 255 / 100, names = name)
  
  ## Save the color
  return(t.col)
} # to add transparency so we can see overlap of points


# Load data
the_data <- fst::read_fst("data/freeflow_data_full.fst", as.data.table = T) #only sites for fitting, excluding below dams

# Prepare data
covars <- c("cov.daylength_hours", "cov.NWM_flow_log", "cov.air_temp_ws", "cov.SWE_ws", 
            "cov.SWE_1Apr", "tim.doy", "cov.SWE_mean_year",
            "cov.lat_v", "cov.elev_mean_smo", "cov.area_km2_ws_log", "cov.BFI_cat", "cov.elev_diff", "cov.slope", 
            "cov.pct_ice_ws", "cov.pct_for_all_cat_rip100m", "cov.canopy_line", "cov.pct_urb_all_ws", "cov.pct_wet_all_ws",
            "cov.precip_cat", "cov.air_temp_range_cat", "cov.pct_extru_vol_ws") #"cov.antec_air_temp"

grouping_vars <- c("COMID", "tim.month", "tim.year") #tim.doy is in covars above

cov.region.f <- as.numeric(as.factor(the_data$cov.NorWeST_region))

tempvars <- c(covars[c(1:7)], "tim.year", "lookup")
spatvars <- covars[c(8:21)]

comids.emp <- unique(the_data$COMID[!is.na(the_data$obs.stream_temp_daily_mean)])

rm(the_data)


# SPATIAL ORDINATION ----
# How representative empirical sites are of prediction sites for spatial variables (i.e. covariates that only vary across space and not time)

spatial_data <- data.table::fread("data/spatial_data.csv") #prediction sites, physical variables only

mat <- as.matrix(cbind("COMID" = spatial_data$COMID, spatial_data[,..spatvars]))
# remove NAs for PCA 
mat <- mat[!is.na(mat[,"cov.BFI_cat"]),]
mat <- mat[!is.na(mat[,"cov.pct_for_all_cat_rip100m"]),]
mat <- mat[!is.na(mat[,"cov.pct_extru_vol_ws"]),]

## Standardize covariates
raw <- mat[,2:ncol(mat)]; row.names(raw)<- mat[,"COMID"]
mat.std <- vegan::decostand(raw, method = "rrank", MARGIN = 2, na.rm = T)
summary(mat.std)

# spatial dataset to use in PCA
spat.df <- cbind.data.frame("COMID" = rownames(mat.std), mat.std)

# Label by COMID
comids.all <- unique(spat.df$COMID)
emp <- which(spat.df$COMID %in% comids.emp)
spat.df$emp <- 0
spat.df$emp[emp] <- 1

# PCA 
pca.s <- prcomp(spat.df[,2:(ncol(spat.df) - 1)])
summary(pca.s)

#Importance of components:
#                       PC1    PC2    PC3     PC4     PC5     PC6     PC7     PC8     PC9    PC10    PC11    PC12    PC13
#Standard deviation     0.5680 0.5100 0.4081 0.2900 0.2674 0.22915 0.19771 0.18193 0.16376 0.16042 0.14240 0.12516 0.11185 0.08758
#Proportion of Variance 0.2835 0.2285 0.1463 0.0739 0.0628 0.04613 0.03434 0.02908 0.02356 0.02261 0.01782 0.01376 0.01099 0.00674
#Cumulative Proportion  0.2835 0.5120 0.6583 0.7322 0.7950 0.84110 0.87545 0.90453 0.92809 0.95069 0.96851 0.98227 0.99326 1.00000

eig <- pca.s$sdev^2	#to get eigenvalues, square the Standard Deviations
trace <- sum(eig)
prop.var <- eig/trace
par(mfrow=c(1,1)); screeplot(pca.s, bstick = TRUE)	#plots eigenvalues; PCs with bars overlapping line should be retained

(axis.loadings <- pca.s$rotation[,1:3])
#                           PC1          PC2         PC3
#cov.lat_v                   -0.29500668 -0.09667585  0.07234097
#cov.elev_mean_smo            0.17847649  0.41187027  0.21713324
#cov.area_km2_ws_log          0.10467673 -0.35428204  0.42861654
#cov.BFI_cat                  0.11600072  0.26607564  0.37549273
#cov.elev_diff               -0.06570819 -0.13393536  0.56216941
#cov.slope                   -0.19895898  0.41388244 -0.13159363
#cov.pct_ice_ws              -0.02685839 -0.05078195  0.14263389
#cov.pct_for_all_cat_rip100m -0.44784762  0.22931005  0.24089843
#cov.canopy_line             -0.47736504 -0.06094189  0.15673010
#cov.pct_urb_all_ws          -0.08531503 -0.44082533 -0.30820794
#cov.pct_wet_all_ws           0.04130387 -0.41704172  0.21838926
#cov.precip_cat              -0.44215070  0.01417159  0.06029390
#cov.air_temp_range_cat       0.42296155  0.08583366  0.19377407
#cov.pct_extru_vol_ws         0.01602986 -0.02779313  0.05245117


# Color by empirical data or not
colrs <- c("gray", "magenta")
colrs.f <- as.numeric(as.factor(spat.df$emp))
t.col <- NULL; for(i in 1:length(colrs)){ t.col <- c(t.col, t_col(colrs[i], percent = 50))}
leglabs <- c("prediction sites", "fitting sites")
axis_a <- 1
axis_b <- 2
plotnam <- paste0("Fig6_pca_", axis_a, "_", axis_b, "_spatial")

# Sub out empirical only for clearer graphing
pca.s_emp <- cbind.data.frame(pca.s$x[,axis_a], pca.s$x[,axis_b])
colnames(pca.s_emp) <- c(paste0("PCA_", axis_a), paste0("PCA_", axis_b))
pca.s_emp <- pca.s_emp[rownames(pca.s_emp) %in% comids.emp, ]

# Create plot
png(paste0(plot.dir, "/", plotnam, ".png"), width = 6, height = 6, units = "in", res = 300)
par(las = 1, mar = c(4.5, 4.5, 1, 1))
plot(pca.s$x[,axis_a], pca.s$x[,axis_b], col = t.col[colrs.f], cex = 0.2, pch = 19,
     xlab = paste0("PC ", axis_a, " (",round(prop.var[axis_a]*100,1),"%)"), cex.lab = 1.5, cex.axis = 1.3, 
     ylab = paste0("PC ", axis_b, " (",round(prop.var[axis_b]*100,1),"%)"), cex.lab = 1.5, cex.axis = 1.3,
     xlim = c(min(pca.s$x[,1]) - 0.25, max(pca.s$x[,1]))) #space for legend
lines(pca.s_emp$PCA_1, pca.s_emp$PCA_2, col = "magenta", type  = "p", cex = 0.3, pch = 19)
legend("topleft", legend = leglabs, col = t.col, pch = 19, bty = 'n', cex = 1.3)
dev.off()

# Save standardized output
data.table::fwrite(spat.df, "data/spat_std.csv")


# TEMPORAL ORDINATION ----
# How representative empirical sites are of prediction sites for temporal variables (i.e. covariates that vary over time)

hucfiles <- dir("data/huc")
idx <- sample(1:length(hucfiles), size = round(0.5 * length(hucfiles)), replace = F)
set.seed(123); rsampl <- hucfiles[idx]

alldat <- NULL
for(f in 1:length(rsampl)){
  print(f/length(rsampl))
  hucdat <- fst::read_fst(paste0("data/huc/", rsampl[f]), as.data.table = T) #prediction sites, temporal variables only
  idx <- sample(1:nrow(hucdat), size = round(0.2 * nrow(hucdat)), replace = F)
  hucdat <- hucdat[idx, ..tempvars]
  alldat <- rbind(alldat, hucdat)
}
fst::write_fst(alldat, "data/temporal_covariates.fst", compress = 80)
rm(hucdat)

sub.df <- as.data.frame(alldat)

# Add date/time columns for aggregating
sub.df$tim.date <- as.Date(sub.df$tim.doy, origin = as.Date(paste0(sub.df$tim.year, "-01-01")))
sub.df$tim.month <- lubridate::month(sub.df$tim.date)
tempvars <- c(tempvars, "tim.month")

## Standardize variables
thecols <- colnames(sub.df)[1:6]
mat <- as.matrix(sub.df[,thecols])
mat.std <- vegan::decostand(mat, method = "rrank", MARGIN = 2, na.rm = T)
head(mat.std)

temp.df <- cbind.data.frame("lookup" = sub.df$lookup, "year" = sub.df$tim.year, "month" = sub.df$tim.month, 
                           "doy" = sub.df$tim.doy, "cov.SWE_mean_year" = sub.df$cov.SWE_mean_year, mat.std)

## Subset to just the reaches where we have no NAs
temp.df <- temp.df[!is.na(temp.df[,"cov.SWE_ws"]),]
temp.df <- temp.df[!is.na(temp.df[,"cov.NWM_flow_log"]),]
temp.df <- temp.df[!is.na(temp.df[,"cov.air_temp_ws"]),]
temp.df <- temp.df[!is.na(temp.df[,"cov.SWE_1Apr"]),]
temp.df <- temp.df[!is.na(temp.df[,"cov.daylength_hours"]),]
#summary(temp.df)

# Label empirical data by COMID
temp.df$COMID <- gsub("_.*", "", temp.df$lookup)

# Aggregate so we can run the PCA on reach-month combinations
mat.agg <- dplyr::summarise(group_by(temp.df, COMID, month),
                     cov.air_temp_ws = mean(cov.air_temp_ws),
                     cov.SWE_ws = mean(cov.SWE_ws),
                     cov.daylength_hours = mean(cov.daylength_hours), 
                     cov.NWM_flow_log = mean(cov.NWM_flow_log), 
                     cov.SWE_1Apr = mean(cov.SWE_1Apr), 
                     tim.doy = mean(tim.doy))

# Save standardized output
data.table::fwrite(mat.agg, "data/temp_std.csv")
rm(alldat, mat.std, temp.df, cov.region.f, idx)

# PCA 
pca.t <- prcomp(mat.agg[,3:8])
summary(pca.t)

eig <- pca.t$sdev^2	#to get eigenvalues, need to square the Standard Deviations
trace <- sum(eig)
prop.var <- eig/trace
par(mfrow = c(1,1)); screeplot(pca.t, bstick = TRUE)	#plots eigenvalues; PCs with bars overlapping line should be retained

(axis.loadings <- pca.t$rotation[,1:3])

axis_a <- 1
axis_b <- 2

# Sub out empirical only for clearer graphing
pca.t_emp <- cbind.data.frame(pca.t$x[,axis_a], pca.t$x[,axis_b], mat.agg$COMID)
colnames(pca.t_emp) <- c(paste0("PCA_", axis_a), paste0("PCA_", axis_b), "COMID")
mat.agg$emp <- 0
mat.agg$emp[mat.agg$COMID %in% comids.emp] <- 1
pca.t_emp <- pca.t_emp[pca.t_emp$COMID %in% comids.emp,]

# Color by empirical data or not
colrs <- c("gray", "magenta")
colrs.f <- as.numeric(as.factor(mat.agg$emp))
t.col <- NULL; for(i in 1:length(colrs)){ t.col <- c(t.col, t_col(colrs[i], percent = 50))}
leglabs <- c("prediction sites", "fitting sites")
plotnam <- paste0("Fig6_pca_", axis_a, "_", axis_b, "_temporal")

# Create plot
png(paste0(plot.dir, "/", plotnam, ".png"), width = 6, height = 6, units = "in", res = 300)
par(las = 1, mar = c(4.5, 4.5, 1, 1))
plot(pca.t$x[,axis_a], pca.t$x[,axis_b], col = t.col[colrs.f], cex = 0.2, pch = 19,
     xlab = paste0("PC ", axis_a, " (",round(prop.var[axis_a]*100,1),"%)"), cex.lab = 1.5, cex.axis = 1.3, 
     ylab = paste0("PC ", axis_b, " (",round(prop.var[axis_b]*100,1),"%)"), cex.lab = 1.5, cex.axis = 1.3,
     xlim = c(min(pca.t$x[,1]) - 0.25, max(pca.t$x[,1]))) #space for legend
lines(pca.t_emp$PCA_1, pca.t_emp$PCA_2, col = "magenta", type  = "p", cex = 0.3, pch = 19)
legend("topleft", legend = leglabs, col = t.col, pch = 19, bty = 'n', cex = 1.3)
dev.off()
