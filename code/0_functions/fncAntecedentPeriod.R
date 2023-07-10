# Function predicts the optimal lag for the antecedent air temperature covariate

fncAntecedentPeriod <- function(the_data = freeflow_data){
  
  # Standardized flow quantiles
  flow_std_quantiles <- quantile(the_data$cov.std_mean_flow, seq(0, 1, 0.005), na.rm = T)
  
  lookup <- matrix(nrow = 200, ncol = 2)
  lookup[,1] <- flow_std_quantiles[-1]
  for(i in 1:length(flow_std_quantiles[-1])){
    lookup[i,2] <- mean(subset(the_data, cov.std_mean_flow  >= flow_std_quantiles[i] & 
                        cov.std_mean_flow < flow_std_quantiles[i + 1])$cov.std_mean_flow)
  }
  lookup <- as.data.frame(lookup)
  colnames(lookup) <- c("Upper_value_flowanom", "mean_value_flowanom")
  
  the_data$mean_stand_flow_categ <- NA
  for(i in 1:200){
    the_data$mean_stand_flow_categ <- ifelse(the_data$cov.std_mean_flow < lookup$Upper_value_flowanom[201 - i], 
                                            lookup$mean_value_flowanom[201 - i], the_data$mean_stand_flow_categ)
  }

  # Create container to collect correlation values between standardized flow categories and different air temperature lags
  corr_categs <- matrix(ncol = 20, nrow = nrow(subset(as.data.frame(table(the_data$mean_stand_flow_categ)), Freq > 49)))
  corr_categs[,1] <- as.character(subset(as.data.frame(table(the_data$mean_stand_flow_categ)), Freq > 49)$Var1) #Only categories with 50 or more points considered
  corr_categs <- as.data.frame(corr_categs)
  colnames(corr_categs) <- c("cov.std_mean_flow", "n_days","lag_choice", "cor_max",
                               "d1", "d3", "d6", "d9", "d12", "d15", "d18", "d21", "d24", "d27", "d30", "d35", "d40", "d50", "d60", "End")

  # Progress bar
  pb <- txtProgressBar(min = 1, max = length(corr_categs[,1]), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # Summarize
  for(i in 1:length(corr_categs[,1])){
    #print(i / length(corr_categs[,1]))
    setTxtProgressBar(pb, i) #initialize progress bar
    corr_categs[i, 5:19] <- cor(dplyr::select(subset(the_data, mean_stand_flow_categ == corr_categs[i,1]),
                                obs.stream_temp_daily_mean, cov.air_temp, cov.air_temp_3d, cov.air_temp_6d, cov.air_temp_9d, 
                                cov.air_temp_12d, cov.air_temp_15d, cov.air_temp_18d, cov.air_temp_21d, cov.air_temp_24d, 
                                cov.air_temp_27d, cov.air_temp_30d, cov.air_temp_35d, cov.air_temp_40d, cov.air_temp_50d, 
                                cov.air_temp_60d), use = "complete.obs")[1, 2:16]
    corr_categs[i, "n_days"] <- nrow(subset(the_data, mean_stand_flow_categ == corr_categs[i,1]))
    corr_categs[i, "cor_max"] <- max(as.numeric(corr_categs[i,5:19]))
    y <- as.numeric(ifelse(max(as.numeric(corr_categs[i,5:19])) == 1, NA, 
                           ifelse(is.na(max(as.numeric(corr_categs[i, 5:19]))), NA, 
                           subset(data.table::melt(data.table::setDT(corr_categs[i, 4:20]), 
                           id.vars = c("cor_max","End"), variable.name = "cor"), value == corr_categs[i, "cor_max"])[,3])))
    corr_categs[i, "lag_choice"] <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 35, 40, 50, 60)[y]
  }
  close(pb)
  
  # Make correlation category variables numerical
  corr_categs$lag_choice <- as.numeric(corr_categs$lag_choice)
  corr_categs$cov.std_mean_flow <- as.numeric(corr_categs$cov.std_mean_flow)
  corr_categs$cor_max <- as.numeric(corr_categs$cor_max)
  corr_categs$n_days <- as.numeric(corr_categs$n_days)

  # Model best lag (highest correlation)
  lag_model <- mgcv::gam(lag_choice ~ s(cov.std_mean_flow, k = 6), data = corr_categs)

  # Predict lags
  the_data$pred_lag <- predict(lag_model, newdata = the_data)
  
  # Round predicted lags
  rounded_lags <- matrix(ncol = 1, nrow = length(the_data$pred_lag))
  lags <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 35, 40, 50, 60)
  for(i in 1:length(the_data$pred_lag)){
    rounded_lags[i]  <- ifelse(is.na(the_data$pred_lag[i]), NA,
           lags[which(abs(lags - the_data$pred_lag[i]) == min(abs(lags - the_data$pred_lag[i])))])
  }
  the_data$pred_lag_rounded <- rounded_lags
  
  return(list(the_data$pred_lag_rounded, lag_model, corr_categs))
  
}


