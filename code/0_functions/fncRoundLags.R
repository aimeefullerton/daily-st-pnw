# Round predicted lags to hearest level
fncRoundLags <- function(the_data){
  rounded_lags <- matrix(ncol = 1, nrow = length(the_data$pred_lag))
  lags <- c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 35, 40, 50, 60)
  for(i in 1:length(the_data$pred_lag)){
    rounded_lags[i]  <- ifelse(is.na(the_data$pred_lag[i]), NA,
                               lags[which(abs(lags - the_data$pred_lag[i]) == min(abs(lags - the_data$pred_lag[i])))])
  }
  return(rounded_lags)
}