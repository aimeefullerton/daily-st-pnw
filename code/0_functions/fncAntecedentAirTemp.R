# Function predicts antecedent air temperature 
# by choosing the air temperature metric associated with the optimal lag duration

fncAntecedentAirTemp <- function(the_data){

the_data$cov.antec_air_temp <- 
  ifelse(the_data$pred_lag == 1, the_data$cov.air_temp,
         ifelse(the_data$pred_lag == 3, the_data$cov.air_temp_3d,
                ifelse(the_data$pred_lag == 6, the_data$cov.air_temp_6d,
                       ifelse(the_data$pred_lag == 9, the_data$cov.air_temp_9d,
                              ifelse(the_data$pred_lag == 12, the_data$cov.air_temp_12d,
                                     ifelse(the_data$pred_lag == 15, the_data$cov.air_temp_15d,
                                            ifelse(the_data$pred_lag == 18, the_data$cov.air_temp_18d,
                                                   ifelse(the_data$pred_lag == 21, the_data$cov.air_temp_21d,
                                                          ifelse(the_data$pred_lag == 24, the_data$cov.air_temp_24d,
                                                                 ifelse(the_data$pred_lag == 27, the_data$cov.air_temp_27d,
                                                                        ifelse(the_data$pred_lag == 30, the_data$cov.air_temp_30d,
                                                                               ifelse(the_data$pred_lag == 35, the_data$cov.air_temp_35d,
                                                                                      ifelse(the_data$pred_lag == 40, the_data$cov.air_temp_40d,
                                                                                             ifelse(the_data$pred_lag == 50, the_data$cov.air_temp_50d,
                                                                                                    ifelse(the_data$pred_lag == 60, the_data$cov.air_temp_60d,
                                                                                                           ifelse(the_data$pred_lag == 80, the_data$cov.air_temp_80d,
                                                                                                                  NA))))))))))))))))

return(the_data)
}
