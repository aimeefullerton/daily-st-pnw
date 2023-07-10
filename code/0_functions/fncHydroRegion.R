# Function separates dataset into 3 subsets by hydroregion
# where hydroregion types are rain-dominated, transitional, or snow-dominated based on snowpack depth

# swe1 = depth of snowpack in mm to delineate rain-dominated hydrograph
# swe2 = depth of snowpack in mm to delineate snow-dominated hydrograph

fncHydroRegion <- function(the_data, swe1 = 20, swe2 = 100){
  
  rain <- subset(the_data, cov.SWE_mean_year < swe1)
  trans <- subset(the_data, cov.SWE_mean_year > swe1 & cov.SWE_mean_year < swe2)
  snow <- subset(the_data, cov.SWE_mean_year > swe2)

  return(list("rain" = rain, "trans" = trans, "snow" = snow))
}
