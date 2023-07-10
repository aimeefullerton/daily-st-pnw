# Function to create a lookup column based on reach id and date

fncCreateLookup <- function(the_data){

lookup <- paste0(the_data$COMID, "_", the_data$tim.year, "_", as.numeric(the_data$tim.doy)) #strip extra 0 at start of doy

return(lookup)

}
#usage: SWE_data$lookup <- fncCreateLookup(the_data = SWE_data[, c("COMID", "tim.year", "tim.doy")])
