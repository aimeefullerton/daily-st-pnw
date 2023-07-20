# Function to extract time elements from the lookup
# adds new columns to data for 'comid', 'tim.year', 'tim.doy', and 'tim.date'
fncExtractLookup <- function(the_data){
  out <- strsplit(the_data$lookup, "_")
  the_data$COMID <- as.numeric(do.call(rbind, out)[,1])
  the_data$tim.year <- as.numeric(do.call(rbind, out)[,2])
  the_data$tim.doy <- as.numeric(do.call(rbind, out)[,3])
  the_data$tim.date <- as.Date(paste(the_data$tim.doy, the_data$tim.year), format = "%j %Y")
  rm(out)
  return(the_data)
}