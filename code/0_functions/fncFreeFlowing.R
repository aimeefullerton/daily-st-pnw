# Function subsets data to free-flowing reaches using the value of pDA as a cutoff 
# where pDA = the proportion of drainage area above a reach that is below a large dam

fncFreeFlowing <- function(the_data, PDI = 0.25){
  
  # Get the reaches with no dam influence
  freeflowing <- the_data[is.na(the_data$cov.proportion_dam_influenced), ]

  # Get the reaches that are minimally influenced by dams based on a threshold
  dam_influenced <- the_data[!is.na(the_data$cov.proportion_dam_influenced), ]
  
  minimally_influenced <- subset(dam_influenced, cov.proportion_dam_influenced < PDI)

  # Combine
  freeflowing <- rbind(freeflowing, minimally_influenced)

  return(freeflowing)
}