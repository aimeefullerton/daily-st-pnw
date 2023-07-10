# Function splits data into seasonal, cov.NorWeST_regional, or seasonal-cov.NorWeST_regional datasets for model fitting


fncSubsetBy <- function(the_data, split_by, julian_day_to_split = 210){
  
  # Subset to season, cov.NorWeST_region, or season-cov.NorWeST_region
  
  # Split into spring and fall subsets
  if(split_by == "season"){
    the_data_spring <- subset(the_data, tim.doy <= julian_day_to_split)
    the_data_fall <- subset(the_data, tim.doy > julian_day_to_split)
    
    the_list <- (list(spring = the_data_spring, fall = the_data_fall))
  }
  
  # Split into cov.NorWeST_regional (NorWeST processing unit) subsets
  if(split_by == "region") {
    cov.NorWeST_regions <- unique(the_data$cov.NorWeST_region)
    for(reg in cov.NorWeST_regions){
      dat.sub <- subset(the_data, cov.NorWeST_region == reg)
      assign(paste0("the_data_", reg), dat.sub)
    }
    
    the_list <- list()
    for(i in 1:length(cov.NorWeST_regions)) the_list[[i]] <- get(paste0("the_data_", cov.NorWeST_regions[i]))
    names(the_list) <- cov.NorWeST_regions
    
  }
  
  # Split into seasonal x cov.NorWeST_regional subsets
  if(split_by == "season-region"){
    cov.NorWeST_regions <- unique(the_data$cov.NorWeST_region)
    for(reg in cov.NorWeST_regions){
      dat.sub <- subset(the_data, cov.NorWeST_region == reg & tim.doy <= julian_day_to_split)
      assign(paste0("the_data_", reg, "_spring"), dat.sub)
      dat.sub <- subset(the_data, cov.NorWeST_region == reg & tim.doy > julian_day_to_split)
      assign(paste0("the_data_", reg, "_fall"), dat.sub)
    }
    
    the_list <- list()
    for(i in 1:length(cov.NorWeST_regions)) the_list[[i]] <- get(paste0("the_data_", cov.NorWeST_regions[i], "_spring"))
    for(i in 1:length(cov.NorWeST_regions)) the_list[[i + length(cov.NorWeST_regions)]] <- get(paste0("the_data_", cov.NorWeST_regions[i], "_fall"))
    names(the_list) <- c(paste0(cov.NorWeST_regions, "_spring"), paste0(cov.NorWeST_regions, "_fall"))
    
  }
  
  if(split_by != "full"){
    return(the_list)
  }

}
