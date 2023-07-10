# Combine all the raw NorWeST data in hydroregion 17 into a single flat file

# Crowd-sourced raw data can be obtained and downloaded here: 
  # https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/StreamTemperatureDataSummaries.shtml

  # Chandler, G.L., Wollrab, S.P., Horan, D. L., Nagel, D. E., Parkes, S.L., Isaak, D.J., Wenger, S.J.,
    # Peterson, E.E., Ver Hoef, J.M., Hostetler, S.W., Luce, C.H., Dunham, J.B., Kershner, J.L., Roper, B.B. 2016. 
    # NorWeST stream temperature data summaries for the western U.S. Fort Collins, CO: Forest Service Research Data Archive. 
    # https://doi.org/10.2737/RDS-2016-0032.


# SETUP ----
# Load packages
library(dplyr)
library(data.table)
library(tidyverse)
library(sf)
library(readxl)

# LOAD DATA ----
# note that each dataset may have some unique differences (.txt vs .xlsx file format, different headers, etc.)

# Oregon coast
OrCoast <- read.delim("data/response/NorWeST_ObservedStreamTempDailySummaries_OregonCoast_AllDays.txt", header = T, sep = ",", dec = ".")
# This data file does not have a COMID field!!!
OrCoastPoints<- sf::st_read("data/response", "NorWest_ObservedTempPoints_OregonCoast", type = 7)
OrCoastPoints$NorWeST_ID <- paste0("OregonCoast_", OrCoastPoints$OBSPRED_ID)
OrCoast <- merge(OrCoast, dplyr::select(OrCoastPoints, NorWeST_ID, COMID, PERMA_FID, GNIS_NAME), by = "NorWeST_ID", all.x = T)
OrCoast$SampleDate <- as.POSIXct(OrCoast$SampleDate, format = "%m/%d/%Y", origin = "1970-01-01")
length(OrCoast[is.na(OrCoast$COMID),]$COMID ) #9619 days of unconnected data due to no COMID field
# loading from a previous version where we got COMIDs from GIS
oldv <- data.table::fread("data/response/NorWestAll.csv")
oldv$NorWeST_region <- gsub("_.*", "", oldv$NorWeST_ID)
OrCoast <- oldv[oldv$region == "OregonCoast", c("OBSPRED_ID", "NorWeST_ID", "SampleDate", "DailyMax", "DailyMin", "DailyMean", "COMID", "Nobs")]

# Mid Columbia
MidColumbia <- read.delim("data/response/NorWeST_ObservedStreamTempDailySummaries_MidColumbia_AllDays.txt", header = T, sep = ",", dec = ".")
MidColumbiaPoints<- sf::st_read("data/response", "NorWest_ObservedTempPoints_MidColumbia", type = 7)
MidColumbia$NorWeST_ID <- paste0("MiddleColumbia_", MidColumbia$OBSPRED_ID)
MidColumbiaPoints$NorWeST_ID <- paste0("MiddleColumbia_",MidColumbiaPoints$OBSPRED_ID)
MidColumbia <- merge(MidColumbia, dplyr::select(MidColumbiaPoints, NorWeST_ID, COMID, PERMA_FID, GNIS_NAME), by = "NorWeST_ID", all.x = T)
MidColumbia$SampleDate <- as.POSIXct(MidColumbia$SampleDate, format = "%m/%d/%Y", origin = "1970-01-01")
length(MidColumbia[is.na(MidColumbia$COMID),]$COMID) #0

# Upper Columbia River/Yakima
UCYak <- readxl::read_xlsx("data/response/NorWeST_ObservedStreamTemp_TempDailySummaries_UpperColumbiaYakima_AllDays.xlsx")
UCYakPoints<- sf::st_read("data/response", "NorWest_ObservedTempPoints_UpperColumbiaYakima", type = 7)
UCYakPoints$NorWeST_ID <- paste0("UpperColumbia-Yakima_", UCYakPoints$OBSPRED_ID)
UCYak <- merge(UCYak, dplyr::select(UCYakPoints, NorWeST_ID, COMID, PERMA_FID, GNIS_NAME), by = "NorWeST_ID", all.x = T)
length(UCYak[is.na(UCYak$COMID),]$COMID) #0

# Washington Coast
WaCoast <- readxl::read_xlsx("data/response/NorWeST_ObservedStreamTemp_TempDailySummaries_WashingtonCoast_AllDays.xlsx")
WaCoastPoints<- sf::st_read("data/response", "NorWest_ObservedTempPoints_WACoast", type = 7)
WaCoastPoints$NorWeST_ID <- paste0("WashingtonCoast_", WaCoastPoints$OBSPRED_ID)
WaCoast <- merge(WaCoast, dplyr::select(WaCoastPoints, NorWeST_ID, COMID, PERMA_FID, GNIS_NAME), by = "NorWeST_ID", all.x = T)
length(WaCoast[is.na(WaCoast$COMID),]$COMID) #0

# Clearwater
Clear <- readxl::read_xlsx("data/response/NorWeST_ObservedStreamTempDailySummaries_Clearwater_AllDays.xlsx")
ClearPoints<- sf::st_read("data/response", "NorWest_ObservedTempPoints_Clearwater", type = 7)
ClearPoints$NorWeST_ID <- paste0("Clearwater_",ClearPoints$OBSPRED_ID)
Clear$NorWeST_ID <- paste0("Clearwater_",Clear$OBSPRED_ID)
Clear <- merge(Clear, dplyr::select(ClearPoints, NorWeST_ID, COMID, PERMA_FID, GNIS_NAME), by = "NorWeST_ID", all.x = T)
length(Clear[is.na(Clear$COMID),]$COMID) #0

# South-central Oregon
SouthCentOre <- readxl::read_xlsx("data/response/NorWeST_ObservedStreamTempDailySummaries_ORSouthCentral_AllDays.xlsx")
SouthCentOrePoints<- sf::st_read("data/response", "NorWest_ObservedTempPoints_SouthCentralOregon", type = 7)
SouthCentOrePoints$NorWeST_ID <- paste0("South-Central Oregon_", SouthCentOrePoints$OBSPRED_ID)
SouthCentOre <- merge(SouthCentOre, dplyr::select(SouthCentOrePoints, NorWeST_ID, COMID, PERMA_FID, GNIS_NAME), by = "NorWeST_ID", all.x = T)
length(SouthCentOre[is.na(SouthCentOre$COMID),]$COMID) #0

# Salmon River
Salmon <- readxl::read_xlsx("data/response/NorWeST_ObservedStreamTempDailySummaries_SalmonRiverBasin_AllDays.xlsx")
SalmonPoints<- sf::st_read("data/response", "NorWest_ObservedTempPoints_Salmon", type = 7)
Salmon$NorWeST_ID <- paste0("Salmon_",Salmon$OBSPRED_ID)
SalmonPoints$NorWeST_ID <- paste0("Salmon_", SalmonPoints$OBSPRED_ID)
Salmon <- merge(Salmon, dplyr::select(SalmonPoints, NorWeST_ID, COMID, PERMA_FID, GNIS_NAME), by = "NorWeST_ID", all.x = T)
length(Salmon[is.na(Salmon$COMID),]$COMID) #0

# Snake/Bear
SnakeBear <- readxl::read_xlsx("data/response/NorWeST_ObservedStreamTempDailySummaries_SnakeBear_AllDays.xlsx")
SnakeBearPoints<- sf::st_read("data/response", "NorWest_ObservedTempPoints_SnakeBear", type = 7)
SnakeBearPoints$NorWeST_ID <- paste0("Snake-Bear_", SnakeBearPoints$OBSPRED_ID)
SnakeBear <- merge(SnakeBear, dplyr::select(SnakeBearPoints, NorWeST_ID, COMID, PERMA_FID, GNIS_NAME), by = "NorWeST_ID", all.x = T)
length(SnakeBear[is.na(SnakeBear$COMID),]$COMID) #0

# SpoKoot
SpoKoot <- readxl::read_xlsx("data/response/NorWeST_ObservedStreamTempDailySummaries_Spokoot_AllDays.xlsx")
SpoKootPoints<- sf::st_read("data/response", "NorWest_ObservedTempPoints_Spokoot", type = 7)
SpoKootPoints$NorWeST_ID <- paste0("SpoKoot_", SpoKootPoints$OBSPRED_ID)
SpoKoot <- merge(SpoKoot, dplyr::select(SpoKootPoints, NorWeST_ID, COMID, PERMA_FID, GNIS_NAME), by = "NorWeST_ID", all.x = T)
length(SpoKoot[is.na(SpoKoot$COMID),]$COMID) #0

# Mid-Snake
MidSnake <- readxl::read_xlsx("data/response/NorWeST_ObservedStreamTempDailySummaries_MidSnake_AllDays.xlsx")
MidSnakePoints<- sf::st_read("data/response", "NorWest_ObservedTempPoints_MiddleSnake", type = 7)
MidSnakePoints$NorWeST_ID <- paste0("MiddleSnake_", MidSnakePoints$OBSPRED_ID)
MidSnake <- MidSnake %>% rename(NorWeST_ID = NoRWeST_ID)
MidSnake$NorWeST_ID <- gsub("MidSnake", "MiddleSnake", MidSnake$NorWeST_ID)
MidSnake <- merge(MidSnake, dplyr::select(MidSnakePoints, NorWeST_ID, COMID, PERMA_FID, GNIS_NAME), by = "NorWeST_ID", all.x = T)
length(MidSnake[is.na(MidSnake$COMID),]$COMID) #0
MidSnake <- MidSnake[, c(colnames(WaCoast))]


# COMBINE INTO ONE DATASET ----
cols2keep <- c("OBSPRED_ID", "NorWeST_ID", "SampleDate", "DailyMax", "DailyMin", "DailyMean", "Nobs", "COMID")
NorWeST_obs <- rbind(Salmon[,cols2keep], Clear[,cols2keep], UCYak[,cols2keep], WaCoast[,cols2keep], SouthCentOre[,cols2keep], SnakeBear[,cols2keep], 
                SpoKoot[,cols2keep], MidSnake[,cols2keep], MidColumbia[,cols2keep], OrCoast[,cols2keep])

NorWeST_obs$NorWeST_region <- gsub("_.*", "", NorWeST_obs$NorWeST_ID)
NorWeST_obs$SampleDate <- as.Date(NorWeST_obs$SampleDate, format = "%m/%d/%Y")
NorWeST_obs$doy <- format(as.Date(NorWeST_obs$SampleDate, format = "%m/%d/%Y"), format = "%j")
NorWeST_obs$year <- format(as.Date(NorWeST_obs$SampleDate, format = "%m/%d/%Y"), format = "%Y")
NorWeST_obs$month <- format(as.Date(NorWeST_obs$SampleDate, format = "%m/%d/%Y"), format = "%m")

# Plot to evaluate
plot(DailyMean ~ doy, NorWeST_obs, pch = ".")
length(unique(NorWeST_obs$NorWeST_ID)); nrow(NorWeST_obs); nrow(NorWeST_obs)/length(unique(NorWeST_obs$NorWeST_ID))
# 29,497 sites, 3,580,362 days of data, average of 121 days per site OLD
# 43,734 sites, 6,670,346 days of data, average of 152.5 days per site NEW, re-downloaded data on 5/12/23
# date range: 1/1/1993 to 12/31/2013
# 432 NA for DailyMean; 9619 NA for COMID

# Change F measurements into C measurments, No measurements over 32 C
foo <- NorWeST_obs[NorWeST_obs$DailyMean > 32,] # a handful in Middle Columbia
NorWeST_obs$DailyMean <- ifelse(NorWeST_obs$DailyMean > 32, (NorWeST_obs$DailyMean - 32) / (9 / 5), NorWeST_obs$DailyMean)
NorWeST_obs$DailyMin <- ifelse(NorWeST_obs$DailyMin > 32, (NorWeST_obs$DailyMin - 32) / (9 / 5), NorWeST_obs$DailyMin)
NorWeST_obs$DailyMax <- ifelse(NorWeST_obs$DailyMax > 32, (NorWeST_obs$DailyMax - 32) / (9 / 5), NorWeST_obs$DailyMax)
plot(DailyMean ~ doy, NorWeST_obs, pch=".")


# CROSSWALK NHD VERSIONS ----
   # We need NHDPlus version 2 COMID as reach identifiers
   # NorWest used NHD version 1 COMIDs for Hydroregion 17 but other regions use version 2 already
   # NorWest_COMID_Xreference.txt is a lookup table
   # with the NorWest points spatially related to NHD version 2 catchments
NW_COMID_Xreference <- read.delim("data/response/NorWest_COMID_Xreference.txt", header = T, sep = ",", dec = ".")
NW_COMID_Xreference$FEATUREID <- as.factor(NW_COMID_Xreference$FEATUREID)
summary(NW_COMID_Xreference$FEATUREID) #917 not matching with 0 value, these are outside the region 17 boundary
sum(is.na(NW_COMID_Xreference$FEATUREID)) #no NAs
NW_COMID_Xreference <- NW_COMID_Xreference %>% mutate("CorrectOrNot" = ifelse(COMID == FEATUREID, "Correct","Incorrect"))
NW_COMID_Xreference$CorrectOrNot <- as.factor(NW_COMID_Xreference$CorrectOrNot)
summary(NW_COMID_Xreference$CorrectOrNot) #2249 out of ~44k incorrect
NW_COMID_Xreference <- rename(NW_COMID_Xreference, c(COMID_v1 = COMID, COMID = FEATUREID)) #rename to update COMIDS
NW_COMID_Xreference <- select(NW_COMID_Xreference, COMID_v1, COMID, AreaSqKM, CorrectOrNot)

# Remove duplicates
length(unique(NW_COMID_Xreference$COMID_v1))
NW_COMID_Xreference <- NW_COMID_Xreference[!duplicated(NW_COMID_Xreference$COMID_v1),]
length(NW_COMID_Xreference$COMID_v1) #11120 unique COMIDS

# Add lookup
NorWeST_obs <- rename(NorWeST_obs, c(COMID_v1 = COMID)) #rename Norwest COMID column to old version
NorWeST_obs <- merge(NorWeST_obs, NW_COMID_Xreference, by = "COMID_v1", all.x = T)
NorWeST_obs$lookup <- paste0(NorWeST_obs$COMID, "_", NorWeST_obs$year, "_", as.numeric(NorWeST_obs$doy))

NorWeST_obs_noCOMID  <- subset(NorWeST_obs, COMID == 0)
unique(subset(NorWeST_obs, COMID == 0)$NorWeST_ID) #all Snake Bear and south central Oregon, out of region 17
rm(NorWeST_obs_noCOMID) # remove points not in in region 17

NorWeST_obs  <- subset(NorWeST_obs, COMID != 0)
nrow(NorWeST_obs) #5,304,867

# RENAME SOME VARIABLES ----
NorWeST_obs <- rename(NorWeST_obs, c(obs.stream_temp_daily_min = DailyMin)) 
NorWeST_obs <- rename(NorWeST_obs, c(obs.stream_temp_daily_mean = DailyMean)) 
NorWeST_obs <- rename(NorWeST_obs, c(obs.stream_temp_daily_max = DailyMax)) 
NorWeST_obs <- rename(NorWeST_obs, c(tim.date = SampleDate)) 
NorWeST_obs <- rename(NorWeST_obs, c(tim.doy = doy)) 
NorWeST_obs <- rename(NorWeST_obs, c(tim.year = year)) 
NorWeST_obs <- rename(NorWeST_obs, c(tim.month = month))
NorWeST_obs <- rename(NorWeST_obs, c(cov.NorWeST_region = NorWeST_region)) 
NorWeST_obs <- NorWeST_obs[,-15] # remove "CorrectOrNot" column

# Change some data types
NorWeST_obs$tim.doy <- as.numeric(NorWeST_obs$tim.doy)
NorWeST_obs$tim.year <- as.numeric(NorWeST_obs$tim.year)
NorWeST_obs$tim.month <- as.numeric(NorWeST_obs$tim.month)
NorWeST_obs$COMID <- as.character(NorWeST_obs$COMID)

# EXPORT ----
data.table::fwrite(NorWeST_obs, file = paste0("data/response/NorWeST_obs.csv"))
rm(Clear, ClearPoints, MidColumbia, MidColumbiaPoints, MidSnake, MidSnakePoints, OrCoast, OrCoastPoints, Salmon, SalmonPoints,
   SnakeBear, SnakeBearPoints, SouthCentOre, SouthCentOrePoints, SpoKoot, SpoKootPoints, UCYak, UCYakPoints, WaCoast, WaCoastPoints,
   cols2keep)


# Compare to version of NorWeST data downloaded in 2021
oldv <- data.table::fread("data/response/NorWestAll.csv")
(sdf <- setdiff(NorWeST_obs$COMID, as.character(oldv$COMID))) #1337 extra (same date range)
# Mapped externally; these are located in ID and eastern OR
write.csv(sdf, "data/response/extra_COMIDS_new_NorWeST_data.csv", row.names = F) 
setdiff(as.character(oldv$COMID), NorWeST_obs$COMID) #0 in old version but not new
summary(as.factor(obs_data$cov.NorWeST_region))
oldv$NorWeST_region <- gsub("_.*", "", oldv$NorWeST_ID)
summary(as.factor(oldv$NorWeST_region))

