# function to merge ECC and ECCC data together
# note: all data needs to be saved in wd ("allstations_raw.rds", "ECCC_Climate_Data_Merged.rds", "sites.csv")

# Arguments for testing:

# wd = "C:/Users/Emma_Riley/Documents/R_Scripts/Packages/nwtclimate/data"
# filepath = "C:/Users/Emma_Riley/Documents/R_Scripts/Packages/nwtclimate/data"


# fill in below as desired, run everything in ECC_ECCC_helper.R and Station_Merge function first and this filled-in section last
StationMerge(
  filepath = "C:/Users/Emma_Riley.corp/Documents/R_Scripts/Packages/nwtclimate/data", # change this to where files are/will be saved
  filepath_name1 = Quote('allstations_flagged_to2024.rds'), # ECC data: allstations.rds, allstations_flagged_raw.rds or allstations_flagged_raw_DBcompatible.rds
  filepath_name2 = Quote('ECCC_Climate_Data_Merged.rds'), # ECCC data: ECCC_Climate_Data_Merged.rds
  filepath_name3 = Quote('cd_stations.csv'), #ECC sites metadata
  save_files = TRUE, # if TRUE, file will be saved as rds
  save_files_raw = TRUE # if TRUE, raw ECC data is used (not QAQC'd)
)

# main function 
StationMerge<-function(
    filepath = "C:/Users/Emma_Riley/Documents/R_Scripts/Packages/nwtclimate/data", 
    filepath_name1 = Quote('allstations_flagged_raw.rds'), #changed the input file
    filepath_name2 = Quote('ECCC_Climate_Data_Merged.rds'), 
    filepath_name3 = Quote('cd_stations.csv'), #changed this file also
    save_files = TRUE,
    save_files_raw = TRUE
)

{# Import climate data locally (ECC & ECCC) and ECC sites metadata
data_ECC <- readRDS(paste0(filepath, "/", filepath_name1))
data_ECCC <- readRDS(paste0(filepath, "/", filepath_name2))
sites_ECC <- read.csv(paste0(filepath, "/", filepath_name3))

#append ECC metadata to data_ECC sheet
# Lat = appendvar(data_ECC$station_name, sites_ECC$station_name, sites_ECC$lat) #changed these names slightly
# Long = appendvar(data_ECC$station_name, sites_ECC$station_name, sites_ECC$lon) #changed these names slightly
# Elev = appendvar(data_ECC$station_name, sites_ECC$station_name, sites_ECC$elev) #changed these names slightly
# data_ECC = cbind(data_ECC, Lat, Long, Elev)

data_ECC <- data_ECC %>%
  dplyr::relocate(c(39:41), .after = last_col())

#colnames for ECC and ECCC data - in helper function
colnames(data_ECC) <- names_ECC_hr
#data_ECC$Date <- as.Date(data_ECC$Date) DB compatible data does not have a date column
colnames(data_ECCC) <- names_ECCC_daily
data_ECCC$Date <- as.Date(data_ECCC$Date)

# Hour & Time columns
# data_ECC$Time <- lubridate::ymd_hms(base::paste(data_ECC$Date,base::sprintf("%02d:00:00",data_ECC$Hour)))
# data_ECC$Hour <- base::sprintf("%02d:00", data_ECC$Hour)

# merged_name column
data_ECC$merged_name <- data_ECC$Station_name

#flag hourly values from data_ECC
#flag_data(data_ECC) #returns dataframe "data_ECC_flagged"

#clean hourly values from data_ECC
data_ECC_cleaned <- clean_data_working(data_ECC) #returns dataframe "data_ECC_cleaned"

# calc daily data values for ECC data (using helper function in ECC_ECCC_helper.R)
data_ECC_cleaned_daily <- summarize_data(data_ECC_cleaned) 
data_ECC_raw_daily <- summarize_data(data_ECC) # option for returning a raw dataframe

print(colnames(data_ECC_cleaned_daily))
print(colnames(data_ECC_raw_daily))
print(colnames(data_ECCC))

# merge data
merged_data_cleaned <- base::merge(data_ECC_cleaned_daily, data_ECCC,
                           by = c("Station_name","Year","Month","Day",
                                  "T_air","Total_precip","merged_name", "Lat", "Long", "Elev"), all = TRUE) #removed Date from this line - ER

merged_data_raw <- base::merge(data_ECC_cleaned_daily, data_ECCC,
                                   by = c("Station_name","Year","Month","Day","Date",
                                          "T_air","Total_precip","merged_name", "Lat", "Long", "Elev"), all = TRUE)

# Save the merged_data dataframe as an RDS file
if (save_files) {
  saveRDS(merged_data_cleaned, file.path(filepath, "Climate_data_clean_merged.rds"))
} else {
  print("Dataframe not saved to wd")
}
if (save_files_raw) {
  saveRDS(merged_data_raw, file.path(filepath, "Climate_data_raw_merged.rds"))
} else {
  print("raw dataframe not saved to wd")
}

}

