# function to merge ECC and ECCC data together
# note: all data needs to be saved in wd ("allstations_raw.rds", "ECCC_Climate_Data_Merged.rds", "sites.csv")

# Arguments for testing:

# wd = "C:/Users/maucl/Documents/R_Scripts/Packages/nwtclimate/data"
# filepath = "C:/Users/maucl/Documents/R_Scripts/Packages/nwtclimate/data"

# main function 
StationMerge<-function(
    filepath, 
    filepath2, # Added Feb 18 by MA
    filepath_name1, 
    filepath_name2, 
    filepath_name4, # Added Feb 18 by MA
    filepath_name5, # Added Feb 18 by MA
    filepath_name6, # Added Mar 12 by MA
    filepath_name7, # Add Mar 12 by MA
    save_files,
    save_files_raw
)
  
{# Import climate data locally (ECC & ECCC) and ECC sites metadata
  
  data_ECC <- readRDS(paste0(filepath, "/", filepath_name1))
  data_ECCC <- readRDS(paste0(filepath, "/", filepath_name2))

  data_ECC <- data_ECC %>%
    dplyr::filter((is.na(hour)==FALSE & is.na(date) == FALSE))
  
  data_ECC$date <- as.Date(data_ECC$date)
  colnames(data_ECCC) <- names_ECCC_daily
  data_ECCC$date <- as.Date(data_ECCC$date)
  
  data_ECCC <- replace_coordinates(data_ECCC)
  
  data_ECC$time <- lubridate::ymd_hms(base::paste(data_ECC$date,base::sprintf("%02d:00:00",data_ECC$hour)))
  data_ECC$hour <- base::sprintf("%02d:00", data_ECC$hour)

  data_ECC$merged_name <- data_ECC$station_name

  data_ECC_flagged <- flag_data(data_ECC) 

  data_ECC_cleaned <-clean_data(data_ECC_flagged)

  data_ECC_cleaned_daily <- summarize_data(data_ECC_cleaned)
  data_ECC_raw_daily <- summarize_data(data_ECC) 

  data_ECC_cleaned_daily$day <- as.numeric(data_ECC_cleaned_daily$day)
  data_ECC_raw_daily$day <- as.numeric(data_ECC_raw_daily$day)
  
  scotty_clean_daily <- readRDS(paste0(filepath2, "/", filepath_name4))
  scotty_raw_daily <- readRDS(paste0(filepath2, "/", filepath_name5))
  scotty_clean_daily$merged_name <- "Scotty Creek"
  scotty_raw_daily$merged_name <- "Scotty Creek"
  
  FTS_clean_daily <- readRDS(paste0(filepath, "/", filepath_name6))
  FTS_raw_daily <- readRDS(paste0(filepath, "/", filepath_name7))

  newdata_clean <- dplyr::bind_rows(data_ECC_cleaned_daily, scotty_clean_daily)
  data_ECCC_FTS <- dplyr::bind_rows(data_ECCC, FTS_clean_daily)
  merged_data_cleaned <- dplyr::bind_rows(newdata_clean, data_ECCC_FTS)
                               
  newdata_raw <- dplyr::bind_rows(data_ECC_raw_daily, scotty_raw_daily)
  data_ECCC_FTS_raw <- dplyr::bind_rows(data_ECCC, FTS_raw_daily)
  merged_data_raw <- dplyr::bind_rows(newdata_raw, data_ECCC_FTS_raw)
  
  waterssites = c("Blueberry",
                  "BDL",
                  "Colomac",
                  "Daring FTS",
                  "Daring Lake",
                  "Dempster515",
                  "Dempster85",
                  "Discovery",
                  "Giant Mine",
                  "Harry FTS",
                  "Harry",
                  "Hoarfrost FTS",
                  "ITH FTS",
                  "Lupin",
                  "Mile222",
                  "Nanisivik",
                  "Peel FTS",
                  "Peel",
                  "Pocket",
                  "Salmita",
                  "Silver Bear",
                  "Snare Rapids",
                  "Taglu",
                  "Tibbitt",
                  "Tibbitt Pine",
                  "Tuktoyaktuk",
                  "Walker Bay",
                  "Winter Lake",
                  "YK Ski Club",
                  "Scotty Creek",
                  "Powder Lake",
                  "Yaltea Lake",
                  "Ninelin Lake",
                  "Forestry Lake",
                  "Wrigley")
  
  `%!in%` = Negate(`%in%`)
  
  merged_ECCC <- merged_data_cleaned %>%
    dplyr::filter(merged_name %!in% waterssites) %>%
    dplyr::relocate(water_year, .before = year)
  
  merged_data <- merged_data_cleaned %>%
    dplyr::filter(merged_name %in% waterssites) %>%
    dplyr::relocate(water_year, .before = year) 
  
  data <- rbind(merged_ECCC, merged_data)


  if (save_files) {
    saveRDS(data, file.path(filepath, "Climate_data_clean_merged.rds"))
  } else {
    print("Dataframe not saved to wd")
  }
  if (save_files_raw) {
    saveRDS(merged_data_raw, file.path(filepath, "Climate_data_raw_merged.rds"))
  } else {
    print("raw dataframe not saved to wd")
  }
  
}

# Helper functions
{
  # column names:
  {# column names for daily ECCC data
    names_ECCC_daily <- c(
      "station_name",
      "station_id",
      "station_operator",
      "prov",
      "lat",
      "lon",
      "elev",
      "climate_id",
      "wmo_id",
      "tc_id",
      "date",
      "year",
      "month",
      "day",
      "qual",
      "cool_deg_days",
      "cool_deg_days_flag",
      "dir_max_gust",
      "dir_max_gust_flag",
      "heat_deg_days",
      "heat_deg_days_flag",
      "t_air_max",
      "t_air_max_flag",
      "t_air",
      "t_air_flag",
      "t_air_min",
      "t_air_min_flag",
      "snow_grnd",
      "snow_grnd_flag",
      "spd_max_gust",
      "spd_max_gust_flag",
      "total_precip",
      "total_precip_flag",
      "total_rain",
      "total_rain_flag",
      "total_snow",
      "total_snow_flag",
      "merged_name"
    )
  }
  
  # function to replace the coordinates that are wrong in df (Fort McPherson & Tulita)
  replace_coordinates <- function(df) {
    if ("Fort McPherson" %in% df$merged_name) {
      # Find rows where merged_name is "Fort McPherson"
      fort_mcpherson_rows <- df$merged_name == "Fort McPherson"
      # Replace lat and lon values for "Fort McPherson"
      df$lat[fort_mcpherson_rows] <- 67.45
      df$lon[fort_mcpherson_rows] <- -134.88
    }
    if ("Tulita" %in% df$merged_name) {
      # Find rows where merged_name is "Tulita"
      tulita_rows <- df$merged_name == "Tulita"
      # Replace lat and lon values for "Tulita"
      df$lat[tulita_rows] <- 64.90
      df$lon[tulita_rows] <- -125.57
    }
    return(df)
  }
  
  
}


StationMerge(
  filepath = "C:/Users/maucl/Documents/R_Scripts/Packages/nwtclimate/data", # change this to where files are/will be saved
  filepath2 = "C:/Users/maucl/Documents/Data/R_data", # Added Feb 18 by MA
  filepath_name1 = Quote('allstations_raw.rds'), # ECC data: allstations.rds
  filepath_name2 = Quote('ECCC_Climate_Data_Merged.rds'), # ECCC data: ECCC_Climate_Data_Merged.rds
  filepath_name4 = Quote('Scotty_clean_daily.rds'), # added Feb 18 by MA 
  filepath_name5 = Quote('Scotty_raw_daily.rds'), # added Feb 18 by MA
  filepath_name6 = Quote('FTS_othersites_clean_daily.rds'), # added Mar 12 by MA
  filepath_name7 = Quote('FTS_othersites_raw_daily.rds'), # added Mar 12 by MA
  save_files = TRUE, # if TRUE, cleaned file will be saved as rds
  save_files_raw = TRUE # if TRUE, raw ECC data is used (not QAQC'd)
)





# assign variables --------------------------------------------------------
# 
# 
# 
  # filepath <- "C:/Users/maucl/Documents/R_Scripts/Packages/nwtclimate/data" # change this to where files are/will be saved
  # filepath2 <- "C:/Users/maucl/Documents/Data/R_data" # Added Feb 18 by MA
  # filepath_name1 <- Quote('allstations_raw.rds') # ECC data: allstations.rds
  # filepath_name2 <- Quote('ECCC_Climate_Data_Merged.rds') # ECCC data: ECCC_Climate_Data_Merged.rds
  # filepath_name4 <- Quote('Scotty_clean_daily.rds') # added Feb 18 by MA
  # filepath_name5 <- Quote('Scotty_raw_daily.rds') # added Feb 18 by MA
  # filepath_name6 <- Quote('FTS_othersites_clean_daily.rds') # added Mar 12 by MA
  # filepath_name7 <- Quote('FTS_othersites_raw_daily.rds') # added Mar 12 by MA
  # save_files <- TRUE # if TRUE, cleaned file will be saved as rds
  # save_files_raw <- TRUE # if TRUE, raw ECC data is used (not QAQC'd)

