#Logger file to excel file conversion with database compatibility
#Logger files are saved to excel files, but have headers from logger programs
#file headers may vary from year to year
#each file conversion requires verification

library(haven)

#opposite of %in%:
`%!in%` = Negate(`%in%`)

#leap year function
is_leap_year <- function(year) {
  (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
}

#define pathways
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME"))) 
wf_path <- paste0(user, "/Documents/R_Scripts/Functions/Workflows/") #added /Documents/ here before R_Scripts
package_path <- paste0(user, "/Documents/R_Scripts/Packages/") #added /Documents/ here before R_Scripts
nwtclimate_path <- paste0(package_path, "nwtclimate/")
hydroclim_path <- paste0(package_path, "hydroclim/")
R_path <- paste0("R/")
#data_path <- paste0(nwtclimate_path, "data/")
datapath <- paste0(user, "/Documents/NT_Hydrology/Abiotic Monitoring/Data/Data_file_dump/", lubridate::year(Sys.Date()), "/")
#manual edit datapath
#datapath <- paste0("C:/Users/emma_riley/Documents/NT_Hydrology/Abiotic Monitoring/Data/Data_file_dump/", "2024", "/")

#source necessary functions
source(paste0(nwtclimate_path, R_path, "dependencies_functions.R"))

#All column names, except station and year, which are defined/calculated later
all_columns <- c("JD", "Month", "Day", "Hour", "Date", "Air Temperature", "Relative Humidity","Rainfall", "Wind Speed", "Wind direction",
                 "Net SW Radiation", "Net LW Radiation", "Net Radiation", "Incoming SW Radiation",
                 "Snow depth", "TS_1", "TS_2", "TS_3", "TS_4", "TS_5", "TS_6",
                 "Air Temperature_2", "Relative Humidity_2", "Twater", "Twater_2",
                 "DepthTW", "dataflag_AT", "dataflag_RH", "dataflag_Rn", "dataflag_WS",
                 "dataflag_WD", "dataflag_ISW", "dataflag_SD", "dataflag_NR", "dataflag_NSW",
                 "dataflag_NLW", "notes", "WDep_corr")

#BDL
start_year <- 2024 #start data file year
station <- "BDL" #station name
file_name <- "BDL SEPT30_AUG30 2025 HOURLY"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = T)

if (station == "BDL"){
  
  colnames(data) <- c("JD", "Hour", "Rainfall", "Air Temperature")
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  data[, new_columns] <- NA
  data <- dplyr::mutate(data, Year = {
    year <- start_year
    years <- numeric(dplyr::n())  # Preallocate a vector for performance
    for (i in seq_along(data$JD)) {
      threshold <- if (is_leap_year(year)) 366 else 365
      if (i > 1 && JD[i] < JD[i - 1]) {
        year <- year + 1  # Increment year when JD resets
      } else if (JD[i] > threshold) {
        stop("Invalid Julian Day value exceeding the number of days in the year.")
      }
      years[i] <- year
    }
    years
  })

  data <- dplyr::mutate(data, Station = station)
  
  write.csv(data, paste0(datapath, "Complete files/", "BDL.csv"), row.names = F)
}

#Blueberry
station <- "Blueberry" #station name
file_name <- "Blueberry 2024_2025"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = T)

if (station == "Blueberry"){
  
  colnames(data) <- c("Date", "Record", "Rainfall", "Incoming SW Radiation", "Air Temperature_max", "Air Temperature_min", "Air Temperature", "Relative Humidity")
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  data <- dplyr::select(data, -c("Record", "Air Temperature_max", "Air Temperature_min"))
  data[, new_columns] <- NA
  data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y-%m-%d %H:%M:%S")))
  data <- dplyr::mutate(data, Year = lubridate::year(Date))
  data <- dplyr::mutate(data, Month = lubridate::month(Date))
  data <- dplyr::mutate(data, Day = lubridate::day(Date))
  data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
  data <- dplyr::mutate(data, Station = station)
  
  write.csv(data, paste0(datapath, "Complete files/", "Blueberry.csv"), row.names = F)
}

#Dempster85
station <- "Dempster85" #station name
file_name <- "Dempster 8.5 2024_2025"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = T)

if (station == "Dempster85"){
  
  colnames(data) <- c("Date", "Record", "BattV", "Ptemp_C", "NetSW_Avg", "NetLW_Avg", "NetRad_Avg", "Rainfall", "Wind Speed", "Wind direction", "Air Temperature", "Air Temperature_max", "Air Temperature_min")
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  data <- dplyr::select(data, -c("Record", "BattV", "Ptemp_C", "NetSW_Avg", "NetLW_Avg", "NetRad_Avg", "Air Temperature_max", "Air Temperature_min"))
  data[, new_columns] <- NA
  data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y-%m-%d %H:%M:%S")))
  data <- dplyr::mutate(data, Year = lubridate::year(Date))
  data <- dplyr::mutate(data, Month = lubridate::month(Date))
  data <- dplyr::mutate(data, Day = lubridate::day(Date))
  data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
  data <- dplyr::mutate(data, Station = station)
  
  write.csv(data, paste0(datapath, "Complete files/", "Dempster85.csv"), row.names = F)
}

#Dempster515
station <- "Dempster515" #station name
file_name <- "Dempster515 2024_2025"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = F)

if (station == "Dempster515"){
  
  colnames(data) <- c("Date", "Record", "Wind Speed", "Wind direction", "Air Temperature", "Relative Humidity", "Rainfall")
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  data <- dplyr::select(data, -c("Record"))
  data[, new_columns] <- NA
  data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y-%m-%d %H:%M:%S")))
  data <- dplyr::mutate(data, Year = lubridate::year(Date))
  data <- dplyr::mutate(data, Month = lubridate::month(Date))
  data <- dplyr::mutate(data, Day = lubridate::day(Date))
  data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
  data <- dplyr::mutate(data, Station = station)
  
  write.csv(data, paste0(datapath, "Complete files/", "Dempster515.csv"), row.names = F)
}


#Harry
station <- "Harry" #station name
file_name <- "Harry2024_2025"
file_path  <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = T)

if (station == "Harry") {
  
  # Manually assign column names in the same order as your logger program
  colnames(data) <- c(
    "Date",
    "Record",
    "Air Temperature sample",
    "Air Temperature",
    "Rainfall",
    "Net SW Radiation",
    "Net LW Radiation",
    "Net Radiation",
    "Wind Speed",
    "Wind direction"
  )
  
  # Add any missing standard columns
  new_columns <- all_columns[all_columns %!in% colnames(data)]
  data <- dplyr::select(data, -c("Record", "Air Temperature sample"))
  data[, new_columns] <- NA
  
  # Parse datetime and add time components
  data <- data %>%
    dplyr::mutate(
      Date    = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      Year    = lubridate::year(Date),
      Month   = lubridate::month(Date),
      Day     = lubridate::day(Date),
      Hour    = lubridate::hour(Date),
      Station = station
    )
  
  # Optional: drop any rows before your start_year if needed
  # data <- filter(data, Year >= start_year)
  
  # Save as CSV
  write.csv(
    data,
    file = paste0(datapath, "Complete files/", "Harry.csv"),
    row.names = FALSE
  )
}

#Peel
station <- "Peel" #station name
file_name <- "Peel2024_2025"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = T)

if (station == "Peel"){
  
  colnames(data) <- c("Date", "Record", "Air Temperature sample", "Air Temperature", "Rainfall", "Soil_Moist1", "Soil_Moist2",  "Net SW Radiation", "Net LW Radiation", "Net Radiation", "Wind Speed", "Wind direction" )
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  data <- dplyr::select(data, -c("Record", "Air Temperature sample", "Soil_Moist1", "Soil_Moist2"))
  data[, new_columns] <- NA
  data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y-%m-%d %H:%M:%S")))
  data <- dplyr::mutate(data, Year = lubridate::year(Date))
  data <- dplyr::mutate(data, Month = lubridate::month(Date))
  data <- dplyr::mutate(data, Day = lubridate::day(Date))
  data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
  data <- dplyr::mutate(data, Station = station)
  
  write.csv(data, paste0(datapath, "Complete files/", "Peel.csv"), row.names = F)
}

#Taglu
station <- "Taglu" #station name
file_name <- "Taglu_2024_2025"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = T)

if (station == "Taglu"){
  
  colnames(data) <- c("Date", "Record", "Air Temperature sample", "Air Temperature", "Rainfall", "Net SW Radiation", "Net LW Radiation", "Net Radiation", "Wind Speed", "Wind direction" )
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  data <- dplyr::select(data, -c("Record", "Air Temperature sample"))
  data[, new_columns] <- NA
  data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y-%m-%d %H:%M:%S")))
  data <- dplyr::mutate(data, Year = lubridate::year(Date))
  data <- dplyr::mutate(data, Month = lubridate::month(Date))
  data <- dplyr::mutate(data, Day = lubridate::day(Date))
  data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
  data <- dplyr::mutate(data, Station = station)
  
  write.csv(data, paste0(datapath, "Complete files/", "Taglu.csv"), row.names = F)
}

#Tuk
station <- "Tuktoyaktuk" #station name
file_name <- "TukHourly2024_2025"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = F)

if (station == "Tuktoyaktuk"){
  
  colnames(data) <- c("Date", "Record", "Air Temperature sample", "Air Temperature", "Rainfall", "Net SW Radiation", "Net LW Radiation", "Net Radiation", "Wind Speed", "Wind direction" )
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  data <- dplyr::select(data, -c("Record", "Air Temperature sample"))
  data[, new_columns] <- NA
  data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y-%m-%d %H:%M:%S")))
  data <- dplyr::mutate(data, Year = lubridate::year(Date))
  data <- dplyr::mutate(data, Month = lubridate::month(Date))
  data <- dplyr::mutate(data, Day = lubridate::day(Date))
  data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
  data <- dplyr::mutate(data, Station = station)
  
  write.csv(data, paste0(datapath, "Complete files/", "Tuktoyaktuk.csv"), row.names = F)
}

#Pocket
station <- "Pocket" #station name
file_name <- "PocketMay_Oct2025"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = T)

if (station == "Pocket"){
  
  colnames(data) <- c("Date", "record", "Rainfall", "battv", "Air Temperature", "Air Temperature_2", "Relative Humidity",  "Wind Speed", "wind speed max", "Wind direction", "Net Radiation_obs", "Net Radiation")
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  data <- dplyr::select(data, c("Date", "Rainfall", "Air Temperature", "Air Temperature_2", "Relative Humidity",  "Wind Speed", "Wind direction", "Net Radiation"))
  data[, new_columns] <- NA
  data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y-%m-%d %H:%M:%S")))
  data <- dplyr::mutate(data, Year = lubridate::year(Date))
  data <- dplyr::mutate(data, Month = lubridate::month(Date))
  data <- dplyr::mutate(data, Day = lubridate::day(Date))
  data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
  data <- dplyr::mutate(data, Station = station)
  
  write.csv(data, paste0(datapath, "Complete files/", "Pocket.csv"), row.names = F)
  
}

#Tibbitt
start_year<- 2024 #data file start year
station <- "Tibbitt muskeg" #station name
file_name <- "TIBBITT JUNE24_SEPT 2025"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = T)

if (station == "Tibbitt muskeg"){
  
  colnames(data) <- c( "JD", "Hour", "Air Temperature", "Relative Humidity", "Wind Speed", "Rainfall", "Incoming SW Radiation", "TS_1", "TS_2", "TS_3", "TS_4", "TS_5", "TS_6")
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  data <- dplyr::select(data, colnames(data))
  data <- dplyr::mutate(data, Year = {
    year <- start_year
    years <- numeric(dplyr::n())  # Preallocate a vector for performance
    for (i in seq_along(data$JD)) {
      threshold <- if (is_leap_year(year)) 366 else 365
      if (i > 1 && JD[i] < JD[i - 1]) {
        year <- year + 1  # Increment year when JD resets
      } else if (JD[i] > threshold) {
        stop("Invalid Julian Day value exceeding the number of days in the year.")
      }
      years[i] <- year
    }
    years
  })
  data <- dplyr::mutate(data, Station = station)
  data[, new_columns] <- NA
  
  write.csv(data, paste0(datapath, "Complete files/", "Tibbitt muskeg.csv"), row.names = F)
}

#Snare
start_year<- 2024 #data file start year
station <- "Snare rapids" #station name
file_name <- "SNARE NOV_OCT 2025 HOURLY"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = T)

if (station == "Snare rapids"){
  
  colnames(data) <- c("JD", "Hour", "Air Temperature", "Rainfall", "Wind Speed", "Wind direction")
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  #data <- dplyr::filter(data, data.type == 61)
  data <- dplyr::select(data, colnames(data))
  #data <- dplyr::select(data, -c("data.type", "NA1", "NA2", "NA3", "NA4", "NA5"))
  data <- dplyr::mutate(data, Year = {
    year <- start_year
    years <- numeric(dplyr::n())  # Preallocate a vector for performance
    for (i in seq_along(data$JD)) {
      threshold <- if (is_leap_year(year)) 366 else 365
      if (i > 1 && JD[i] < JD[i - 1]) {
        year <- year + 1  # Increment year when JD resets
      } else if (JD[i] > threshold) {
        stop("Invalid Julian Day value exceeding the number of days in the year.")
      }
      years[i] <- year
    }
    years
  })
  data <- dplyr::mutate(data, Station = station)
  data[, new_columns] <- NA
  
  write.csv(data, paste0(datapath, "Complete files/", "Snare rapids.csv"), row.names = F)
}

#Whati - LLM-N
station <- "LLM-N" #station name
file_name <- "Whati_LLMN_hourly_23_2025"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = T)

if (station == "LLM-N"){
  
  colnames(data) <- c("Date", "record",  "Air Temperature_sample", "Air Temperature",  "Rainfall", "Relative Humidity")
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  data <- dplyr::select(data, -c("record", "Air Temperature_sample"))
  data[, new_columns] <- NA
  data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y-%m-%d %H:%M:%S")))
  data <- dplyr::mutate(data, Year = lubridate::year(Date))
  data <- dplyr::mutate(data, Month = lubridate::month(Date))
  data <- dplyr::mutate(data, Day = lubridate::day(Date))
  data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
  data <- dplyr::mutate(data, Station = station)
  
  write.csv(data, paste0(datapath, "Complete files/", "LLM-N.csv"), row.names = F)
  
}

#Whati - LLM-E
station <- "LLM-E" #station name
file_name <- "Whati_LLME_hourly_24_2025"
file_path <- paste0(datapath, file_name)
data<-readxl::read_excel(paste0(file_path, ".xlsx"), col_names = T)

if (station == "LLM-E"){
  
  colnames(data) <- c("Date", "record",  "Air Temperature_sample", "Air Temperature",  "Rainfall", "Relative Humidity")
  new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
  data <- dplyr::select(data, -c("record", "Air Temperature_sample"))
  data[, new_columns] <- NA
  data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y-%m-%d %H:%M:%S")))
  data <- dplyr::mutate(data, Year = lubridate::year(Date))
  data <- dplyr::mutate(data, Month = lubridate::month(Date))
  data <- dplyr::mutate(data, Day = lubridate::day(Date))
  data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
  data <- dplyr::mutate(data, Station = station)
  
  write.csv(data, paste0(datapath, "Complete files/", "LLM-E.csv"), row.names = F)
  
}

# #FTS stations - no longer needed bc of automatic upload
# #Daring FTS
# datapath <- "C:/Users/emma_riley.CORP/Documents/NT_Hydrology/Abiotic Monitoring/Data/Data_file_dump/2024/FTS/"
# start_year<- 2023 #year you downloaded the data from the logger
# station <- "Daring FTS" #station name
# file_name <- "Daring Lake-2024-9-3-14-28-42"
# file_path <- paste0(datapath, file_name)
# data<-read.csv(paste0(file_path, ".csv"), skip=1)
# 
# if (station == "Daring FTS"){
#   
#   colnames(data) <- c("Date", "Tmax", "Tmin", "Rhmax", "Rhmin", "Rn24", 
#                       "Air Temperature", "Relative Humidity", "Wind Speed_nan", 
#                       "Mx_spd_nan", "Mx_dir", "Wind direction", "Rainfall",
#                       "Tmax", "Tmin", "Rhmax", "Rhmin", "Vb",
#                       "Incoming SW Radiation", "Rn_total", "Snow depth", "TS_1", "TS_2", "TS_3",
#                       "Wind Speed", "Max_spd", "Baro")
#   
#   new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
#   data <- dplyr::select(data, colnames(data)[colnames(data) %in% c(all_columns)==TRUE])
#   data <- dplyr::mutate(data, Station = station)
#   data[, new_columns] <- NA
#   data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y/%m/%d %H:%M:%S")))
#   data <- dplyr::mutate(data, Year = lubridate::year(Date))
#   data <- dplyr::mutate(data, Month = lubridate::month(Date))
#   data <- dplyr::mutate(data, Day = lubridate::day(Date))
#   data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
#   
#   write.csv(data, paste0(datapath, "Complete files/", "Daring FTS.csv"), row.names = F)
# }
# 
# #Harry FTS
# datapath <- "C:/Users/emma_riley.CORP/Documents/NT_Hydrology/Abiotic Monitoring/Data/Data_file_dump/2024/FTS/"
# start_year<- 2022 #year you downloaded the data from the logger
# station <- "Harry FTS" #station name
# file_name <- "Harry Channel Mac Delta-2024-8-9-14-24-48"
# file_path <- paste0(datapath, file_name)
# data<-read.csv(paste0(file_path, ".csv"), skip=2)
# 
# if (station == "Harry FTS"){
#   
#   colnames(data) <- c("Date", "Air Temperature", "Relative Humidity", "Wind Speed", 
#                       "Mx_spd", "Mx_dir", "Wind direction", "Rainfall",
#                       "Tmax", "Tmin", "Rhmax", "Rhmin", "Vb",
#                       "Incoming SW Radiation", "Rn_total", "Snow depth", "TS_1", "TS_2", "TS_3",
#                       "Tmax24", "Tmin24", "Rhmax24", "Rhmin24", "Rn24", "Baro")
#   
#   new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
#   data <- dplyr::select(data, colnames(data)[colnames(data) %in% c(all_columns)==TRUE])
#   data <- dplyr::mutate(data, Station = station)
#   data[, new_columns] <- NA
#   data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y/%m/%d %H:%M:%S")))
#   data <- dplyr::mutate(data, Year = lubridate::year(Date))
#   data <- dplyr::mutate(data, Month = lubridate::month(Date))
#   data <- dplyr::mutate(data, Day = lubridate::day(Date))
#   data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
#   write.csv(data, paste0(datapath, "Complete files/", "Harry FTS.csv"), row.names = F)
# }
# 
# #Hoarfrost FTS
# datapath <- "C:/Users/emma_riley.CORP/Documents/NT_Hydrology/Abiotic Monitoring/Data/Data_file_dump/2024/FTS/"
# start_year<- 2022 #year you downloaded the data from the logger
# station <- "Hoarfrost FTS" #station name
# file_name <- "Hoarfrost-2024-9-3-17-31-26"
# file_path <- paste0(datapath, file_name)
# data<-read.csv(paste0(file_path, ".csv"), skip=1)
# 
# if (station == "Hoarfrost FTS"){
#   
#   colnames(data) <- c("Date", "Air Temperature", "Relative Humidity", "Wind Speed", 
#                       "Mx_spd", "Mx_dir", "Wind direction", "Rainfall",
#                       "Tmax", "Tmin", "Rhmax", "Rhmin", "Vb",
#                       "Incoming SW Radiation", "Rn_total", "Snow depth", "TS_1", "TS_2", "TS_3",
#                       "Baro", "Tmax24", "Tmin24", "Rhmax24", "Rhmin24", "Rn24")
#   
#   new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
#   data <- dplyr::select(data, colnames(data)[colnames(data) %in% c(all_columns)==TRUE])
#   data <- dplyr::mutate(data, Station = station)
#   data[, new_columns] <- NA
#   data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y/%m/%d %H:%M:%S")))
#   data <- dplyr::mutate(data, Year = lubridate::year(Date))
#   data <- dplyr::mutate(data, Month = lubridate::month(Date))
#   data <- dplyr::mutate(data, Day = lubridate::day(Date))
#   data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
#   write.csv(data, paste0(datapath, "Complete files/", "Hoarfrost FTS.csv"), row.names = F)
# }
# 
# #ITH FTS
# datapath <- "C:/Users/emma_riley.CORP/Documents/NT_Hydrology/Abiotic Monitoring/Data/Data_file_dump/2024/FTS/"
# start_year<- 2023 #year you downloaded the data from the logger
# station <- "ITH FTS" #station name
# file_name <- "Inuvik-Tuk Hwy-2024-9-16-15-26-27"
# file_path <- paste0(datapath, file_name)
# data<-read.csv(paste0(file_path, ".csv"), skip=1)
# 
# if (station == "ITH FTS"){
#   
#   colnames(data) <- c("Date", "Tmax", "Tmin", "Rhmax", "Rhmin", "Rn24", 
#                       "Air Temperature", "Relative Humidity", "Wind Speed_nan", 
#                       "Mx_spd_nan", "Mx_dir", "Wind direction", "Rainfall",
#                       "Tmax", "Tmin", "Rhmax", "Rhmin", "Vb",
#                       "Incoming SW Radiation", "Rn_total", "Snow depth", "TS_1", "TS_2", "TS_3",
#                       "Wind Speed", "Max_spd", "Baro")
#   
#   new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
#   data <- dplyr::select(data, colnames(data)[colnames(data) %in% c(all_columns)==TRUE])
#   data <- dplyr::mutate(data, Station = station)
#   data[, new_columns] <- NA
#   data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y/%m/%d %H:%M:%S")))
#   data <- dplyr::mutate(data, Year = lubridate::year(Date))
#   data <- dplyr::mutate(data, Month = lubridate::month(Date))
#   data <- dplyr::mutate(data, Day = lubridate::day(Date))
#   data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
#   write.csv(data, paste0(datapath, "Complete files/", "ITH FTS.csv"), row.names = F)
# }
# 
# #Peel FTS
# datapath <- "C:/Users/emma_riley.CORP/Documents/NT_Hydrology/Abiotic Monitoring/Data/Data_file_dump/2024/FTS/"
# start_year<- 2023 #year you downloaded the data from the logger
# station <- "Peel FTS" #station name
# file_name <- "Peel Plateau km 53-2024-8-7-16-46-56"
# file_path <- paste0(datapath, file_name)
# data<-read.csv(paste0(file_path, ".csv"), skip=2) #change skip based on if units are included in download step
# 
# if (station == "Peel FTS"){
#   
#   colnames(data) <- c("Date", "Air Temperature", "Relative Humidity", "Wind Speed", 
#                       "Mx_spd", "Mx_dir", "Wind direction", "Rainfall",
#                       "Tmax", "Tmin", "Rhmax", "Rhmin", "Vb",
#                       "Incoming SW Radiation", "Rn_total", "Snow depth", "TS_1", "TS_2", "TS_3",
#                       "Tmax24", "Tmin24", "Rhmax24", "Rhmin24", "Rn24", "Baro")
#   
#   new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
#   data <- dplyr::select(data, colnames(data)[colnames(data) %in% c(all_columns)==TRUE])
#   data <- dplyr::mutate(data, Station = station)
#   data[, new_columns] <- NA
#   data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y/%m/%d %H:%M:%S")))
#   data <- dplyr::mutate(data, Year = lubridate::year(Date))
#   data <- dplyr::mutate(data, Month = lubridate::month(Date))
#   data <- dplyr::mutate(data, Day = lubridate::day(Date))
#   data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
#   write.csv(data, paste0(datapath, "Complete files/", "Peel FTS.csv"), row.names = F)
# }
# 
# #Winter FTS
# datapath <- "C:/Users/emma_riley.CORP/Documents/NT_Hydrology/Abiotic Monitoring/Data/Data_file_dump/2024/FTS/"
# start_year<- 2023 #year you downloaded the data from the logger
# station <- "Winter Lake FTS" #station name
# file_name <- "Winter Lake-2024-9-3-11-25-35"
# file_path <- paste0(datapath, file_name)
# data<-read.csv(paste0(file_path, ".csv"), skip=1) #change skip based on if units are included in download step
# 
# if (station == "Winter Lake FTS"){
#   
#   colnames(data) <- c("Date", "Tmax", "Tmin", "Rhmax", "Rhmin", "Rn24", 
#                       "Air Temperature", "Relative Humidity", "Wind Speed_nan", 
#                       "Mx_spd_nan", "Mx_dir", "Wind direction", "Rainfall",
#                       "Tmax", "Tmin", "Rhmax", "Rhmin", "Vb",
#                       "Incoming SW Radiation", "Rn_total", "Snow depth_obs", "TS_1", "TS_2", "TS_3",
#                       "Wind Speed", "Max_spd", "Snow depth", "Baro")
#   
#   new_columns <- all_columns[all_columns %!in% (colnames(data)) == TRUE]
#   data <- dplyr::select(data, colnames(data)[colnames(data) %in% c(all_columns)==TRUE])
#   data <- dplyr::mutate(data, Station = station)
#   data[, new_columns] <- NA
#   data <- dplyr::mutate(data, Date = as.POSIXct(Date, format = c("%Y/%m/%d %H:%M:%S")))
#   data <- dplyr::mutate(data, Year = lubridate::year(Date))
#   data <- dplyr::mutate(data, Month = lubridate::month(Date))
#   data <- dplyr::mutate(data, Day = lubridate::day(Date))
#   data <- dplyr::mutate(data, Hour = lubridate::hour(Date))
#   write.csv(data, paste0(datapath, "Complete files/", "Winter Lake FTS.csv"), row.names = F)
# }
