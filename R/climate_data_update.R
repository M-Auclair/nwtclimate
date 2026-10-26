#bring in new climate data (yearly) and append to existing rds file
library(dplyr)
library(stringr)
library(lubridate)

# Call functions from other worksheets into active environment

# Define file paths

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME"))) 
wf_path <- paste0(user, "R_Scripts/Functions/Workflows/")
package_path <- paste0(user, "/Documents/R_Scripts/Packages/")
nwtclimate_path <- paste0(package_path, "nwtclimate/")
R_path <- paste0("R/")
datapath <- paste0(user, "/Documents/NT_Hydrology/Abiotic Monitoring/Data/Data_file_dump/", lubridate::year(Sys.Date()), "/Complete files/")

source(paste0(nwtclimate_path, R_path, "dependencies_functions.R"))

#comment out stations below if no data were retrieved for a given year or if they have yet to be retrieved
stations <- c("Blueberry", 
              "BDL",
              "Dempster85",
              #"Dempster515",
              "Harry",
              "Peel",
              "Pocket",
              #"Snare rapids",
              "Taglu",
              "Tibbitt muskeg",
              "Tuktoyaktuk"
              #"LLM-E",
              #"LLM-N"
)
              
desired_columns <- c(
  "Station", "Year", "JD", "Month", "Day", "Hour", "Date", "Air Temperature", 
  "Relative Humidity", "Rainfall", "Wind Speed", "Wind direction",
  "Net SW Radiation", "Net LW Radiation", "Net Radiation", "Incoming SW Radiation",
  "Snow depth", "baro_pressure_mbar", "TS_1", "TS_2", "TS_3", "TS_4", "TS_5", "TS_6",
  "Air Temperature_2", "Relative Humidity_2", "Twater", "Twater_2",
  "DepthTW", "dataflag_AT", "dataflag_RH", "dataflag_Rn", "dataflag_WS",
  "dataflag_WD", "dataflag_ISW", "dataflag_SD", "dataflag_NR", "dataflag_NSW",
  "dataflag_NLW", "baro_pressure_flag", "notes", "WDep_corr"
)

for (i in stations){
file_name <- i
file_path <- paste0(datapath, file_name)
file_data<-read.csv(paste0(file_path, ".csv"), check.names = F)
for (col in desired_columns) {
  if (!(col %in% colnames(file_data))) {
    file_data[[col]] <- NA
  }
}

# Reorder columns to match the desired order
file_data <- file_data[desired_columns]

assign(paste0("", file_name), file_data)
}

#option to write out file_data - this is helpful for data requests
#writexl::write_xlsx(file_data, paste0(datapath, "Tuktoyaktuk2024_25.xlsx"))

#bring all these files together
#comment out stations not brought in above
climatedf_new<-dplyr::bind_rows(Blueberry,
                            BDL,
                            Dempster85,
                            #Dempster515,
                            Harry,
                            Peel,
                            Pocket,
                            #`Snare rapids`,
                            Taglu,
                            `Tibbitt muskeg`,
                            Tuktoyaktuk)
                            #`LLM-E`,
                            #`LLM-N`)

#use existing climate functions from climate_bindfiles

#rename all Tuktoyaktuk entries to Tuk for DB compatibility
climatedf<-climatedf_new %>%
  dplyr::mutate(Station = dplyr::if_else(Station == "Tuktoyaktuk", "Tuk", Station))

#bring in site information from read only database connection
connection <- odbc::dbConnect(drv = odbc::odbc(), dsn = "ECC_CLIMATE", uid = Sys.getenv("ORACLE_CLIM_USER"), pwd = Sys.getenv("ORACLE_CLIM_PWD"))

#stations/sites table
sites<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_STATIONS")

lat = appendvar(climatedf$Station, sites$STATION_NAME, sites$LATITUDE)
lon = appendvar(climatedf$Station, sites$STATION_NAME, sites$LONGITUDE)
elev = appendvar(climatedf$Station, sites$STATION_NAME, sites$ELEVATION)
climatedf<- cbind(climatedf, lat, lon, elev) # double check this object for any missing lat/long data - it could be a station name mismatch

# change col names
colnames <- c(
  "station_name",
  "year",
  "JD",
  "month",
  "day",
  "hour",
  "date",
  "t_air",
  "RH",
  "total_precip",
  "wind_sp",
  "wind_dir",
  "net_SW",
  "net_LW",
  "rn",
  "sw_in",
  "sr50",
  "baro_pressure_mbar",
  "t_soil_1",
  "t_soil_2",
  "t_soil_3",
  "t_soil_4",
  "t_soil_5",
  "t_soil_6",
  "t_air_2",
  "RH_2",
  "t_water",
  "t_water_2",
  "water_depth",
  "t_air_flag",
  "RH_flag",
  "rain_flag",
  "wind_sp_flag",
  "wind_dir_flag",
  "sw_in_flag",
  "sr50_flag",
  "rn_flag",
  "net_SW_flag",
  "net_LW_flag",
  "baro_pressure_flag",
  "notes",
  "water_depth_corr",
  "lat",
  "lon",
  "elev")
colnames(climatedf) <- colnames

#remove variables from active environment
rm(lat, lon, elev)

# #create hourly values from half hourly values at Mile 222 - this is done rarely, as Mile 222 hasn't been visited since ~2021
# Mile222 <- climatedf %>%
#   dplyr::filter(station_name == "Mile222") %>%
#   dplyr::group_by(station_name, year, JD, month, day, date, hour) %>%
#   dplyr::reframe(t_air = mean(t_air, na.rm=T),
#                  RH = mean(RH, na.rm=T),
#                  total_precip = sum(total_precip, na.rm=T),
#                  wind_sp = mean(wind_sp, na.rm=T),
#                  wind_dir = mean(wind_dir, na.rm=T),
#                  net_SW = mean(net_SW, na.rm=T),
#                  net_LW = mean(net_LW, na.rm=T),
#                  rn = mean(rn, na.rm=T),
#                  sw_in = mean(sw_in, na.rm=T),
#                  sr50 = mean(sr50, na.rm=T),
#                  t_soil_1 = mean(t_soil_1, na.rm=T),
#                  t_soil_2 = mean(t_soil_2, na.rm=T),
#                  t_soil_3 = mean(t_soil_3, na.rm=T),
#                  t_soil_4 = mean(t_soil_4, na.rm=T),
#                  t_soil_5 = mean(t_soil_5, na.rm=T),
#                  t_soil_6 = t_soil_6,
#                  t_air_2 = mean(t_air_2, na.rm=T),
#                  RH_2 = mean(RH_2, na.rm=T),
#                  t_water = mean(t_water, na.rm = T),
#                  t_water_2 = t_water_2,
#                  water_depth = water_depth,
#                  t_air_flag = t_air_flag,
#                  RH_flag = RH_flag,
#                  rain_flag = rain_flag,
#                  wind_sp_flag = wind_sp_flag,
#                  wind_dir_flag = wind_dir_flag,
#                  sw_in_flag = sw_in_flag,
#                  sr50_flag = sr50_flag,
#                  rn_flag = rn_flag,
#                  net_SW_flag = net_SW_flag, 
#                  net_LW_flag = net_LW_flag,
#                  notes = notes,
#                  water_depth_corr = water_depth_corr,
#                  lat = lat, 
#                  lon = lon,
#                  elev = elev)

#bringing edited Mile222 data back into climate df
# Mile222 <- Mile222 %>%
#   dplyr::relocate(date, .after = hour)
# Mile222 <- unique(Mile222)
# climatedf <- climatedf %>%
#   dplyr::filter(station_name != "Mile222")
# climatedf <- rbind(climatedf, Mile222)

#Fill in Date, JD and month-year values using lubridate
#make sure to double check date format after each step below

climatedf$date <- dplyr::case_when(
  !is.na(climatedf$date) ~ as.Date(lubridate::parse_date_time(
    stringr::str_squish(as.character(climatedf$date)),
    orders = c("mdY HM", "mdY", "Ymd HMS", "Ymd HM", "Ymd"),
    tz = "UTC"
  )),
  
  # Use year-month-day when available
  !is.na(climatedf$year) &
    !is.na(climatedf$month) &
    !is.na(climatedf$day) ~
    make_date(climatedf$year, climatedf$month, climatedf$day),
  
  # Otherwise fall back to JD (1-based)
  !is.na(climatedf$year) &
    !is.na(climatedf$JD) ~
    as.Date(climatedf$JD - 1, origin = paste0(climatedf$year, "-01-01")),
  
  TRUE ~ as.Date(NA)
)

climatedf <- climatedf %>%
  mutate(
    year  = suppressWarnings(as.integer(year)),
    month = suppressWarnings(as.integer(month)),
    day   = suppressWarnings(as.integer(day)),
    JD    = suppressWarnings(as.integer(JD)),
    
    date_raw = date,
    date = case_when(
      inherits(date, "Date") ~ date,
      TRUE ~ suppressWarnings(as.Date(parse_date_time(
        str_squish(as.character(date)),
        orders = c("Ymd", "Y-m-d", "Y/m/d", "mdY", "m/d/Y", "dmy", "d/m/Y"),
        tz = "UTC"
      )))
    ),
    
    # guardrails for impossible values commonly used as placeholders
    month = na_if(month, 0L),
    day   = na_if(day, 0L),
    JD    = na_if(JD, 0L),
    
    date = case_when(
      !is.na(date) ~ date,
      
      !is.na(year) & !is.na(month) & !is.na(day) &
        month >= 1L & month <= 12L &
        day   >= 1L & day   <= 31L ~ make_date(year, month, day),
      
      !is.na(year) & !is.na(JD) &
        JD >= 1L & JD <= if_else(leap_year(year), 366L, 365L) ~
        as.Date(JD - 1L, origin = paste0(year, "-01-01")),
      
      TRUE ~ as.Date(NA)
    )
  )

climatedf$month =dplyr::if_else(is.na(climatedf$month), lubridate::month(climatedf$date), climatedf$month) 
climatedf$day =dplyr::if_else(is.na(climatedf$day), lubridate::day(climatedf$date), as.integer(climatedf$day)) 
climatedf$JD = dplyr::if_else(is.na(climatedf$JD), lubridate::yday(climatedf$date), climatedf$JD) 
climatedf$year = dplyr::if_else(is.na(climatedf$year), lubridate::year(climatedf$date), climatedf$year) 
#climatedf$hour = dplyr::if_else(is.na(climatedf$hour), lubridate::hour(climatedf$date), as.integer(climatedf$hour)) # there should be no NA hour rows

#remove -6999 or -9999 values
NANvalues = c(-6999, 6999, -99999, "NAN", "Inf")
#climatedf[, 8:28][climatedf[, 8:28] %in% NANvalues] <- NA #takes too long to run
climatedf$t_air[climatedf$t_air %in% NANvalues] <- NA
climatedf$total_precip[climatedf$total_precip %in% NANvalues] <- NA
climatedf$t_water[climatedf$t_water %in% NANvalues] <- NA
climatedf$RH[climatedf$RH %in% NANvalues] <- NA
climatedf$wind_dir[climatedf$wind_dir %in% NANvalues] <- NA
climatedf$wind_sp[climatedf$wind_sp %in% NANvalues] <- NA
climatedf$rn[climatedf$rn %in% NANvalues] <- NA
climatedf$sw_in [climatedf$sw_in  %in% NANvalues] <- NA
climatedf$sr50 [climatedf$sr50 %in% NANvalues] <- NA
climatedf$t_soil_1 [climatedf$t_soil_1 %in% NANvalues] <- NA
climatedf$t_soil_2 [climatedf$t_soil_2 %in% NANvalues] <- NA
climatedf$t_soil_3 [climatedf$t_soil_3 %in% NANvalues] <- NA
climatedf$t_soil_4 [climatedf$t_soil_4 %in% NANvalues] <- NA
climatedf$water_depth[climatedf$water_depth %in% NANvalues] <- NA

# FTS_stns = c("Daring FTS",
#              "Harry FTS",
#              "Hoarfrost FTS",
#              "ITH FTS",
#              "Peel FTS",
#              "Winter Lake FTS")

#change FTS wind speed values from km hr-1 to m s-1 - note this is commented out as FTS data are brought into the database automatically
# climatedf$wind_sp = dplyr::if_else(!is.na(climatedf$wind_sp)& climatedf$station_name %in% FTS_stns,  climatedf$wind_sp/3.6, climatedf$wind_sp, missing = climatedf$wind_sp)

#change incoming radiation units - Tibbitt muskeg
climatedf$sw_in = dplyr::if_else(!is.na(climatedf$sw_in)&climatedf$station_name == "Tibbitt muskeg", climatedf$sw_in*1000, climatedf$sw_in)

#correct incoming radiation - Blueberry
#logger program multiplied mV by the sensor sensitivity (uV/W m-2) instead of 1000/sensitivity,
#so recorded values are too high by sensitivity^2/1000
#Sept 17, 2024 (10:00) program/sensor: sensitivity 68.09; Aug 2025 program/new sensor: sensitivity 69.8
#Note this correction below can be commented out once the new, fixed program "Blueberry2026.CR1" has been applied to the logger
climatedf$sw_in = dplyr::case_when(
  !is.na(climatedf$sw_in) & climatedf$station_name == "Blueberry" &
    climatedf$date >= as.Date("2025-08-01") ~ climatedf$sw_in / (69.8^2/1000),
  !is.na(climatedf$sw_in) & climatedf$station_name == "Blueberry" &
    climatedf$date >= as.Date("2024-09-17") ~ climatedf$sw_in / (68.09^2/1000),
  TRUE ~ climatedf$sw_in
)

# flag data & prep for climate db
# remove rows of NAs
climatedf <- climatedf %>%
  dplyr::filter((is.na(hour)==FALSE))

#hour correct is when its formatted like 1, 2, 3, not 100, 200, 300
#hour is incorrect for Tibbitt muskeg, BDL, and snare rapids

# remove trailing zeros from hour col, replace 24 with 0
hour_correct_stns <- c(
  "Harry","Pocket","Peel","Taglu","Tuktoyaktuk",
  "Dempster85",
  #"Dempster515",
  "Blueberry"
  #"LLM-N",
  #"LLM-E"
)

climatedf <- climatedf %>%
  mutate(
    station_name = str_squish(station_name),
    JD   = as.integer(JD),
    year = as.integer(year),
    hour = as.numeric(hour),
    hour_raw = hour
  ) %>%
  mutate(
    pocket_window =
      station_name == "Pocket" & !is.na(JD) & !is.na(year) &
      (
        (year == 2024 & JD >= 296) |
          (year == 2025 & (
            JD < 150 |
              (JD == 150 & !is.na(hour_raw) & hour_raw <= 1400)
          ))
      ),
    
    needs_div_100 =
      !(station_name %in% hour_correct_stns) | pocket_window,
    
    # Only divide when flagged AND hour is in 4-digit HHMM-style
    hour = if_else(needs_div_100 & !is.na(hour_raw) & hour_raw >= 100, hour_raw / 100, hour_raw),
    
    # Replace 24 with 0 after conversion
    hour = if_else(hour == 24, 0, hour)
  ) %>%
  select(-hour_raw)

#check hour conversion
#plot(climatedf$hour[climatedf$station_name == "BDL"])
#unique(climatedf$hour)

# Hour & Time columns - note that all hour values should be "padded" with an extra leading zero if the time is under 10:00
#climatedf$hour <- as.numeric(climatedf$hour)
climatedf$time <- lubridate::ymd_hms(base::paste(climatedf$date,base::sprintf("%02d:00:00",climatedf$hour)))
climatedf$hour <- base::sprintf("%02d:00", climatedf$hour)
climatedf <- climatedf[, !names(climatedf) %in% c("time")]

#Bring in QAQC df (for manual QAQC melting snow in tipping bucket)
# file_path <- "C:/Users/emma_riley/Documents/NT_Hydrology/Abiotic Monitoring/Data/cleaned"
# file_name <- "QAQCraindf.rds"
# QAQCraindf<- readRDS(paste0(file_path, "/", file_name ))

# note flag data function is in "dependencies_functions.R"
climatedf_flagged <- flag_data(climatedf) #added this dataframe to flag QAQC'd data

#merge with existing data - bring in existing data
# filepath <- "C:/Users/Emma_gregory/Documents/R_Scripts/Packages/nwtclimate/data"
# filepath_name1 = Quote('allstations_flagged.rds')
# data_ECC <- readRDS(paste0(filepath, "/", filepath_name1))

#make sure rain_check_flag values are applied to rain_flag
climatedf_flagged  <- climatedf_flagged  %>%
  dplyr::mutate(
    rain_flag = ifelse(
      !is.na(Rain_check_flag) & Rain_check_flag == "QC", 
      "QC_PSM", 
      rain_flag
    )
  )

#look at rain_check_flag values
# datacheck <- climatedf_flagged[climatedf_flagged$Rain_check_flag == "QC", ]
# datacheck <- datacheck %>%
#   dplyr::filter(!dplyr::if_all(everything(), is.na))

# remove Rain_check_flag and other columns if present
climatedf_flagged <- climatedf_flagged[, !names(climatedf_flagged) %in% c("Rain_check_flag", "date_raw", "pocket_window", "needs_div_100")]

# save file - to use in cd_prep.R for each year's upload
 newpath <- paste0(user, "/Documents/R_scripts/Packages/nwtclimate/data")
 saveRDS(climatedf_flagged, paste0(newpath, "/allstations_flagged_2026.rds"))

#rename hour column to time, then remove date and hour col
climatedf_flagged$cd_time <- climatedf_flagged$hour
climatedf_flagged <- climatedf_flagged[, !(names(climatedf_flagged) %in% c("date", "hour"))]

##Only use below when merging everything together as RDS 
#rename to match fieldnames
# climatedf_flagged <- climatedf_flagged %>%
#   dplyr::rename(
#     station_notes = notes,
#     rain_mm = total_precip,
#     cd_year = year,
#     cd_month = month,
#     cd_day = day,
#     cd_time = cd_time,
#     t_air_high_C = t_air,
#     t_air_low_C = t_air_2,
#     RH_high_per = RH,
#     RH_low_per = RH_2,
#     wind_sp_ms = wind_sp,
#     wind_dir_deg = wind_dir,
#     net_SW_Wm2 = net_SW,
#     net_LW_Wm2 = net_LW,
#     rn_Wm2 = rn,
#     sw_in_Wm2 = sw_in,
#     sr50_cm = sr50,
#     t_soil_1_C = t_soil_1,
#     t_soil_2_C = t_soil_2,
#     t_soil_3_C = t_soil_3,
#     t_soil_4_C = t_soil_4,
#     t_soil_5_C = t_soil_5,
#     t_soil_6_C = t_soil_6,
#     t_water_C = t_water,
#     t_water_2_C = t_water_2,
#     water_depth_m = water_depth,
#     water_depth_corr_m = water_depth_corr,
#     t_air_flag = t_air_flag, #changed this flag name
#     RH_flag = RH_flag, #changed this flag name
#     wind_sp_flag = wind_sp_flag,
#     wind_dir_flag = wind_dir_flag,
#     net_SW_flag = net_SW_flag,
#     net_LW_flag = net_LW_flag,
#     rn_flag = rn_flag,
#     sw_in_flag = sw_in_flag,
#     sr50_flag = sr50_flag
#   )
# 
# #add additional flag columns
# new_flag_cols <- c("t_soil_1_flag",
#                    "t_soil_2_flag",
#                    "t_soil_3_flag",
#                    "t_soil_4_flag",
#                    "t_soil_5_flag",
#                    "t_soil_6_flag",
#                    "t_air_2_flag",
#                    "RH_2_flag",
#                    "t_water_flag",
#                    "t_water_2_flag",
#                    "water_depth_flag",
#                    "water_depth_corr_flag")
# 
# climatedf_flagged[,new_flag_cols] <- NA
# 
# #merge with data_ECC
# 
# data_ECC$cd_time <- data_ECC$hour
# data_ECC <- data_ECC[, !(names(data_ECC) %in% c("date", "hour"))]
# 
# #rename to match fieldnames
# data_ECC <- data_ECC %>%
#   dplyr::rename(
#     station_notes = notes,
#     rain_mm = total_precip,
#     cd_year = year,
#     cd_month = month,
#     cd_day = day,
#     cd_time = cd_time,
#     t_air_high_C = t_air,
#     t_air_low_C = t_air_2,
#     RH_high_per = RH,
#     RH_low_per = RH_2,
#     wind_sp_ms = wind_sp,
#     wind_dir_deg = wind_dir,
#     net_SW_Wm2 = net_SW,
#     net_LW_Wm2 = net_LW,
#     rn_Wm2 = rn,
#     sw_in_Wm2 = sw_in,
#     sr50_cm = sr50,
#     t_soil_1_C = t_soil_1,
#     t_soil_2_C = t_soil_2,
#     t_soil_3_C = t_soil_3,
#     t_soil_4_C = t_soil_4,
#     t_soil_5_C = t_soil_5,
#     t_soil_6_C = t_soil_6,
#     t_water_C = t_water,
#     t_water_2_C = t_water_2,
#     water_depth_m = water_depth,
#     water_depth_corr_m = water_depth_corr,
#     t_air_flag = t_air_flag, #changed this flag name
#     RH_flag = RH_flag, #changed this flag name
#     wind_sp_flag = wind_sp_flag,
#     wind_dir_flag = wind_dir_flag,
#     net_SW_flag = net_SW_flag,
#     net_LW_flag = net_LW_flag,
#     rn_flag = rn_flag,
#     sw_in_flag = sw_in_flag,
#     sr50_flag = sr50_flag
#   )
# 
# #add additional flag columns
# new_flag_cols <- c("t_soil_1_flag",
#                    "t_soil_2_flag",
#                    "t_soil_3_flag",
#                    "t_soil_4_flag",
#                    "t_soil_5_flag",
#                    "t_soil_6_flag",
#                    "t_air_2_flag",
#                    "RH_2_flag",
#                    "t_water_flag",
#                    "t_water_2_flag",
#                    "water_depth_flag",
#                    "water_depth_corr_flag")
# 
# data_ECC[,new_flag_cols] <- NA
# 
# climatedf_2024 <- base::merge(data_ECC, climatedf_flagged, all = TRUE)
# 
# nwtclimate_datapath <- "C:/Users/emma_gregory/Documents/R_scripts/Packages/nwtclimate/data/"
# saveRDS(climatedf_2024, paste0(nwtclimate_datapath, "/allstations_flagged_raw.rds"))

