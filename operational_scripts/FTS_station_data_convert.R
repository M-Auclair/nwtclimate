#Formatting FTS data script 
#for re-formating downloaded FTS data for upload to climate database
#only needed if there is a telemetry failure but station data are unaffected

library(lubridate)

#bring in dependencies functions
source("C:/Users/Emma_Gregory/Documents/R_Scripts/Packages/nwtclimate/R/dependencies_functions.R")

#change the path below for the file you are using 
climate_data <- read.csv("C:/Users/Emma_Gregory/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/FTS/Daring Lake-2026-5-4-8-58-45.csv")
Station_name <- "Daring FTS"

#rename FTS data columns
#this function needs to be changed for each station, as some collect soil temperature data and some do not
#verify that columns align with the object "colnames_FTS" below

convert_FTS_colnames <- function(x) {
  
  colnames_FTS <- c(
    "Date",
    "Tmax24",
    "Tmin24",
    "RhMax24",
    "RhMin24",
    "Rn24",
    "t_air",
    "RH",
    "wind_sp_na",
    "Mx_Spd_na",
    "Mx_Dir",
    "wind_dir",
    "total_precip",
    "TMax",
    "TMin",
    "RhMax",
    "RhMin",
    "Vb",
    "sw_in",
    "RnTotal",
    "sr50",
    "t_soil_1",
    "t_soil_2",
    "t_soil_3",
    "wind_sp",
    "Mx_Spd_na",
    "baro_pressure_mbar"
  )
  colnames(x) <- colnames_FTS
  return(x)
}

#select only required columns
climate_data <- convert_FTS_colnames(climate_data) %>%
  dplyr::select("Date", 
                "t_air",
                "RH",
                "wind_dir",
                "total_precip",
                "sw_in",
                "t_soil_1",
                "t_soil_2",
                "t_soil_3",
                "wind_sp",
                "baro_pressure_mbar")

#add additional flag columns
new_flag_cols <- c("t_air_flag",
                   "RH_flag",
                   "rain_flag",
                   "wind_sp_flag",
                   "wind_dir_flag",
                   "net_SW_flag",
                   "net_LW_flag",
                   "rn_flag",
                   "sw_in_flag",
                   "sr50_flag",
                   "t_soil_1_flag",
                   "t_soil_2_flag",
                   "t_soil_3_flag",
                   "t_soil_4_flag",
                   "t_soil_5_flag",
                   "t_soil_6_flag",
                   "t_air_2_flag",
                   "RH_2_flag",
                   "t_water_flag",
                   "t_water_2_flag",
                   "water_depth_flag",
                   "water_depth_corr_flag",
                   "baro_pressure_flag")

new_cols <- c("station_name",
              "year",
              "JD",
              "month",
              "day",
              "hour")

climate_data[,new_flag_cols] <- NA
climate_data[,new_cols] <- NA
climate_data[,"station_name"] <- paste0(Station_name)

#Date-time columns

climate_data$Date <- lubridate::mdy_hm(climate_data$Date)
climate_data$time <- dplyr::if_else(is.na(climate_data$hour), lubridate:::hour(climate_data$Date), climate_data$hour) 
climate_data$time <- base::sprintf("%02d:00", climate_data$time)
climate_data$hour <- climate_data$time

climate_data$month =dplyr::if_else(is.na(climate_data$month), lubridate::month(climate_data$Date), climate_data$month) 
climate_data$day =dplyr::if_else(is.na(climate_data$day), lubridate::day(climate_data$Date), as.integer(climate_data$day)) 
climate_data$JD = dplyr::if_else(is.na(climate_data$JD), lubridate::yday(climate_data$Date), climate_data$JD) 
climate_data$year = dplyr::if_else(is.na(climate_data$year), lubridate::year(climate_data$Date), climate_data$year) 

#flagging and cleaning

NANvalues = c(-6999, 6999, -99999, "NULL")
climate_data$t_air[climate_data$t_air %in% NANvalues] <- NA
climate_data$total_precip[climate_data$total_precip %in% NANvalues] <- NA
climate_data$t_water[climate_data$t_water %in% NANvalues] <- NA
climate_data$RH[climate_data$RH %in% NANvalues] <- NA
climate_data$wind_dir[climate_data$wind_dir %in% NANvalues] <- NA
climate_data$wind_sp[climate_data$wind_sp %in% NANvalues] <- NA
climate_data$rn[climate_data$rn %in% NANvalues] <- NA
climate_data$sw_in [climate_data$sw_in  %in% NANvalues] <- NA
climate_data$sr50 [climate_data$sr50 %in% NANvalues] <- NA
climate_data$t_soil_1 [climate_data$t_soil_1 %in% NANvalues] <- NA
climate_data$t_soil_2 [climate_data$t_soil_2 %in% NANvalues] <- NA
climate_data$t_soil_3 [climate_data$t_soil_3 %in% NANvalues] <- NA
climate_data$t_soil_4 [climate_data$t_soil_4 %in% NANvalues] <- NA
climate_data$t_soil_5 [climate_data$t_soil_5 %in% NANvalues] <- NA
climate_data$t_soil_6 [climate_data$t_soil_6 %in% NANvalues] <- NA
climate_data$water_depth[climate_data$water_depth %in% NANvalues] <- NA

#change FTS wind speed values from km hr-1 to m s-1
climate_data$wind_sp = climate_data$wind_sp/3.6

#flag data
climate_data_flagged <- flag_data(climate_data)

#make sure rain check flags are transitioned to "rain_flag"
climate_data_flagged <- climate_data_flagged %>%
  dplyr::mutate(rain_flag = ifelse(!is.na(Rain_check_flag) & Rain_check_flag == "QC", "QC_PSM", rain_flag))

#rename columns

climate_data_flagged <- climate_data_flagged %>%
  dplyr::rename("t_air_high_C" = "t_air",
                "RH_high_per" = "RH",
                "wind_dir_deg"="wind_dir",
                "wind_sp_ms"="wind_sp",
                "sw_in_Wm2"="sw_in",
                "rain_mm"="total_precip",
                "t_soil_1_C"="t_soil_1",
                "t_soil_2_C"="t_soil_2",
                "t_soil_3_C"="t_soil_3",
                "t_soil_4_C"="t_soil_4",
                "t_soil_5_C"="t_soil_5",
                "t_soil_6_C"="t_soil_6",
                "cd_year"="year",
                "cd_day"="day",
                "cd_time"="time",
                "cd_hour"="hour",
                "cd_month"="month")

#optional filter for specific data upload

# climate_data_flagged <- climate_data_flagged %>%
#   dplyr::filter(cd_year==2025&cd_month==12&cd_day>11|cd_year==2026)

#define columns
main_columns <- c(
  "t_air_high_C",
  "RH_high_per",
  "rain_mm",
  "wind_sp_ms",
  "wind_dir_deg",
  "net_SW_Wm2",
  "net_LW_Wm2",
  "rn_Wm2",
  "sw_in_Wm2",
  "sr50_cm",
  "t_soil_1_C",
  "t_soil_2_C",
  "t_soil_3_C",
  "t_soil_4_C",
  "t_soil_5_C",
  "t_soil_6_C",
  "t_air_low_C",
  "RH_low_per",
  "t_water_C",
  "t_water_2_C",
  "water_depth_m",
  "water_depth_corr_m",
  "baro_pressure_mbar"
)

# variable flags
flag_col <- c(
  "t_air_flag",
  "RH_flag",
  "rain_flag",
  "wind_sp_flag",
  "wind_dir_flag",
  "net_SW_flag",
  "net_LW_flag",
  "rn_flag",
  "sw_in_flag",
  "sr50_flag",
  "t_soil_1_flag",
  "t_soil_2_flag",
  "t_soil_3_flag",
  "t_soil_4_flag",
  "t_soil_5_flag",
  "t_soil_6_flag",
  "t_air_2_flag",
  "RH_2_flag",
  "t_water_flag",
  "t_water_2_flag",
  "water_depth_flag",
  "water_depth_corr_flag",
  "baro_pressure_flag"
)

additional_columns <- c("station_name",
                        "cd_year",
                        "JD",
                        "cd_month",
                        "cd_day",
                        "cd_time")

#split data into long data format
climate_data_list <- split_df(
  df = climate_data_flagged,
  additional_columns = additional_columns,
  main_columns = main_columns,
  flag_col = flag_col
)

# remove all rows where variable is NA in each df
climatedf_list <- climate_data_list
climatedf_list <- remove_na_rows(climatedf_list)

#
# save as separate csv's
for (df_name in names(climatedf_list)) {
  df <- climatedf_list[[df_name]]
  filename <- paste0(savepath, df_name, ".csv")
  write.csv(df, file = filename, row.names = FALSE)
  cat("df", df_name, "saved as", filename, "\n")
}

