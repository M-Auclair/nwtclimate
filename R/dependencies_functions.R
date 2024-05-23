# Climate Dependency Functions

###################################################################################################

# Define `%>%` operator in current environment

`%>%` <- magrittr::`%>%`

###################################################################################################

# Manually call tidyhydat.ws into active library
library(tidyhydat.ws)

###################################################################################################

# Define function to calculate water year

wtr_yr <- function(dates, 
                   start.month) 
{
  dates.posix = as.POSIXlt(dates)
  offset = ifelse(dates.posix$mon >= start.month - 1, 1, 0)
  adj.year = dates.posix$year + 1900 + offset
  adj.year
}


###################################################################################################
# Function to bring in data based on site(s) input
import_site_data <- function(site, data_path = "./") {
  data_list <- list()
  for (i in site) {
    
    filename <- paste0(data_path, i, ".rds")
    
    data <- readRDS(filename)
    
    data_list[[i]] <- data 
    
    data <- dplyr::bind_rows(data_list)
  }
  return(data)
}

# data <- import_site_files(site, data_path) # to apply function

###################################################################################################

# Functions for adding lat/lon, flagging, cleaning, and summarizing data (FTS, ECC, & Scotty)

# Append variables function (pulling lat/lon/elev from another input csv & adding to dfs)
appendvar = function (var1, var2, var3){
  mtch = match(var1, var2)
  var4<-var3[match(var1, var2)]
  return(var4)
}

# Data flagging function
flag_data = function (df){
  
  df$rain_flag <- dplyr::if_else((df$t_air<(-0.1)&df$total_precip>0|df$total_precip<0), "QC", NA)
  df$wind_sp_flag <- dplyr::if_else((df$wind_sp < 0|(zoo::rollapply(df$wind_sp, width = 5, FUN = sd, fill = NA)<0.001)), "QC", NA)
  df$wind_dir_flag <- dplyr::if_else((df$wind_dir < 0|df$wind_dir > 360|df$wind_sp <=0 | (zoo::rollapply(df$wind_dir, width = 5, FUN = sd, fill = NA)<0.1)), "QC", NA)
  df$RH_flag <- dplyr::if_else((df$RH<0|df$RH>100), "QC", NA)
  df$t_air_flag <- dplyr::if_else((df$t_air<(-60)|df$t_air>50), "QC", NA)  
  df$sw_in_flag <- dplyr::if_else((df$sw_in<0), "QC", NA)
  
  df <- dplyr::arrange(df, station_name, year, JD)
  df  <- dplyr::group_by(df, station_name, year)
  df  <- dplyr::mutate(df, year.start.JD = min(JD, na.rm=TRUE))
  df  <- dplyr::mutate(df, first.above.zero = head(c(JD[c(t_air[-length(t_air)] > 0, TRUE)]), 1))
  df$first.above.zero[df$first.above.zero == df$year.start.JD] <- NA
  
  df  <- dplyr::arrange(.data = df , station_name, year, JD) 
  df  <- dplyr::group_by(.data = df , station_name, year)
  df  <- dplyr::mutate(.data = df , rollmeanAT = zoo::rollmean(t_air, k=720, fill=NA))
  df$Rain_check_flag <- dplyr::if_else((df$total_precip>0& df$first.above.zero==df$JD)| (df$total_precip>0& df$first.above.zero+1==df$JD)| (df$t_air>0&df$rollmeanAT<0&df$total_precip>0), "QC", NA) 
  
  drops <- c("rollmeanAT","first.above.zero", "year.start.JD")
  df <- df[ , !(names(df) %in% drops)]
  
return(df)
  
}

# Data cleaning function 
# note MA changed ER's function to also remove flag columns (minus exceptions)
clean_data = function (df){
  df$t_air[df$t_air_flag == "QC"]<-NA
  df$RH[df$RH_flag == "QC"]<-NA
  df$total_precip[df$rain_flag == "QC"]<-NA
  df$wind_sp[df$wind_sp_flag== "QC"]<-NA
  df$wind_dir[df$wind_dir_flag == "QC"]<-NA
  df$sw_in[df$sw_in_flag == "QC"]<-NA
  
  flag_columns <- grep("_flag$", names(df))
  keepflags <- c("rain_flag","Rain_check_flag")
  remove_columns <- flag_columns[!(names(df)[flag_columns] %in% keepflags)]
  
  df <- df[, -remove_columns, drop=FALSE]
  #df <- df[, !(names(df) %in% remove_columns)]
  
  return(df)
  
}

# calc. daily values 
# note - Wrigley is modified bc it is not calculating hourly data
summarize_data <- function(input_data) {
  # Group by columns
  grouped_data <- dplyr::group_by(input_data, station_name, merged_name, lat, lon, elev, 
                                  year, JD, month, day, date)
  
  summarized_data <- dplyr::summarize(
    grouped_data,
    dplyr::across(
      c(t_air, RH, wind_sp, wind_dir, net_SW, net_LW, rn, sw_in, sr50,
        t_soil_1, t_soil_2, t_soil_3, t_soil_4, t_soil_5, t_soil_6,
        t_air_2, RH_2, t_water, t_water_2, water_depth, water_depth_corr),
      ~ if("Wrigley" %in% merged_name) {
        if(sum(!is.na(.)) >= 1) mean(., na.rm = TRUE) else NA
      } else {
        if(sum(!is.na(.)) >= 19) mean(., na.rm = TRUE) else NA
      }
    ),
    total_precip = ifelse(sum(!is.na(total_precip)) >= 19, sum(total_precip, na.rm = TRUE), NA)
  )
  
  return(summarized_data)
}

# convert classes of daily values/data so they can be binding in merger 
convert_classes_daily <- function(x) {
  
  char_cols <- c("station_name","dataflag_AT","dataflag_RH","dataflag_Rn","dataflag_WS","dataflag_WD",
                 "dataflag_ISW","dataflag_SD","dataflag_NR","dataflag_NSW","dataflag_NLW","merged_name")
  
  num_cols <- c("year","JD","month","day","t_air","RH","wind_sp","wind_dir","new_SW","net_LW","rn","sw_in",
                "sr50","t_soil_1","t_soil_2","t_soil_3","t_soil_4","t_soil_5","t_soil_6","t_air_2","RH_2","t_water",
                "t_water_2","water_depth","water_depth_corr","total_precip")   
  
  char_cols <- intersect(char_cols, names(x))
  num_cols <- intersect(num_cols, names(x))
  
  x[char_cols] <- lapply(x[char_cols], as.character)
  x[num_cols] <- lapply(x[num_cols], as.numeric)
  
  return(x)
}




###################################################################################################

# Function to determine parameter name based on different inputs

parameter_name <- function(parameter) {
  
  if(grepl(paste0("(?i)", parameter), "precipitation") == T |
     grepl(paste0("(?i)", parameter), "total_precip") == T) {
    parameter <- "total_precip"
  } else if (grepl(paste0("(?i)", parameter), "rainfall") == T |
             grepl(paste0("(?i)", parameter), "total_rain") == T) {
    parameter <- "rain"
  } else if (grepl(paste0("(?i)", parameter), "snowfall") == T | 
             grepl(paste0("(?i)", parameter), "snow depth") == T |
             grepl(paste0("(?i)", parameter), "total_snow") == T) {
    parameter <- "total_snow"
  } else if (grepl(paste0("(?i)", parameter), "swe") == T | 
             grepl(paste0("(?i)", parameter), "snow water equivalent") == T) {
    parameter <- "SWE"
  } else if (grepl(paste0("(?i)", parameter), "mean temperature") == T |
             grepl(paste0("(?i)", parameter), "mean_temp") == T) {
    parameter <- "mean_temp"
  } else if (grepl(paste0("(?i)", parameter), "minimum temperature") == T |
             grepl(paste0("(?i)", parameter), "min_temp") == T) {
    parameter <- "min_temp"
  } else if (grepl(paste0("(?i)", parameter), "maximum temperature") == T |
             grepl(paste0("(?i)", parameter), "max_temp") == T) {
    parameter <- "max_temp"
  }else if (grepl(paste0("(?i)", parameter), "RH") == T |
            grepl(paste0("(?i)", parameter), "relative humidity") == T) {
    parameter <- "RH"
  }else if (grepl(paste0("(?i)", parameter), "wind speed") == T |
            grepl(paste0("(?i)", parameter), "wind_speed") == T) {
    parameter <- "wind_speed"
  } else if (grepl(paste0("(?i)", parameter), "wind direction") == T |
             grepl(paste0("(?i)", parameter), "wind_dir") == T) {
    parameter <- "wind_dir"
  } else if (grepl(paste0("(?i)", parameter), "net shortwave radiation") == T |
            grepl(paste0("(?i)", parameter), "net_sw") == T) {
    parameter <- "net_SW"
  } else if (grepl(paste0("(?i)", parameter), "net longwave radiation") == T |
             grepl(paste0("(?i)", parameter), "net_lw") == T) {
    parameter <- "net_LW"
  } else if (grepl(paste0("(?i)", parameter), "net radiation") == T |
             grepl(paste0("(?i)", parameter), "nr") == T |
             grepl(paste0("(?i)", parameter), "rn") == T) {
    parameter <- "net_rad"
  } else if (grepl(paste0("(?i)", parameter), "incoming shortwave radiation") == T |
             grepl(paste0("(?i)", parameter), "sw_in") == T) {
    parameter <- "sw_in"
  }
  else {
    stop ("Invalid parameter entered")
  }
  
  parameter
  
}



################################################################################################### --------
# ECC_ECCC merge helper functions

# column names:
{
  # column names for hourly ECC data
  names_ECC_hr <- c(
    "Station_name",
    "Year",
    "JD",
    "Month",
    "Day",
    "Hour",
    "Date",
    "T_air",
    "RH",
    "Total_precip",
    "Wind_sp",
    "Wind_dir",
    "Net_SW",
    "Net_LW",
    "Rn",
    "SW_in",
    "SR50",
    "T_soil_1",
    "T_soil_2",
    "T_soil_3",
    "T_soil_4",
    "T_soil_5",
    "T_soil_6",
    "T_air_2",
    "RH_2",
    "T_water",
    "T_water_2",
    "Water_depth",
    "T_air_flag",
    "RH_flag",
    "Rain_flag",
    "Wind_sp_flag",
    "Wind_dir_flag",
    "SW_in_flag",
    "SR50_flag",
    "Rn_flag",
    "Net_SW_flag",
    "Net_LW_flag",
    "Notes",
    "Water_depth_corr",
    "Lat",
    "Long",
    "Elev"
  )
  
  
  
  # column names for daily ECCC data
  names_ECCC_daily <- c(
    "Station_name",
    "Station_id",
    "Station_operator",
    "Prov",
    "Lat",
    "Long",
    "Elev",
    "Climate_id",
    "WMO_id",
    "TC_id",
    "Date",
    "Year",
    "Month",
    "Day",
    "Qual",
    "Cool_deg_days",
    "Cool_deg_days_flag",
    "Dir_max_gust",
    "Dir_max_gust_flag",
    "Heat_deg_days",
    "Heat_deg_days_flag",
    "T_air_max",
    "T_air_max_flag",
    "T_air",
    "T_air_flag",
    "T_air_min",
    "T_air_min_flag",
    "Snow_grnd",
    "Snow_grnd_flag",
    "Spd_max_gust",
    "Spd_max_gust_flag",
    "Total_precip",
    "Total_precip_flag",
    "Total_rain",
    "Total_rain_flag",
    "Total_snow",
    "Total_snow_flag",
    "merged_name"
  )
}





