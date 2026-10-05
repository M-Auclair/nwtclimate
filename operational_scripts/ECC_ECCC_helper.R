# ECC and ECCC merge helper functions

# column names:
{
  # column names for hourly ECC data - ER: edited the names below as we have more flag columns now
  names_ECC_hr <- c(
    "Station_name",
    "Year",
    "JD",
    "Month",
    "Day",
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
    "Time",
    "T_soil_1_flag",
    "T_soil_2_flag",
    "T_soil_3_flag",
    "T_soil_4_flag",
    "T_soil_5_flag",
    "T_soil_6_flag",
    "T_air_low_flag",
    "RH_low_flag",
    "T_water_flag",
    "T_water_2_flag",
    "Water_depth_flag",
    "Water_depth_corr_flag",
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

# calc. daily values - removed Date from the group_by line below -ER
summarize_data <- function(input_data) {
  # Group by columns
  grouped_data <- dplyr::group_by(input_data, Station_name, Lat, Long, Elev, 
                                  Year, JD, Month, Day, merged_name)
  
  # Summarize data
  summarized_data <- dplyr::summarize(
    grouped_data,
    across(c(T_air, RH, Wind_sp, Wind_dir, Net_SW, Net_LW, Rn, SW_in, SR50,
             T_soil_1, T_soil_2, T_soil_3, T_soil_4, T_soil_5, T_soil_6,
             T_air_2, RH_2, T_water, T_water_2, Water_depth, Water_depth_corr),
           ~ ifelse(sum(!is.na(.)) >= 19, mean(., na.rm = TRUE), NA)), # 19 or more obs / day needed
    Total_precip = sum(Total_precip, na.rm = TRUE)
  )

  return(summarized_data)
}

#Append variables function
appendvar = function (var1, var2, var3){
  mtch = match(var1, var2)
  var4<-var3[match(var1, var2)]
  return(var4)
}

# flag data function

flag_data = function (df, QAQCraindf){
  
  df["uniqueID"]<-paste0(df$station_name, df$year, df$month, df$day)
  QAQCraindf["uniqueID"]<-paste0(QAQCraindf$station, QAQCraindf$Year, QAQCraindf$Month, QAQCraindf$Day)
  
  tips_removed<-appendvar(df$uniqueID, QAQCraindf$uniqueID, QAQCraindf$`Tips Removed (mm)`)
  df["tips_removed"]<-tips_removed
  
  df$rain_flag <- dplyr::if_else((df$t_air<(-0.1)&df$total_precip>0|df$total_precip<0), "QC", dplyr::if_else((df$tips_removed>0&df$total_precip>0), "QC_PSM", NA))
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
  
  drops <- c("rollmeanAT","first.above.zero", "year.start.JD", "uniqueID", "tips_removed")
  df <- df[ , !(names(df) %in% drops)]
  
  return(df)
  
}

#clean data function
clean_data = function (x){
cleaned_data <- x
cleaned_data$T_air[cleaned_data$T_air_flag == "QC"]<-NA
cleaned_data$RH[cleaned_data$RH_flag == "QC"]<-NA
cleaned_data$Total_precip[cleaned_data$Rain_flag == "QC"]<-0
cleaned_data$Wind_sp[cleaned_data$Wind_sp_flag== "QC"]<-NA
cleaned_data$Wind_dir[cleaned_data$Wind_dir_flag == "QC"]<-NA
cleaned_data$SW_in[cleaned_data$SW_in_flag == "QC"]<-NA

data_ECC_cleaned <<- cleaned_data

}


