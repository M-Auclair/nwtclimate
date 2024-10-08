# Climate Dependency Functions

###################################################################################################

# Define '%>%' operator in current environment

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


#####################################
#' Amend coordinates of select site
#'
#' @param df dataframe
#' @param site Site of coordinates to change (in quotations)
#' @param lat New latitude value of the site
#' @param lon New longitude value of the site
#' @return dataframe with updated coordinates for input site
#' @examples
#' df <- replace_coordinates(df, "Fort McPherson", 67.45, -134.88)
#' @export
#'


replace_coordinates <- function(df, site, lat, lon) {
  # Check if the site exists in the merged_name column
  if (site %in% df$merged_name) {
    # Find rows where merged_name matches the specified site
    site_rows <- df$merged_name == site
    # Replace lat and lon values for the specified site
    df$lat[site_rows] <- lat
    df$lon[site_rows] <- lon
  } else {
    warning(paste(site, " not found in the data frame. Check spelling, and use the name of the site instead of the station name"))
  }
  return(df)
}


###################################################################################################
#' Import data for select sites
#'
#' @param site A character vector of site names
#' @param data_path A character string specifying path to directory
#'
#' @return A data frame containing the imported data
#'
#' @examples
#' # Set data_path directory
#' data_path <- "C:/Users/path/to/your/data"
#' # Define sites
#' site <- c("Yellowknife","Scotty Creek","Deline","Blueberry")
#' # Import data
#' data <- import_site_data(site, data_path)``
#' @export

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


###################################################################################################

#' Append variables function
#' appends variable in df1 with variable from df2 by aligning common group column that exists in both dfs
#' @param var1 variable from df1 to align with df2
#' @param var2 variable from df2 to align with df1
#' @param var3 value column from df2 to append to df1
#'
#' @return dataframe with appended variable
#'
#' @examples
#' lat = appendvar(df$site, sites$site, sites$latitude)
#' @export



appendvar = function (var1, var2, var3){
  mtch = match(var1, var2)
  var4<-var3[match(var1, var2)]
  return(var4)
}


#' Data flagging function
#' @param df raw climate dataframe to be flagged
#' @return flagged cliamte dataframe
#'
#' @examples
#' df <- flag_data(df)
#'
#' @export



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


#' Data cleaning function
#' Assigns NA to values based on data flags
#' @param df flagged climate dataframe to be cleaned
#' @return cleaned climate dataframe
#'
#' @examples
#' df <- clean_data(df)
#' @export

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

#' Summarize data into daily values
#' This function takes hourly or half-hourly data and summarizes it into daily data
#' @param df Dataframe to be summarized
#'
#' @return Dataframe of daily data
#' @note This function relies on existence of following columns: station_name, merged_name, lat, lon, elev, year, JD, month, day, date
#' @examples
#' df <- summarize_data(df)
#' @export

# calc. daily values
# note - Wrigley is modified bc it is not calculating hourly data
summarize_data <- function(df) {
  grouped_data <- dplyr::group_by(df, station_name, merged_name, lat, lon, elev,
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

#############################################
#' Convert column classes of daily data
#' This function takes daily data and converts columns to appropriate class (numeric or character)
#' @param df Dataframe
#' @return Dataframe with appropriate column classes
#' @note This function is meant to be applied to flagged and/or data
#' @examples
#' df <- convert_classes_daily(df)
#' @export

# convert classes of daily values/data so they can be binding in merger
convert_classes_daily <- function(df) {

  char_cols <- c("station_name","dataflag_AT","dataflag_RH","dataflag_Rn","dataflag_WS","dataflag_WD",
                 "dataflag_ISW","dataflag_SD","dataflag_NR","dataflag_NSW","dataflag_NLW","merged_name")

  num_cols <- c("year","JD","month","day","t_air","RH","wind_sp","wind_dir","new_SW","net_LW","rn","sw_in",
                "sr50","t_soil_1","t_soil_2","t_soil_3","t_soil_4","t_soil_5","t_soil_6","t_air_2","RH_2","t_water",
                "t_water_2","water_depth","water_depth_corr","total_precip")

  char_cols <- intersect(char_cols, names(df))
  num_cols <- intersect(num_cols, names(df))

  df[char_cols] <- lapply(df[char_cols], as.character)
  df[num_cols] <- lapply(df[num_cols], as.numeric)

  return(df)
}


#######################################################
#' Remove duplicate rows of data
#' This function creates a datetime column and removes all duplicate rows based on datetime, then removes the datetime column
#' @param df Dataframe
#' @return Dataframe
#' @note To be used in cases where each site is its own df, after binding new data to existing data
#' @examples
#' df <- remove_duplicates(df)
#' @export

remove_duplicates <- function(data){
  data$DateTime <- as.POSIXct(paste(data$date, data$hour), format = "%Y-%m-%d %H")
  data <- data[!duplicated(data$DateTime), ]
  data <- subset(data, select = -DateTime)
}

###################################
#' Creates and/or formats date, year, month, day, JD, and hour columns
#' @param df Dataframe
#' @return Dataframe, with ymd columns
#' @examples
#' df <- ymd_cols(df)
#' @export
#'
ymd_cols <- function(data) {
data$date <- as.Date(new_data$date, format = "%Y-%m-%d")
data$year <- format(data$date, "%Y")
data$month <- format(data$date, "%m")
data$day <- format(data$date, "%d")
data$JD <- as.numeric(format(data$date, "%j"))
data$hour <- as.numeric(format(data$date, "%H"))
return(data)
}


###################################
#' Convert column classes of a df
#'
#' @param df variable from df1 to align with df2
#'
#' @return dataframe with column classes
#' @note columns and classes are pre-assigned within the function
#' @note function used in FTS_bind, climate_bindfiles
#' @examples
#' df <- convert_classes(df)
#' @export
#'
convert_classes <- function(x) {
  char_cols <- c("station_name",
                 "station_notes",
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
                 "station_notes",
                 "water_depth_corr",
                 "water_depth_corr_flag")

  num_cols <- c("year","JD","month","day","hour","t_air",
                "RH",
                "total_precip",
                "wind_sp",
                "wind_dir",
                "net_SW",
                "net_LW",
                "rn",
                "sw_in",
                "sr50",
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
                "water_depth_corr",
                "lat","lon","elev")

  char_cols <- intersect(char_cols, names(x))
  num_cols <- intersect(num_cols, names(x))

  x[char_cols] <- lapply(x[char_cols], as.character)
  x[num_cols] <- lapply(x[num_cols], as.numeric)

  return(x)
}


###################################################################################################


#' Function to determine parameter name based on different inputs
#' @param parameter the input parameter
#' @return standardized parameter name

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

###############################################

#' Separate dfs by variable
#' Creates a df for each variable, its flag, and additional columns defined in cd_prep script
#' @param df dataframe: climatedf
#' @param additional_columns station_name, date, time, and other columns to keep in all dfs split by variable
#' @param main_columns variable columns
#' @param flag_columns flag columns corresponding to each variable
#'
#' @return list of split dataframes - one per variable
#' @note main_columns, flag_col, and additional_columns need to be specified
#' @examples
#'   climatedf_list <- split_df(
#'    df = climatedf,
#'    additional_columns = additional_columns,
#'    main_columns = main_columns,
#'    flag_col = flag_col
#'  )
#' @export



split_df <- function(df, additional_columns, main_columns, flag_columns) {
  dfs <- list()

  for (i in seq_along(main_columns)) {
    main_col <- main_columns[i]
    flag_col <- flag_columns[i]

    if (main_col %in% names(df) & flag_col %in% names(df)) {
      #df name will be same as main_columns name - important for removing NAs
      #df_name <- sub("_flag$","", flag_col)
      new_df <- df[c(additional_columns, main_col, flag_col)]
      dfs[[paste(main_col)]] <- new_df
    } else {
      warning("Main column or flag column missing in dataframe.")
    }
  }

  return(dfs)
}




#############################
#' Remove rows where variable is NA
#' Note: for each df in list of variable dfs in preparation for load into DB
#' @param list of dataframes for each variable
#'
#' @examples
#' climatedf_list <- remove_na_rows(climatedf_list)
#' # To extract df from list:
#' rain <- climatedf_list[['rain_mm']]
#' @return list of dataframes with NA values removed
#' @export


remove_na_rows <- function(cliamtedf_list) {
  for(df_name in names(climatedf_list)){
    df <- climatedf_list[[df_name]]
    col_name <- df_name

    df <- df[!is.na(df[[col_name]]),] # remove NAs where colname = dfname

    climatedf_list[[df_name]] <- df # update list
  }
  return(climatedf_list)
}


