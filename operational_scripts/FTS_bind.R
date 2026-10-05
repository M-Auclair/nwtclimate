# Appending new FTS data 

# FTS_filename2 is one of the following: 
# Daring FTS.xlsx 
# Harry FTS.xlsx 
# Hoarfrost FTS.xlsx 
# ITH FTS.xlsx 
# Peel FTS.xlsx 
# Winter Lake FTS.xlsx 

## to make a function
FTS_bind(
  FTS_station = c("Peel"), # Daring, Harry, Hoarfrost, ITH, Peel, or WinterLake
  FTS_filepath = "C:/Users/emma_riley/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/", # where existing data are saved
  FTS_filename1 = Quote("Peel FTS.csv"), # name of newly downloaded data to add to existing data
  FTS_filename2 = Quote("Peel FTS.xlsx"), # name of existing data 
  FTS_filepath_2 = "C:/Users/emma_riley/Documents/NT_Hydrology/Abiotic Monitoring/Data/FTSdata/", # where newly downloaded FTS files are saved
  save_files = T # if TRUE, files will be saved
)



# Note: all sites are handled the same except for Winter Lake, and Hoarfrost timezone
FTS_bind <- function(
    FTS_station,
    FTS_filepath,
    FTS_filename1,
    FTS_filename2,
    FTS_filepath_2,
    save_files 
)

{
if (FTS_station == "Daring" | FTS_station == "Harry" | FTS_station == "Hoarfrost" | 
    FTS_station == "ITH" | FTS_station == "Peel"){
  
  new_data <- read.csv(paste0(FTS_filepath_2, FTS_filename1))
  data <- readxl::read_excel(paste0(FTS_filepath, FTS_filename2), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  # 1. remove unanted columns
  new_data <- new_data[,c(1,3,9,11,12,13,14,16,19,23,25)] #add col 30 for ITH
  # column names
  {col_names <- c(
    "station_name",
    "date",
    "sr50",
    "t_soil_2",
    "t_soil_1",
    "t_air",
    "t_soil_3",
    "RH",
    "wind_sp",
    "rain",
    "wind_dir" 
  )
  }
  # new ymd columns
  colnames(new_data) <- col_names
  # convert wind speed from km/hr to m/s
  # wind_conversion <- 1/3.6
  # newdata$wind_sp <- newdata$wind_sp * wind_conversion # we already do this conversion in climate_bindfiles
  
  if(FTS_station == "Hoarfrost"){
    new_data$date <-  lubridate::as_datetime(new_data$date, tz = "Etc/GMT+8") # fix tz, Hoarfrost was 8hrs ahead
  } else{new_data$date <- lubridate::as_datetime(new_data$date, tz = "Etc/GMT+7") # to change to UTC-7
}
  
  #rename existing data columns
  data <- data %>%
    dplyr::rename(
      station_name = Station,
      year = Year,
      month = Month,
      day = Day,
      hour = Hour,
      date = Date,
      t_air = `Air Temperature`,
      RH = `Relative Humidity`,
      rain = Rainfall,
      wind_sp = `Wind Speed`,
      wind_dir = `Wind direction`,
      net_SW = `Net SW Radiation`,
      net_LW = `Net LW Radiation`,
      rn = `Net Radiation`,
      sw_in = `Incoming SW Radiation`,
      sr50 = `Snow depth`,
      t_soil_1 = TS_1,
      t_soil_2 = TS_2,
      t_soil_3 = TS_3,
      t_soil_4 = TS_4,
      t_soil_5 = TS_5,
      t_soil_6 = TS_6,
      t_air_2 = `Air Temperature_2`,
      RH_2 = `Relative Humidity_2`,
      t_water = Twater,
      t_water_2 = Twater_2,
      water_depth = DepthTW,
      t_air_flag = dataflag_AT,
      RH_flag = dataflag_RH,
      rain_flag = dataflag_Rn,
      wind_sp_flag = dataflag_WS,
      wind_dir_flag = dataflag_WD,
      sw_in_flag = dataflag_ISW,
      sr50_flag = dataflag_SD,
      rn_flag = dataflag_NR,
      net_SW_flag = dataflag_NSW,
      net_LW_flag = dataflag_NLW,
      station_notes = notes,
      water_depth_corr = WDep_corr
    )
  data[["water_depth_corr_flag"]] = NA
  
  
  new_data <- ymd_cols(new_data)
  # Create new columns
  # {new_data[["net_SW"]] <- NULL
  #   new_data[["net_LW"]] <- NULL
  #   new_data[["rn"]] <- NULL
  #   new_data[["sw_in"]] <- NULL
  #   new_data[["t_soil_4"]] <- NULL
  #   new_data[["t_soil_5"]] <- NULL
  #   new_data[["t_soil_6"]] <- NULL
  #   new_data[["t_air_2"]] <- NULL
  #   new_data[["RH_2"]] <- NULL
  #   new_data[["t_water"]] <- NULL
  #   new_data[["t_water_2"]] <- NULL
  #   new_data[["water_depth"]] <- NULL
  #   new_data[["water_depth_corr"]] <- NULL
  #   new_data[["t_air_flag"]] <- NULL
  #   new_data[["RH_flag"]] <- NULL
  #   new_data[["rain_flag"]] <- NULL
  #   new_data[["wind_sp_flag"]] <- NULL
  #   new_data[["wind_dir_flag"]] <- NULL
  #   new_data[["sw_in_flag"]] <- NULL
  #   new_data[["sr50_flag"]] <- NULL
  #   new_data[["rn_flag"]] <- NULL
  #   new_data[["net_SW_flag"]] <- NULL
  #   new_data[["net_LW_flag"]] <- NULL
  #   new_data[["station_notes"]] <- NULL
  #   new_data[["water_depth_corr_flag"]] <- NULL
  # }
  
  new_data <- new_data %>%
    tibble::add_column(net_SW = NA,
                       net_LW = NA, 
                       rn = NA,
                       sw_in = NA,
                       t_soil_4 = NA,
                       t_soil_5 = NA,
                       t_soil_6 = NA,
                       t_air_2 = NA,
                       RH_2 = NA,
                       t_water = NA,
                       t_water_2 = NA,
                       water_depth = NA,
                       t_air_flag = NA,
                       RH_flag = NA,
                       rain_flag = NA,
                       wind_sp_flag = NA,
                       wind_dir_flag = NA,
                       sw_in_flag = NA,
                       sr50_flag = NA,
                       rn_flag = NA,
                       net_SW_flag = NA,
                       net_LW_flag = NA,
                       station_notes = NA,
                       water_depth_corr_flag = NA)
  
  new_data$station_name <- paste0(FTS_station, " FTS")
  new_data <- dplyr::mutate_all(new_data, convert_classes)
  data <- dplyr::mutate_all(data, convert_classes)


  # bind dfs together
  new_data <-  rbind(data, new_data[, names(new_data)])
  # remove duplicates - CHANGED FEB 17 so ymd_cols can be applied afterwards again
  new_data <- remove_duplicates(new_data)
  # re-add ymd columns - not working post-bind for some reason
  new_data <- ymd_cols2(new_data)

  
  if(save_files) {
    saveRDS(new_data, file.path(FTS_filepath_2, paste0(FTS_station, " FTS.rds")))
  }
}
if(FTS_station == "Winter Lake" | FTS_station == "WinterLake"){
  new_data <- read.csv(paste0(FTS_filepath_2, FTS_filename1))
  data <- readxl::read_excel(paste0(FTS_filepath, FTS_filename2), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  new_data <- new_data[,c(1,3,9,11,12,13,14,16,19,24,25,30)]
  #column names
  {col_names <- c(
    "station_name",
    "date",
    "sr50",
    "t_soil_2",
    "t_soil_2",
    "t_air",
    "t_soil_3",
    "RH",
    "wind_sp",
    "sw_in",
    "wind_dir",
    "rain"
  )
  }
  # new ymd columns
  colnames(new_data) <- col_names
  new_data$Date <- lubridate::as_datetime(new_data$date, tz = "Etc/GMT+7") #fixed this to lower case d for Date
  new_data <- ymd_cols(new_data)
  # Create new columns # same as other stations except not creating SW_in for Winter Lake
  # {new_data[["net_SW"]] <- NULL
  #   new_data[["net_LW"]] <- NULL
  #   new_data[["rn"]] <- NULL
  #   new_data[["t_soil_4"]] <- NULL
  #   new_data[["t_soil_5"]] <- NULL
  #   new_data[["t_soil_6"]] <- NULL
  #   new_data[["t_air_2"]] <- NULL
  #   new_data[["RH_2"]] <- NULL
  #   new_data[["t_water"]] <- NULL
  #   new_data[["t_water_2"]] <- NULL
  #   new_data[["water_depth"]] <- NULL
  #   new_data[["t_air_flag"]] <- NULL
  #   new_data[["RH_flag"]] <- NULL
  #   new_data[["rain_flag"]] <- NULL
  #   new_data[["wind_sp_flag"]] <- NULL
  #   new_data[["wind_dir_flag"]] <- NULL
  #   new_data[["sw_in_flag"]] <- NULL
  #   new_data[["sr50_flag"]] <- NULL
  #   new_data[["rn_flag"]] <- NULL
  #   new_data[["net_SW_flag"]] <- NULL
  #   new_data[["net_LW_flag"]] <- NULL
  #   new_data[["station_notes"]] <- NULL
  #   new_data[["water_depth_corr_flag"]] <- NULL
  # }
  
  new_data <- new_data %>%
    tibble::add_column(net_SW = NA,
                       net_LW = NA, 
                       rn = NA,
                       t_soil_4 = NA,
                       t_soil_5 = NA,
                       t_soil_6 = NA,
                       t_air_2 = NA,
                       RH_2 = NA,
                       t_water = NA,
                       t_water_2 = NA,
                       water_depth = NA,
                       t_air_flag = NA,
                       RH_flag = NA,
                       rain_flag = NA,
                       wind_sp_flag = NA,
                       wind_dir_flag = NA,
                       sw_in_flag = NA,
                       sr50_flag = NA,
                       rn_flag = NA,
                       net_SW_flag = NA,
                       net_LW_flag = NA,
                       station_notes = NA,
                       water_depth_corr_flag = NA)
  
  new_data$Station <- paste0(FTS_station, " FTS")
  new_data <- dplyr::mutate_all(new_data, convert_classes)
  data <- dplyr::mutate_all(data, convert_classes)
  
  # bind dfs together
  new_data <-  rbind(data, new_data[, names(new_data)])
  # remove duplicates - CHANGED FEB 17 so ymd_cols can be applied afterwards again
  new_data <- remove_duplicates(new_data)
  # re-add ymd columns - not working post-bind for some reason
  new_data <- ymd_cols2(new_data)
  # convert wind speed from km/hr to m/s
  # wind_conversion <- 1/3.6
  # newdata$wind_sp <- newdata$wind_sp * wind_conversion
  
  if(save_files) {
    saveRDS(new_data, file.path(FTS_filepath_2, paste0(FTS_station, " FTS.rds")))
  }
  }
}
# Helper functions below -----------------------------

# after removing cols and renaming the ones remaining
ymd_cols <- function(data) {
  #data <- data[!duplicated(data$date), ] # note - only remove duplicate datetimes not just dates
  data$year <- format(as.Date(data$date), "%Y") # ER - i added in as.Date () to the Y M D JD H lines
  data$month <- format(as.Date(data$date), "%m")
  data$day <- format(as.Date(data$date), "%d")
  data$JD <- as.numeric(format(as.Date(data$date), "%j"))
  data$hour <- as.numeric(format(as.Date(data$date), "%H"))
  data$date <- format(as.Date(data$date), "%Y-%m-%d")
  data$date <- as.Date(data$date)
  #data <- subset(data, select = -DateTime)
  
  return(data)
}

# function to convert column classes -> applies to new AND old data
convert_classes <- function(x) {
  char_cols <- c("station_name", "t_air_flag", "RH_flag", "rain_flag", "wind_sp_flag",
                 "wind_dir_flag", "sw_in_flag", "sr50_flag", "rn_flag",
                 "net_SW_flag", "net_LW_flag", "notes")
  
  num_cols <- c("sr50", "t_soil_2", "t_soil_1", "t_air", "t_soil_3", "RH",
                "wind_sp", "rain", "wind_dir", "year", "month", 
                "net_SW", "net_LW", "rn", "sw_in",
                "t_soil_4", "t_soil_5", "t_soil_6", "t_air_2")
  
  char_cols <- intersect(char_cols, names(x))
  num_cols <- intersect(num_cols, names(x))
  
  x[char_cols] <- lapply(x[char_cols], as.character)
  x[num_cols] <- lapply(x[num_cols], as.numeric)
  
  return(x)
}

# function to remove any duplicated rows post-bind
remove_duplicates <- function(data){
  data$DateTime <- as.POSIXct(paste(data$date, data$hour), format = "%Y-%m-%d %H")
  data <- data[!duplicated(data$DateTime), ]
  #data <- subset(data, select = -DateTime)
}

ymd_cols2 <- function(data) {
  data$year <- format(data$date, "%Y")
  data$month <- format(data$date, "%m")
  data$day <- format(data$date, "%d")
  data$JD <- as.numeric(format(data$date, "%j"))
  data$hour <- as.numeric(format(data$DateTime, "%H"))
  data$date <- format(data$date, "%Y-%m-%d")
  data$date <- as.Date(data$date)

  
  return(data)
}


