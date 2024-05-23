##############################################################################################

# filepath = "C:/Users/Ryan_Connon/Documents/R_Scripts/Packages/nwtclimate/data/"
# input_file = "ECCC_Climate_Data_Updated"
# output_file = "ECCC_Climate_Data_Updated"
# extension = ".rds"

# Function to update climate data

update <- function(
    filepath,
    input_file,
    output_file
)
  
{
  data <- readRDS(paste0(filepath, input_file, extension))
  data$date <- lubridate::ymd(as.character(data$date))

  data[,12] <- as.numeric(unlist(data[,12]))
  data[,13] <- as.numeric(unlist(data[,13]))
  data[,14] <- as.numeric(unlist(data[,14]))
  
  if(max(data$date) < Sys.Date() - 1) {
  
  # Create a list of all data within the last year (to only download recent data)
  data_1 <- data %>%
    dplyr::group_by(station_name) %>%
    dplyr::filter(dplyr::between(date, Sys.Date() - 365, Sys.Date())) %>%
    dplyr::filter(!is.na(mean_temp))
  
  # Extract station names from stations with recent data and sort alphabetically
  site = as.list(sort(unique(data_1$station_id))) 
  
  df <- NULL
  
  for (i in seq_along(site)) {

    prev_max_date <- data %>%
      dplyr::filter(station_id == site[i]) %>%
      dplyr::slice(which.max(date))
    
    station_id <- prev_max_date$station_id
    last_date <- min(prev_max_date$date)
    data_recent <- weathercan::weather_dl(station_ids = station_id, start = last_date + 1, interval = "day")

    df <- dplyr::bind_rows(df, data_recent)
    if(nrow(data_recent) > 0) {
    print(paste0(data_recent[1,1], " (ID ", data_recent[1,2], ") has been updated"))
    }
    
  }
  
  df[,12] <- as.numeric(unlist(df[,12]))
  df[,13] <- as.numeric(unlist(df[,13]))
  df[,14] <- as.numeric(unlist(df[,14]))
  
  df <- dplyr::bind_rows(data, df) %>%
    dplyr::arrange(station_name)
  
  print(paste0("Congratulations! Climate datafile has been succesfully updated to ", max(df$date)))
  
  } else {
    
    df <- data %>%
      dplyr::arrange(station_name)
    
    print("Climate dataset is already up-to-date!")
    
  }

  saveRDS(df, file = paste0(filepath, output_file, extension))
  
}
