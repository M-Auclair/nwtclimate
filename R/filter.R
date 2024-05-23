##############################################################################################

# Dependency data
# locations
# station_ids

##############################################################################################

# Arguments for testing:

# filepath = "C:/Users/Ryan_Connon/Documents/R_Scripts/Packages/nwtclimate/data/"
# input_file = "ECCC_Climate_Data_Updated"
# output_file = "ECCC_Climate_Data_Filtered"
# extension = ".rds"

##############################################################################################

# Function to filter data

filter <- function(
    filepath,
    input_file,
    output_file
  )
  
{
  
  # Read in current data
  
  data <- readRDS(paste0(filepath, input_file, extension))
  
  # Create list of data to filter
  
  # Remove bad quality snow data at 51058 (Yellowknife A)
  # These are erroneously low data from Lambrecht heated tipping bucket
  
  snow_start_51058 <- "2019-09-01"
  snow_stop_51058 <- "2022-04-30"
  
  df <- dplyr::mutate(data, total_precip = ifelse(station_id == 51058 & 
                                                    date >= snow_start_51058 &
                                                    date <= snow_stop_51058 &
                                                    mean_temp < 0,
                                                  NA,
                                                  total_precip))
  
  # Remove bad precipitation data at 43004 (Norman Wells Climate)
  # These are grossly inflated values and do not appear to be accurate
  
  precip_start_43004 <- "2015-07-12"
  precip_stop_43004 <- "2016-04-05"
  
  df <-  dplyr::mutate(df, total_precip = ifelse(station_id == 43004 &
                                                   date >= precip_start_43004 &
                                                   date <= precip_stop_43004,
                                                 NA,
                                                 total_precip))

  
  saveRDS(df, file = paste0(filepath, output_file, extension))
  print(paste0("Congratulations! Climate datafile has been succesfully filtered to ", max(df$date)))
  
}

