##############################################################################################

# Dependency functions: 
# update()

# Dependency data
# locations
# station_ids

##############################################################################################

# Arguments for testing:

# filepath = "C:/Users/Ryan_Connon/Documents/NT_Hydrology/Climate/ECCC_station_data/"
# input_file = "updated_data/ECCC_Climate_Data_Updated.rds"
# output_file = "stitched_data/ECCC_Climate_Data_Stitched.rds"
# extension = ".rds"

##############################################################################################

# Function to stitch data together

stitch <- function(
    filepath,
    input_file,
    output_file
)
  
{
  # Read in current data
  # Run the data_update() function to read in most current data if necessary

  data <- readRDS(paste0(filepath, input_file, extension))
  
  # Change class of all columns

  data[,11] <- as.Date(unlist(data[,11]))
  data[,12] <- as.numeric(unlist(data[,12]))
  data[,13] <- as.numeric(unlist(data[,13]))
  data[,14] <- as.numeric(unlist(data[,14]))

  df <- NULL
  
  for(i in seq_along(unique(locations))) { # Object 'locations' is required from dependencies_data.R
    
    station_id <- df_locations %>% # Dataframe 'df_locations' is required from dependencies_data.R
      dplyr::filter(location == unique(locations)[i]) 
    
    id <- station_id$station_id
    
      df_1 <- NULL
      
      for(j in seq_along(id)) {

        current_id <- id[j]
        
        if(j == 1) {
          df_2 <- data %>%
            dplyr::filter(station_id == current_id)
        } else if(j > 1) {
          prev_min_date <- df_1 %>%
            dplyr::slice(min(which(!is.na(mean_temp)))) 
          df_2 <- data %>%
            dplyr::filter(station_id == current_id,
                          date < min(df_1$date))
        }
        
        df_1 <- dplyr::bind_rows(df_1, df_2) %>%
          dplyr::arrange(date)
        
      }
      
      df_1 <- df_1 %>%
        dplyr::mutate(merged_name = unique(locations)[i])
      
      df <- dplyr::bind_rows(df, df_1) 
      
    }
  
  df$merged_name <- as.character(df$merged_name)
  
  saveRDS(df, file = paste0(filepath, output_file, extension))
  print(paste0("Congratulations! Climate datafile has been succesfully stitched to ", max(df$date)))
  
}
