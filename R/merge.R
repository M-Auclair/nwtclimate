# Function to merge station data to produce more complete records

###############################################################################################

# Arguments for testing:

# filepath = "C:/Users/Ryan_Connon/Documents/NT_Hydrology/Climate/ECCC_station_data/"
# stitch_input_file = "stitched_data/ECCC_Climate_Data_Stitched.rds"
# orig_input_file = "updated_data/ECCC_Climate_Data_Updated.rds"
# output_file = "merged_data/ECCC_Climate_Data_Merged.rds"

##############################################################################################

#' Merge climate datasets together
#'
#' Produce a map of all station data
#' @param filepath Character. The filepath to find input files and save output files
#' @param stitch_input_file Character. The file containing the stitched input file
#' @param orig_input_file Character. The file containing the original input file
#' @param output_file Character. The name of the final output file

#' @return A dataframe of a merged dataset
#' @export


merge <- function(
    filepath,
    stitch_input_file,
    orig_input_file,
    output_file
)
  
{
  
  data_stitch <- readRDS(paste0(filepath, stitch_input_file)) %>%
    dplyr::mutate(date = as.Date(date),
                  merged_name = as.character(merged_name),
                  station_operator = as.character(station_operator))
  
  data_orig <- readRDS(paste0(filepath, orig_input_file)) %>%
    dplyr::mutate(date = as.Date(date),
                  merged_name = as.character(NA),
                  station_operator = as.character(station_operator))
  
  df <- NULL
  
  for(i in seq_along(unique(locations))) { # Object 'locations' is required from dependencies_data.R
    
    station_id <- df_locations %>% # Dataframe 'df_locations' is required from dependencies_data.R
      dplyr::filter(location == unique(locations)[i]) 
    
    id <- station_id$station_id
    
    df_new <- NULL
    
    if(length(id) != 1) {
    
    for(j in head(seq_along(id),-1)) {

      # Create two data frames for the data sets that are being merged.
      ## This is an iterative process where order matters
      ## The first data set is the primary data set. Gaps will be infilled by the next data set.

      if(j == 1) {
        
        df_1 <- data_stitch %>%
          dplyr::filter(merged_name == unique(locations)[i])
        
      } else {
      
      df_1 <- df_new #%>%
        #dplyr::filter(merged_name == unique(locations)[i])
      
      }
      
      df_1 <- fasstr::fill_missing_dates(df_1, dates = "date", pad_ends = F)

      df_2 <- data_orig %>%
        dplyr::filter(station_id == id[j+1]) %>%
        dplyr::filter(date >= min(df_1$date),
                      date <= max(df_1$date))
      
      df_2 <- fasstr::fill_missing_dates(df_2, dates = "date", pad_ends = F)
      
      # Create slices of df_1 data to add (if applicable) at the end of the merge
      # This ensures equal length between data frames
      
      df_1_slice_head <- dplyr::filter(df_1, date < min(df_2$date))
      df_1_slice_tail <- dplyr::filter(df_1, date > max(df_2$date))
      
      df_1 <- df_1 %>%
        dplyr::filter(date >= min(df_2$date),
                      date <= max(df_2$date))
      
      if(nrow(df_2 < 1) == T) {
        stop("There are no data to merge during the period of overlap")
      }
      
      names <- colnames(df_1)
      variables <- c(16,18,20,22,24,26,28,30,32,34,36) # Identify the variables to be isolated
      
      # Determine the common dates between data sets
      dates <- base::merge(df_1, df_2, by = "date")[,1]
      
      if(length(dates) < 1) {
        stop("There are no data to merge during the period of overlap")
      }
      
      # Create two data frames using only common dates
      df_1_common <- dplyr::filter(df_1, df_1$date %in% dates)
      df_2_common <- dplyr::filter(df_2, df_2$date %in% dates)
      
      # Find all dates for when no data are available in first data set
      df_1_na <- df_1_common %>%
        dplyr::filter_at(dplyr::vars(all_of(variables)), dplyr::all_vars(is.na(.)))
      
      # Find all dates for when there is at least one datapoint available in dataset y (second dataset)
      df_2_na <- df_2_common %>%
        dplyr::filter_at(dplyr::vars(all_of(variables)), dplyr::any_vars(is.na(.)))
      
      # Identify dates for when all variables from data set 1 can be replaced by data set 2
      dates_2 <- base::merge(df_1_na, df_2_na, by = "date")[,1]
      
      # Remove dates from data set 1
      df_x <- df_1 %>%
        dplyr::filter(!date %in% dates_2)
      
      # Add dates from dataset 2
      df_y <- df_2 %>%
        dplyr::filter(date %in% dates_2)
      
      # Infill all rows (including station information) from data set 1 with empty rows from data set 2
      df_merge <- dplyr::bind_rows(df_x, df_y) %>%
        dplyr::arrange(by = date)
      
      ## Step 1 complete. Empty data rows have been infilled
      ## Station information has been updated to represent the correct station
      
      # Step 2: Infill partially empty rows where appropriate
      # Create a data frame of values to merge
      df_merge_2 <- data.frame()
      
      # Create a logical list of all parameters
      for(m in variables) {
        
        df_merge_2.1 <- data.frame(
          "date" = df_merge$date,
          "parameter" = paste(names[m]), 
          "logical" = is.na(df_merge[[m]]) & !is.na(df_2[[m]]))
        
        df_merge_2 <- dplyr::bind_rows(df_merge_2, df_merge_2.1)
        
      }
      
      # Combine all parameters to determine which dates have a value to be replaced
      dates_3 <- df_merge_2 %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(replace = as.numeric(sum(logical))) %>%
        dplyr::mutate(replace = replace > 0) %>%
        dplyr::filter(replace == T) %>%
        dplyr::select(date)
      
      # Use list of dates with replaced values to edit station_name, station_id. WMO_id, TC_id
      df_merge <- df_merge %>%
        dplyr::mutate(station_name = dplyr::case_when(date %in% dates_3$date ~ paste0(toupper(merged_name), " MERGED"),
                                                      !(date %in% dates_3$date) ~ station_name),
                      station_id = dplyr::case_when(date %in% dates_3$date ~ as.numeric(NA), 
                                                    !(date %in% dates_3$date) ~ station_id),
                      climate_id = dplyr::case_when(date %in% dates_3$date ~ as.character(NA), 
                                                    !(date %in% dates_3$date) ~ climate_id),
                      WMO_id = dplyr::case_when(date %in% dates_3$date ~ as.character(NA), 
                                                !(date %in% dates_3$date) ~ WMO_id),
                      TC_id = dplyr::case_when(date %in% dates_3$date ~ as.character(NA), 
                                               !(date %in% dates_3$date) ~ TC_id))
      
      # Merge datasets
      
      for(n in variables) {
        
        df_merge[[n]][is.na(df_merge[[n]])] <- 
          df_2[[n]][match(df_merge$date, df_2$date)][which(is.na(df_merge[[n]]))]
        
      }

      df_new <- dplyr::bind_rows(df_1_slice_head, df_merge, df_1_slice_tail) %>%
        dplyr::mutate(merged_name = as.character(unique(locations)[i]))
      
    } 
      
      } else {
      
      # If there is only one station, skip the loop to merge data sets and append single data set to df
      
      df_new <- data_stitch %>%
        dplyr::filter(station_id == id)
      
    }
    
    df <- dplyr::bind_rows(df, df_new) 
    
    print(paste0("You have merged the ", unique(locations)[i], " dataset"))
    
  }

  saveRDS(df, file = paste0(filepath, output_file))
  print(paste0("Congratulations! Climate datafile has been succesfully merged to ", max(df$date)))
  df

}
