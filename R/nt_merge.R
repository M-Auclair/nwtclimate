# Function to merge station data to produce more complete records

###############################################################################################

# Arguments for testing:
# 
# filepath = "C:/Users/maucl/Documents/R_Scripts/Packages/nwtclimate/data/"
# stitch_input_file = "ECCC_Climate_Data_Stitched"
# orig_input_file = "ECCC_Climate_Data_Updated"
# output_file = "ECCC_Climate_Data_Merged"
# extension = ".rds"

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


nt_merge <- function(
    filepath,
    stitch_input_file,
    orig_input_file,
    output_file
)
  
{
  ########################################
  data.stitch <- readRDS(paste0(filepath, stitch_input_file, extension))
  data.stitch <- dplyr::mutate(data.stitch, date = as.Date(date),
                               merged_name = as.character(merged_name),
                               station_operator = as.character(station_operator))
  
  data.orig <- readRDS(paste0(filepath, orig_input_file, extension))
  data.orig <- dplyr::mutate(data.orig, date = as.Date(date),
                             merged_name = as.character(NA),
                             station_operator = as.character(station_operator))
  
  df <- NULL

  for(i in seq_along(unique(locations))) { # Object 'locations' is required from dependencies_data.R
    
    station.id <- dplyr::filter(df_locations, location == unique(locations)[i])

    id <- station.id$station_id
    
    df.new <- NULL
    
    if(length(id) != 1) {
    
    for(j in head(seq_along(id),-1)) {

      # Create two data frames for the data sets that are being merged.
      ## This is an iterative process where order matters
      ## The first data set is the primary data set. Gaps will be infilled by the next data set.

      if(j == 1) {
        
        df.1 <- dplyr::filter(data.stitch, merged_name == unique(locations)[i])
        
      } else {
      
      df.1 <- df.new 
      
      }
      
      df.1 <- fasstr::fill_missing_dates(df.1, dates = "date", pad_ends = F)
      
      df.2 <- dplyr::filter(data.orig, station_id == id[j+1])
      df.2 <- dplyr::filter(df.2, date >= min(df.1$date),
                            date <= max(df.1$date))

      # df.2 <- data.orig %>%
      #   dplyr::filter(station.id == id[j+1]) %>%
      #   dplyr::filter(date >= min(df.1$date),
      #                 date <= max(df.1$date))
      
      df.2 <- fasstr::fill_missing_dates(df.2, dates = "date", pad_ends = F)
      
      # Create slices of df.1 data to add (if applicable) at the end of the merge
      # This ensures equal length between data frames
      
      df.1.slice.head <- dplyr::filter(df.1, date < min(df.2$date))
      df.1.slice.tail <- dplyr::filter(df.1, date > max(df.2$date))
      
      df.1 <- df.1 %>%
        dplyr::filter(date >= min(df.2$date),
                      date <= max(df.2$date))
      
      if(nrow(df.2 < 1) == T) {
        stop("There are no data to merge during the period of overlap")
      }
      
      names <- colnames(df.1)
      variables <- c(16,18,20,22,24,26,28,30,32,34,36) # Identify the variables to be isolated
      
      # Determine the common dates between data sets
      dates <- base::merge(df.1, df.2, by = "date")[,1]
      
      if(length(dates) < 1) {
        stop("There are no data to merge during the period of overlap")
      }
      
      # Create two data frames using only common dates
      df.1.common <- dplyr::filter(df.1, df.1$date %in% dates)
      df.2.common <- dplyr::filter(df.2, df.2$date %in% dates)
      
      # Find all dates for when no data are available in first data set
      df.1.na <- dplyr::filter_at(df.1.common, dplyr::vars(all_of(variables)), dplyr::all_vars(is.na(.)))
      
      # df.1.na <- df.1.common %>%
      #   dplyr::filter_at(dplyr::vars(all_of(variables)), dplyr::all_vars(is.na(.)))
      
      # Find all dates for when there is at least one datapoint available in dataset y (second dataset)
      df.2.na <- dplyr::filter_at(df.2.common, dplyr::vars(all_of(variables)), dplyr::all_vars(is.na(.)))
      
      # df.2.na <- df.2.common %>%
      #   dplyr::filter_at(dplyr::vars(all_of(variables)), dplyr::any_vars(is.na(.)))
      
      # Identify dates for when all variables from data set 1 can be replaced by data set 2
      dates.2 <- base::merge(df.1.na, df.2.na, by = "date")[,1]
      
      # Remove dates from data set 1
      df.x <- dplyr::filter(df.1, !date %in% dates.2)
      
      # Add dates from dataset 2
      df.y <- dplyr::filter(df.2, date %in% dates.2)
      
      # Infill all rows (including station information) from data set 1 with empty rows from data set 2
      df.merge <- dplyr::bind_rows(df.x, df.y) 
      df.merge <- dplyr::arrange(df.merge, by = date)
      
      ## Step 1 complete. Empty data rows have been infilled
      ## Station information has been updated to represent the correct station
      
      # Step 2: Infill partially empty rows where appropriate
      # Create a data frame of values to merge
      df.merge.2 <- data.frame()
      
      # Create a logical list of all parameters
      for(m in variables) {
        
        df.merge.2.1 <- data.frame(
          "date" = df.merge$date,
          "parameter" = paste(names[m]), 
          "logical" = is.na(df.merge[[m]]) & !is.na(df.2[[m]]))
        
        df.merge.2 <- dplyr::bind_rows(df.merge.2, df.merge.2.1)
        
      }
      
      # Combine all parameters to determine which dates have a value to be replaced
      dates.3 <- df.merge.2 %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(replace = as.numeric(sum(logical))) %>%
        dplyr::mutate(replace = replace > 0) %>%
        dplyr::filter(replace == T) %>%
        dplyr::select(date)
      
      # Use list of dates with replaced values to edit station_name, station_id. WMO_id, TC_id
      df.merge <- df.merge %>%
        dplyr::mutate(station_name = dplyr::case_when(date %in% dates.3$date ~ paste0(toupper(merged_name), " MERGED"),
                                                      !(date %in% dates.3$date) ~ station_name),
                      station_id = dplyr::case_when(date %in% dates.3$date ~ as.numeric(NA), 
                                                    !(date %in% dates.3$date) ~ station_id),
                      climate_id = dplyr::case_when(date %in% dates.3$date ~ as.character(NA), 
                                                    !(date %in% dates.3$date) ~ climate_id),
                      WMO_id = dplyr::case_when(date %in% dates.3$date ~ as.character(NA), 
                                                !(date %in% dates.3$date) ~ WMO_id),
                      TC_id = dplyr::case_when(date %in% dates.3$date ~ as.character(NA), 
                                               !(date %in% dates.3$date) ~ TC_id))
      
      # Merge datasets
      
      for(n in variables) {
        
        df.merge[[n]][is.na(df.merge[[n]])] <- 
          df.2[[n]][match(df.merge$date, df.2$date)][which(is.na(df.merge[[n]]))]
        
      }

      df.new <- dplyr::bind_rows(df.1.slice.head, df.merge, df.1.slice.tail) %>%
        dplyr::mutate(merged_name = as.character(unique(locations)[i]))
      
    } 
      
      } else {
      
      # If there is only one station, skip the loop to merge data sets and append single data set to df
      
      df.new <- data.stitch %>%
        dplyr::filter(station_id == id)
      
    }
    
    df <- dplyr::bind_rows(df, df.new) 
    
    print(paste0("You have merged the ", unique(locations)[i], " dataset"))
    
  }

  saveRDS(df, file = paste0(filepath, output_file, extension))
  save(df, file = paste0(filepath, output_file, ".rda"))
  print(paste0("Congratulations! Climate datafile has been succesfully merged to ", max(df$date)))
  df

}
