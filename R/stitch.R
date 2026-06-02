#' Function to stitch data together
#'
#' @param filepath Character. Required. Writable directory containing
#'   \code{paste0(input_file, extension)} and where output will be written.
#' @param input_file Character. RDS base name (no path, no extension). Standard pipeline:
#'   \code{"ECCC_Climate_Data_Filtered"}.
#' @param output_file Character. RDS base name (no path, no extension). Standard pipeline:
#'   \code{"ECCC_Climate_Data_Stitched"}.
#' @param extension Character. File extension including dot. Default \code{".rds"}.
#'
#' @return Writes \code{output_file} to disk
#' @export




# Dependency functions:
# update()

# Dependency data
# locations
# station_ids

# Function to stitch data together

stitch <- function(
    filepath,
    input_file,
    output_file,
    extension = ".rds"
)

{
  # Read in current data
  if (missing(filepath) || !nzchar(filepath)) {
    stop("'filepath' is required.")
  }
  input_path  <- file.path(filepath, paste0(input_file, extension))
  output_path <- file.path(filepath, paste0(output_file, extension))
  if (!file.exists(input_path)) stop("Input not found: ", input_path)

  data <- readRDS(input_path)

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

  saveRDS(df, file = output_path)
  print(paste0("Congratulations! Climate datafile has been succesfully stitched to ", max(df$date)))

}
