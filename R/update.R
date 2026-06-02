#' Update stitched ECCC climate RDS with recent \code{weathercan} pulls.
#'
#' @param filepath Character. Required. Writable directory containing
#'   \code{paste0(input_file, extension)} and where output will be written.
#' @param input_file Character. RDS base name (no path, no extension). Standard pipeline:
#'   \code{"ECCC_Climate_Data_Updated"}.
#' @param output_file Character. RDS base name (no path, no extension). Standard pipeline:
#'   \code{"ECCC_Climate_Data_Updated"}.
#' @param extension Character. File extension including dot. Default \code{".rds"}.
#'
#' @return Writes \code{output_file} to disk (invisibly returns \code{NULL} unless you choose otherwise).
#' @export

update <- function(
    filepath,
    input_file,
    output_file,
    extension = ".rds"
)

{
  # Validate filepath (same story as download)
  if (missing(filepath) || is.null(filepath) || length(filepath) != 1L || !nzchar(filepath)) {
    stop("'filepath' is required and must be a single non-empty string.")
  }
  filepath <- normalizePath(filepath, winslash = "/", mustWork = FALSE)
  input_path <- file.path(filepath, paste0(input_file, extension))
  output_path <- file.path(filepath, paste0(output_file, extension))
  if (!file.exists(input_path)) {
    stop("Input not found: ", input_path)
  }

  data <- readRDS(input_path)
  data$date <- lubridate::ymd(as.character(data$date))

  data[,12] <- as.numeric(unlist(data[,12]))
  data[,13] <- as.numeric(unlist(data[,13]))
  data[,14] <- as.numeric(unlist(data[,14]))

  # NEW - check station ids and date to determine if update is required
  expected_ids <- sort(unique(df_locations$station_id))
  existing_ids <- sort(unique(data$station_id))
  missing_ids  <- setdiff(expected_ids, existing_ids)

  is_stale <- max(data$date, na.rm = TRUE) < (Sys.Date() - 1)
  has_missing_stations <- length(missing_ids) > 0
  needs_update <- is_stale || has_missing_stations


  #if(max(data$date) < Sys.Date() - 1) {
  if(needs_update) {

    update_end <- Sys.Date() - 1
    df <- NULL
    df_missing <- NULL

    if(is_stale) {
  # Create a list of all data within the last year (to only download recent data)
  data_1 <- data %>%
    dplyr::group_by(station_name) %>%
    dplyr::filter(dplyr::between(date, Sys.Date() - 365, update_end)) %>%
    dplyr::filter(!is.na(mean_temp))

  # Extract station names from stations with recent data and sort alphabetically
  site = as.list(sort(unique(data_1$station_id)))

  for (i in seq_along(site)) {

    prev_max_date <- data %>%
      dplyr::filter(station_id == site[[i]]) %>%
      dplyr::slice(which.max(date))

    station_id <- prev_max_date$station_id
    last_date <- min(prev_max_date$date)
    if(last_date >= update_end) next
    data_recent <- weathercan::weather_dl(
      station_ids = station_id,
      start = last_date + 1,
      end = update_end,
      interval = "day",
      quiet=TRUE)

    df <- dplyr::bind_rows(df, data_recent)
    if(nrow(data_recent) > 0) {
    print(paste0(data_recent[1,1], " (ID ", data_recent[1,2], ") has been updated"))
    }

  }
    } # end of if_stale

    # Retrieve data for station IDs in df_locations but missing from updated data
    if(has_missing_stations) {
      message("missing station_id(s) in Updated file:", paste(missing_ids, collapse=","))

  for (stn_id in missing_ids) {
    data_missing <- weathercan::weather_dl(
      station_ids = stn_id,
      start = "1900-01-01",
      end = update_end,
      interval = "day",
      quiet = TRUE
    )
    if (nrow(data_missing) > 0) {
      print(paste0(data_missing[1,1], " (ID ", stn_id, ") has been seeded (missing station)"))
      df_missing <- dplyr::bind_rows(df_missing, data_missing)
    } else {
      warning(paste0("No rows returned for missing station_id ", stn_id))
    }
  }
  } # end of if has_missing_stations

    if (!is.null(df) && nrow(df) > 0) {
      df[,12] <- as.numeric(unlist(df[,12]))
      df[,13] <- as.numeric(unlist(df[,13]))
      df[,14] <- as.numeric(unlist(df[,14]))
    }
    if (!is.null(df_missing) && nrow(df_missing) > 0) {
      df_missing[,12] <- as.numeric(unlist(df_missing[,12]))
      df_missing[,13] <- as.numeric(unlist(df_missing[,13]))
      df_missing[,14] <- as.numeric(unlist(df_missing[,14]))
    }

  df <- dplyr::bind_rows(data, df, df_missing) %>%
    dplyr::arrange(station_name)

  print(paste0("Congratulations! Climate datafile has been succesfully updated to ", max(df$date)))

  } else {

    df <- data %>%
      dplyr::arrange(station_name)

    print("Climate dataset is already up-to-date and no station_ids from df_locations are missing.")

  }

  saveRDS(df, file = output_path)

}


