# Download ECCC climate data

#' Downloads station data via \code{weathercan} and saves outputs under
#' \code{filepath}.
#'
#' @param prov_terr Character or \code{NULL}. Two-letter abbreviation for province/territory in Canada
#'   (e.g. \code{"NT"}). See function body for details.
#' @param timestep Character. One of \code{"hour"}, \code{"day"}, \code{"month"}.
#' @param start_date,end_date Dates or \code{YYYY-MM-DD} strings, or \code{NULL}.
#' @param filepath Character. **Required.** Writable directory where downloads
#'   are saved. Must be supplied explicitly. For batch jobs or GitHub Actions,
#'   set directory in your script or via environment variable
#'   \code{NWTCLIMATE_DATA_DIR} if you implement that fallback inside the function.
#' @param filename Character. Base name for output files (see function for pattern).
#' @param quiet Logical. If \code{TRUE}, suppress messages.
#'
#' @return .rds file of downloaded dataset
#' @export

# Generic script to download all climate data and save as an .RDS file
# Only necessary to do once
# This process can take multiple hours for large datasets

download <- function(
    prov_terr = NULL, # Character. Capitalized two letter abbreviation for province or territory in Canada.
    timestep = "day", # Character. Timestep of the data, one of "hour", "day", "month"
    start_date = NULL, # Date/Character. The start date of the data in YYYY-MM-DD format. Defaults to start of range.
    end_date = NULL, # Date/Character. The end date of the data in YYYY-MM-DD format. Defaults to end of range.
    filepath = NULL,
    filename = "ECCC_Master_Climate_List_Original_Data_",
    quiet = FALSE # Logical. Suppress all messages
  )

{

  if (is.null(filepath) || !nzchar(filepath)) {
    stop(
      "Argument 'filepath' is required.\n",
      "Provide a directory where downloaded data should be saved, e.g.:\n",
      "  filepath = 'C:/path/to/your/ECCC_download_folder'\n",
      "For scripts, you can also set environment variable NWTCLIMATE_DATA_DIR."
    )
  }
  filepath <- normalizePath(filepath, winslash = "/", mustWork = FALSE)
  if (!dir.exists(filepath)) {
    dir.create(filepath, recursive = TRUE, showWarnings = FALSE)
  }

# Download station list for all stations in selected jurisdiction
df_stations <- weathercan::stations() %>%
  dplyr::filter(interval == timestep)

if(!is.null(prov_terr)) {
  df_stations <- dplyr::filter(df_stations, prov == prov_terr)
}

if(!is.null(start_date)) {
  df_stations <- dplyr::filter(df_stations, end >= lubridate::year(start_date))
}

if(!is.null(end_date)) {
  df_stations <- dplyr::filter(df_stations, start <= lubridate::year(end_date))
}

df <- NULL

for(i in seq_along(df_stations$station_id)) {
  df_single <- weathercan::weather_dl(start = start_date,
                                      end = end_date,
                                      station_id = df_stations$station_id[i],
                                      interval = timestep,
                                      quiet = quiet)

  if(quiet == FALSE) {
  print(paste0(df_stations$station_name[i], " successfully downloaded"))
  }

  df <- dplyr::bind_rows(df, df_single)
}

file_date = paste0(filename,
                  lubridate::year(Sys.Date()), "_",
                  round(lubridate::month(Sys.Date()), 1), "_",
                  lubridate::day(Sys.Date()),
                  ".rds")

saveRDS(df, file = paste0(filepath, file_date))

}
