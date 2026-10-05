#Script to get climate database files - MEMORY EFFICIENT VERSION
library(magrittr)
library(dplyr)
library(tidyr)
'%!in%' = Negate('%in%')

connection <- odbc::dbConnect(drv = odbc::odbc(), dsn = "ECC_CLIMATE", uid = Sys.getenv("ORACLE_CLIM_USER"), pwd = Sys.getenv("ORACLE_CLIM_PWD"))
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")), "/Documents/")
save_path <- paste0(user, "R_Scripts/Packages/nwtclimate/data/")

#stations table
cd_stations<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_STATIONS")

#data flags table
cd_data_flags<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_DATA_FLAGS_LUT")
cd_data_flags_remove<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_DATA_FLAGS_LUT")

#data types table
cd_data_types<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_DATA_TYPES_LUT")

#data tables
Table0<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_0 WHERE STATION_ID = 58") #this step takes awhile, could filter down based on station here
Table3<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_3 WHERE STATION_ID = 58")
Table1<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_1 WHERE STATION_ID = 58")
Table2 <- odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_2 WHERE STATION_ID = 58")

#optional save
#change paths as needed
saveRDS(Table0, paste0(save_path, "PROD_Table0", Sys.Date(), ".rds"))
saveRDS(Table1, paste0(save_path, "PROD_Table1", Sys.Date(), ".rds"))
saveRDS(Table3, paste0(save_path, "PROD_Table3", Sys.Date(), ".rds"))
saveRDS(Table2, paste0(save_path, "PROD_Table2", Sys.Date(), ".rds"))

merged_data <- rbind(Table0, Table1, Table3, Table2)

#left join station_id using station name
merged_data <- merged_data %>%
  dplyr::left_join(cd_stations %>% dplyr::select(STATION_NAME, STATION_ID), by = c("STATION_ID"))

#left join variable name using data type list
merged_data <- merged_data %>%
  dplyr::left_join(cd_data_types %>% dplyr::select(DATA_TYPE_SH, DATA_TYPE_ID), by = c("DATA_TYPE_ID"))

#left join flag flag_col based on flag list
merged_data <- merged_data %>%
  dplyr::left_join(cd_data_flags %>% dplyr::select(DATA_FLAG, DATA_FLAG_ID), by = c("DATA_FLAG_ID"))

#write out long data format (if desired)
#saveRDS(merged_data, paste0(save_path, "PRODlong_data20260713.RDS"))

# function using pivot wider

create_wide_data <- function(merged_data) {

  # Parameters of interest
  climate_params <- c(
    "RH_HIGH_PER", "RH_LOW_PER", "WIND_SP_MS", "WIND_DIR_DEG",
    "RAIN_MM", "SR50_CM", "SW_IN_WM2", "NET_SW_WM2", "NET_LW_WM2",
    "RN_WM2", "T_AIR_HIGH_C", "T_AIR_LOW_C", "T_WATER_C",
    "T_WATER_2_C", "WATER_DEPTH_M", "WATER_DEPTH_CORR_M",
    "T_SOIL_1_C", "T_SOIL_2_C", "T_SOIL_3_C", "T_SOIL_4_C",
    "T_SOIL_5_C", "T_SOIL_6_C", "BARO_PRESSURE_MBAR"
  )

  # ID columns that define a unique time step at a station
  id_cols <- c(
    "STATION_ID", "CD_YEAR", "CD_MONTH", "CD_DAY", "CD_TIME",
     "CD_TIME_SEC_FR_MIDNIGHT", "STATION_NAME"
  )

  # Work only with parameters of interest
  long_filtered <- merged_data %>%
    dplyr::filter(DATA_TYPE_SH %in% climate_params) %>%
    # keep only what we need
    dplyr::select(dplyr::all_of(id_cols),
                  DATA_TYPE_SH, DATA_VALUE, DATA_FLAG)

  cat("Long data dimensions (filtered):", dim(long_filtered), "\n")

  # If you may have multiple rows per ID + DATA_TYPE_SH, you can
  # collapse here (for now we assume uniqueness)

  wide_data <- long_filtered %>%
    # Make sure flags are not factors
    dplyr::mutate(DATA_FLAG = as.character(DATA_FLAG)) %>%
    tidyr::pivot_wider(
      id_cols   = dplyr::all_of(id_cols),
      names_from  = DATA_TYPE_SH,
      values_from = c(DATA_VALUE, DATA_FLAG),
      names_glue  = "{DATA_TYPE_SH}{ifelse(.value == 'DATA_FLAG', '_flag', '')}"
    )

  cat("Wide data dimensions:", dim(wide_data), "\n")

  return(wide_data)
}

wide_data <- create_wide_data(merged_data)

#check data for NA values
summarize_missing_keys_wide <- function(wide_data) {
  # Key ID/time columns to check (exclude JD on purpose)
  key_cols <- c(
    "STATION_ID",
    "STATION_NAME",
    "CD_YEAR",
    "CD_MONTH",
    "CD_DAY",
    "CD_TIME",
    "CD_TIME_SEC_FR_MIDNIGHT"
  )

  # Safety check: warn if any expected columns are missing
  existing_key_cols <- intersect(key_cols, names(wide_data))
  missing_key_cols  <- setdiff(key_cols, names(wide_data))

  if (length(missing_key_cols) > 0) {
    warning(
      "These expected columns are missing in wide_data: ",
      paste(missing_key_cols, collapse = ", ")
    )
  }

  wide_data %>%
    dplyr::summarise(
      n_rows = dplyr::n(),
      # count NAs per key column
      dplyr::across(
        dplyr::all_of(existing_key_cols),
        ~ sum(is.na(.x)),
        .names = "n_na_{.col}"
      ),
      # count rows where ANY key column is NA
      n_rows_with_any_na_key = sum(
        dplyr::if_any(dplyr::all_of(existing_key_cols), is.na)
      )
    )
}
summarize_missing_keys_wide(wide_data)

# Clean up the data
clean_flag_columns <- function(wide_data) {

  # Identify all flag columns (those ending in "_flag")
  flag_cols <- grep("_flag$", names(wide_data), value = TRUE)

  if (length(flag_cols) == 0) {
    message("No flag columns found.")
    return(wide_data)
  }

  # Replace any character "NA" with actual NA
  wide_data <- wide_data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(flag_cols),
        ~ dplyr::na_if(.x, "NA")
      )
    )

  return(wide_data)
}
wide_data <- clean_flag_columns(wide_data)

# Remove flagged data from dataset
remove_flagged_data <- function(wide_data){

   remove_flags <- c("UQ", "QC_CT", "QC", "QC_SSM", "QC_SS")

   data_cols <- cd_data_types$DATA_TYPE_SH[cd_data_types$DATA_TYPE_SH %in% names(wide_data)]

   clean_data <- wide_data

   for (i in data_cols){

     data_flag <- i %>%
       stringr::str_c("_flag")

     if (!data_flag %in% names(clean_data)) next

     clean_data[[i]] <- ifelse(clean_data[[data_flag]] %in% remove_flags, NA, clean_data[[i]])

   }

    clean_data

  }

cleaned_data <- remove_flagged_data(wide_data)

#test lines
#wide_data["Date"] <- as.Date(paste0(wide_data$CD_YEAR, "-", wide_data$CD_MONTH, "-", wide_data$CD_DAY))
#plot(wide_data$T_AIR_HIGH_C~wide_data$Date)

# Save the data
saveRDS(wide_data, paste0(save_path, "Pocket93_25.RDS"))

#save as csv
#write.csv(wide_data, paste0(save_path, "/wide_data.csv"))
#write.csv(cd_data_flags, paste0(save_path, "/data_flags.csv"))
#write.csv(cd_stations, paste0(save_path, "/cd_stations.csv"))
#write.csv(cd_data_types, paste0(save_path, "/cd_data_types.csv"))

# Save the cleaned version
saveRDS(cleaned_data, paste0(save_path, "cleaned_data.RDS"))

# Save the data - as individual sites

save_wide_data_per_site <- function(wide_data,
                                    id_col = "STATION_ID",
                                    name_col = "STATION_NAME",
                                    output_dir = paste0(save_path)) {
  # Ensure required columns exist
  if (!id_col %in% names(wide_data)) {
    stop("Column ", id_col, " not found in wide_data.")
  }
  if (!name_col %in% names(wide_data)) {
    stop("Column ", name_col, " not found in wide_data.")
  }

  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Get unique station IDs
  station_ids <- unique(wide_data[[id_col]])

  for (sid in station_ids) {
    # Subset data for this site
    site_data <- wide_data[wide_data[[id_col]] == sid, ]

    # Get the (first) station name for nicer filenames
    site_name <- unique(site_data[[name_col]])[1]

    # Build a safe file name: replace spaces and problematic characters
    safe_name <- gsub("[^A-Za-z0-9_]+", "_", site_name)
    safe_id   <- gsub("[^A-Za-z0-9_]+", "_", as.character(sid))

    file_name <- paste0(safe_id, "_", safe_name, ".rds")
    file_path <- file.path(output_dir, file_name)

    csvfile_name <- paste0(safe_id, "_", safe_name, ".csv")
    csvfile_path <- file.path(output_dir, csvfile_name)

    # Save this site's data
    saveRDS(site_data, file_path)
    write.csv(site_data, csvfile_path)
  }

  message("Saved ", length(station_ids), " site files to: ", normalizePath(output_dir))

  invisible(station_ids)
}

save_wide_data_per_site(
  wide_data   = wide_data,
  id_col      = "STATION_ID",
  name_col    = "STATION_NAME",
  output_dir  = paste0(save_path)
)

#or for cleaned data

save_wide_data_per_site(
  wide_data   = cleaned_data,
  id_col      = "STATION_ID",
  name_col    = "STATION_NAME",
  output_dir  = paste0(save_path)
)


# Test the output
cat("Final data dimensions:", dim(wide_data), "\n")
cat("Column names:\n")
print(names(wide_data))
cat("First few rows:\n")
print(head(wide_data))
