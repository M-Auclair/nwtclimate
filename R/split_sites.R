# Function to split updated homogenized data into rds file for each site

df <- readRDS(paste0(data_path, merged_data, extension))


# Function to split & save data by site
#savepath <- "C:/Users/maucl/Documents/R_Scripts/Packages/nwtclimate/data/"

split_sites <- function(df, data_path = "./") {

  merged_names <- unique(df$merged_name)

  # rename cols
  {
    names_ECCC_daily <- c(
      "station_name",
      "station_id",
      "station_operator",
      "prov",
      "lat",
      "lon",
      "elev",
      "climate_id",
      "wmo_id",
      "tc_id",
      "date",
      "year",
      "month",
      "day",
      "qual",
      "cool_deg_days",
      "cool_deg_days_flag",
      "dir_max_gust",
      "dir_max_gust_flag",
      "heat_deg_days",
      "heat_deg_days_flag",
      "t_air_max",
      "t_air_max_flag",
      "t_air",
      "t_air_flag",
      "t_air_min",
      "t_air_min_flag",
      "snow_grnd",
      "snow_grnd_flag",
      "spd_max_gust",
      "spd_max_gust_flag",
      "total_precip",
      "total_precip_flag",
      "total_rain",
      "total_rain_flag",
      "total_snow",
      "total_snow_flag",
      "merged_name"
    )
  }
  colnames(df) <- names_ECCC_daily
  df$date <- as.Date(df$date)

  # replace_coordinates function from dependencies_functions
  df <- replace_coordinates(df, "Fort McPherson", 67.45, -134.88)
  df <- replace_coordinates(df, "Tulita", 64.90, -125.57)

  split_dfs <- list() # list to store split df

  for (name in merged_names) {

    subset_df <- df[df$merged_name == name, ]

    split_dfs[[name]] <- subset_df

    filename <- file.path(data_path, paste0(name, ".rds"))

    #filename <- paste0(data_path, name, ".rds")
    saveRDS(subset_df, file = filename)
  }

  return(split_dfs)
}

#split_dfs <- split_sites(df, data_path = data_path)


# function to bring in data - function is in 'dependencies_functions.R'
## example usage:
#site <- c("Wrigley","Fort Simpson","Scotty Creek","Fort Liard","Sambaa Ke") # example sites to test function
#data_path <- "C:/Users/maucl/Documents/R_Scripts/Packages/nwtclimate/data/" #defined in nwtclimate_functions.R

import_site_files <- function(site, data_path = "./") {
  data_list <- list()
  for (i in site) {

    filename <- paste0(data_path, i, ".rds")

    data <- readRDS(filename)

    data_list[[i]] <- data

    data <- dplyr::bind_rows(data_list)
  }
  return(data)
}

# data <- import_site_files(site, data_path)
#
# test <- import_site_files("Yellowknife", data_path)
#
#
















