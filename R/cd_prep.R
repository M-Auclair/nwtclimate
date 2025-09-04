# Prep data for Climate DB
library(dplyr)

`%>%` <- magrittr::`%>%`

source("R/dependencies_functions.R")

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")))

file_path <- paste0(user, "/Documents/R_scripts/Packages/nwtclimate/data/")
dir.create(paste0(file_path, "separated/"))
savepath <- paste0(user, "/Documents/R_scripts/Packages/nwtclimate/data/separated/")
file_name <- "allstations_flagged.rds"
climatedf <- readRDS(paste0(file_path, "/", file_name))

#make sure rain check flags are transitioned to "rain_flag"
climatedf <- climatedf %>%
  dplyr::mutate(rain_flag = ifelse(!is.na(Rain_check_flag) & Rain_check_flag == "QC", "QC_PSM", rain_flag))

# remove Rain_check_flag column if present
climatedf <- climatedf[, !names(climatedf) %in% c("Rain_check_flag")]

#rename hour column to cd_time, then remove date and hour col
climatedf$cd_time <- climatedf$hour
climatedf <- climatedf[, !(names(climatedf) %in% c("date", "hour"))]

#rename to match fieldnames in DB
climatedf <- climatedf %>%
  dplyr::rename(
    rain_mm = total_precip,
    cd_year = year,
    cd_month = month,
    cd_day = day,
    t_air_high_C = t_air,
    t_air_low_C = t_air_2,
    RH_high_per = RH,
    RH_low_per = RH_2,
    wind_sp_ms = wind_sp,
    wind_dir_deg = wind_dir,
    net_SW_Wm2 = net_SW,
    net_LW_Wm2 = net_LW,
    rn_Wm2 = rn,
    sw_in_Wm2 = sw_in,
    sr50_cm = sr50,
    t_soil_1_C = t_soil_1,
    t_soil_2_C = t_soil_2,
    t_soil_3_C = t_soil_3,
    t_soil_4_C = t_soil_4,
    t_soil_5_C = t_soil_5,
    t_soil_6_C = t_soil_6,
    t_water_C = t_water,
    t_water_2_C = t_water_2,
    water_depth_m = water_depth,
    water_depth_corr_m = water_depth_corr,
    t_air_flag = t_air_flag,
    RH_flag = RH_flag,
    wind_sp_flag = wind_sp_flag,
    wind_dir_flag = wind_dir_flag,
    net_SW_flag = net_SW_flag,
    net_LW_flag = net_LW_flag,
    rn_flag = rn_flag,
    sw_in_flag = sw_in_flag,
    sr50_flag = sr50_flag
  )

#add missing flag columns
new_flag_cols <- c("t_soil_1_flag",
                   "t_soil_2_flag",
                   "t_soil_3_flag",
                   "t_soil_4_flag",
                   "t_soil_5_flag",
                   "t_soil_6_flag",
                   "t_air_2_flag",
                   "RH_2_flag",
                   "t_water_flag",
                   "t_water_2_flag",
                   "water_depth_flag",
                   "water_depth_corr_flag")

climatedf[,new_flag_cols] <- NA

# columns to include in all variable dfs
additional_columns <- c("station_name",
                        "cd_year",
                        "JD",
                        "cd_month",
                        "cd_day",
                        "cd_time")
# variables
main_columns <- c(
  "t_air_high_C",
  "RH_high_per",
  "rain_mm",
  "wind_sp_ms",
  "wind_dir_deg",
  "net_SW_Wm2",
  "net_LW_Wm2",
  "rn_Wm2",
  "sw_in_Wm2",
  "sr50_cm",
  "t_soil_1_C",
  "t_soil_2_C",
  "t_soil_3_C",
  "t_soil_4_C",
  "t_soil_5_C",
  "t_soil_6_C",
  "t_air_low_C",
  "RH_low_per",
  "t_water_C",
  "t_water_2_C",
  "water_depth_m",
  "water_depth_corr_m"
)

# variable flags
flag_col <- c(
  "t_air_flag",
  "RH_flag",
  "rain_flag",
  "wind_sp_flag",
  "wind_dir_flag",
  "net_SW_flag",
  "net_LW_flag",
  "rn_flag",
  "sw_in_flag",
  "sr50_flag",
  "t_soil_1_flag",
  "t_soil_2_flag",
  "t_soil_3_flag",
  "t_soil_4_flag",
  "t_soil_5_flag",
  "t_soil_6_flag",
  "t_air_2_flag",
  "RH_2_flag",
  "t_water_flag",
  "t_water_2_flag",
  "water_depth_flag",
  "water_depth_corr_flag"
)

# make sure stations are named properly
climatedf$station_name[climatedf$station_name == "Winter Lake"] <- "Winter Lake FTS"
climatedf$station_name[climatedf$station_name == "Tuktoyaktuk"] <- "Tuk"
climatedf$station_name[climatedf$station_name == "Tibbitt"] <- "Tibbitt muskeg"

#option to save file
#saveRDS(climatedf, paste0(file_path, "/allstations_flagged_raw_DBcompatible.rds"))

#remove duplicates
climatedf <- climatedf %>%
  distinct(station_name, cd_year, cd_month, cd_day, cd_time, .keep_all = TRUE)

# apply split_df function to create one df per variable
# note: split_df function in dependencies_functions
  climatedf_list <- split_df(
    df = climatedf,
    additional_columns = additional_columns,
    main_columns = main_columns,
    flag_col = flag_col
  )

# remove all rows where variable is NA ine each df
climatedf_list <- remove_na_rows(climatedf_list)

#
# save as separate csv's
for (df_name in names(climatedf_list)) {
  df <- climatedf_list[[df_name]]
  filename <- paste0(savepath, df_name, ".csv")
  write.csv(df, file = filename, row.names = FALSE)
  cat("df", df_name, "saved as", filename, "\n")
}


