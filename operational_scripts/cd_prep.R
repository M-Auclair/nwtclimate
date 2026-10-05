# Prep data for database
library(dplyr)

`%>%` <- magrittr::`%>%`

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")))

file_path <- paste0(user, "/Documents/R_scripts/Packages/nwtclimate/data")
file_name <- "allstations_flagged_2025.rds"
climatedf <- readRDS(paste0(file_path, "/", file_name ))

#make sure rain check flags are transitioned to "rain_flag"
climatedf <- climatedf %>%
  dplyr::mutate(rain_flag = ifelse(!is.na(Rain_check_flag) & Rain_check_flag == "QC", "QC_PSM", rain_flag))

# remove Rain_check_flag column if present
climatedf <- climatedf[, !names(climatedf) %in% c("Rain_check_flag")]

#rename hour column to time, then remove date and hour col
climatedf$cd_time <- climatedf$hour
climatedf <- climatedf[, !(names(climatedf) %in% c("date", "hour"))]

#rename to match fieldnames
climatedf <- climatedf %>%
  dplyr::rename(
    station_notes = notes,
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
    t_air_flag = t_air_flag, #changed this flag name
    RH_flag = RH_flag, #changed this flag name
    wind_sp_flag = wind_sp_flag,
    wind_dir_flag = wind_dir_flag,
    net_SW_flag = net_SW_flag,
    net_LW_flag = net_LW_flag,
    rn_flag = rn_flag,
    sw_in_flag = sw_in_flag,
    sr50_flag = sr50_flag
  )

#add additional flag columns
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

additional_columns <- c("station_name",
                        "cd_year",
                        "JD",
                        "cd_month",
                        "cd_day",
                        "cd_time")

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

flag_col <- c(
  "t_air_flag", #changed this flag column
  "RH_flag", # changed this flag
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
  "t_air_2_flag", #changed this flag
  "RH_2_flag", #changed this flag
  "t_water_flag",
  "t_water_2_flag",
  "water_depth_flag",
  "water_depth_corr_flag"
)

# make sure winter lake is called Winter Lake FTS
climatedf$station_name[climatedf$station_name == "Winter Lake"] <- "Winter Lake FTS"
climatedf$station_name[climatedf$station_name == "Tuktoyaktuk"] <- "Tuk"
climatedf$station_name[climatedf$station_name == "Tibbitt"] <- "Tibbitt muskeg"

#option to save file
#saveRDS(climatedf, "C:/Users/Emma_gregory/Documents/R_Scripts/Packages/nwtclimate/data/allstations_flagged_raw_DBcompatible2025.rds")

#check for duplicates
duplicates <- climatedf %>%
  group_by(station_name, cd_year, cd_month, cd_day, cd_time) %>%
  filter(n() > 1) %>%
  mutate(duplicate_count = n()) %>% # Add count of duplicates in each group
  ungroup()

unique(duplicates$station_name)

duplicate_check <- duplicates %>%
  dplyr::filter(station_name =="Dempster515")
  
# Check if there are any duplicates
if (nrow(duplicates) > 0) {
  print(paste("Number of duplicates found:", nrow(duplicates)))
   
 # Find their row indices in the original dataframe
  duplicate_indices <- which(duplicated(climatedf[c("station_name", "cd_year", "cd_month", "cd_day", "cd_time")]))

  print("Duplicate rows:")
  print(duplicates)

  print("Row indices of duplicates in the original dataframe:")
  print(duplicate_indices)
} else {
  print("No duplicates found.")
}
 
 #remove duplicates
 #sometimes duplicates pop up in the dempster sites because of the download process. it is ok to remove these duplicates
 # climatedf <- climatedf %>%
 #   distinct(station_name, cd_year, cd_month, cd_day, cd_time, .keep_all = TRUE)

#split df function

  split_df <- function(df, additional_columns, main_columns, flag_columns) {
    dfs <- list()

    for (i in seq_along(main_columns)) {
      main_col <- main_columns[i]
      flag_col <- flag_columns[i]

      if (main_col %in% names(df) & flag_col %in% names(df)) {
        #df name will be same as main_columns name - important for removing NAs
        #df_name <- sub("_flag$","", flag_col)
        new_df <- df[c(additional_columns, main_col, flag_col)] 
        dfs[[paste(main_col)]] <- new_df
      } else {
        warning("Main column or flag column missing in dataframe.")
      }
    }

    return(dfs)
  }


  # apply function
  climatedf_list <- split_df(
    df = climatedf,
    additional_columns = additional_columns,
    main_columns = main_columns,
    flag_col = flag_col
  )


# remove all rows where variable is NA in each df
remove_na_rows <- function(climatedf_list) {
  for(df_name in names(climatedf_list)){
    df <- climatedf_list[[df_name]]
    col_name <- df_name

    df <- df[!is.na(df[[col_name]]),] # remove NAs where colname = dfname

    climatedf_list[[df_name]] <- df # update list
  }
  return(climatedf_list)
}

climatedf_list <- remove_na_rows(climatedf_list)

#
# save as separate csv's
savepath <- "C:/Users/emma_gregory/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/separated/2025/"

for (df_name in names(climatedf_list)) {
  df <- climatedf_list[[df_name]]
  filename <- paste0(savepath, df_name, ".csv")
  write.csv(df, file = filename, row.names = FALSE)
  cat("df", df_name, "saved as", filename, "\n")
}

# save as separate csv's - for each station for each parameter
#I added this in because the database didn't appear to everything in when using the larger files created from lines 179-184

# savepath_2 <-"C:/Users/emma_riley/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/separated/bysite/"
# 
# for (df_name in names(climatedf_list)) {
#   df <- climatedf_list[[df_name]]
#   for(station in unique(df$station_name)){
#     df_2 <- df[df$station_name == station, ]
#     filename <- paste0(savepath, df_name, station, ".csv")
#     write.csv(df_2, file = filename, row.names = FALSE)
#   }
#   
# }


# testing above ------------------------------------------

# check names of dfs in df list
print(names(climatedf_list))

#test extraction of df
rain <- climatedf_list[['rain_mm']]

t_air <- climatedf_list[['t_air_high_C']]

t_air_low <- climatedf_list[['t_air_low_C']]

net_LW <- climatedf_list[['net_LW_Wm2']]

net_SW <- climatedf_list[['net_SW_Wm2']]

net_rad <- climatedf_list[['rn_Wm2']]

SR_50 <- climatedf_list[['sr50_cm']]

net_rad <- climatedf_list[['rn_Wm2']]

Inc_rad <- climatedf_list[['sw_in_Wm2']]

unique(Inc_rad$station_name)

winddir <- climatedf_list[['wind_dir_deg']]

windspd <- climatedf_list[['wind_sp_ms']]

t_soil1 <- climatedf_list[['t_soil_1_C']]
t_soil2 <- climatedf_list[['t_soil_2_C']]
t_soil3 <- climatedf_list[['t_soil_3_C']]
t_soil4 <- climatedf_list[['t_soil_4_C']]
t_soil5 <- climatedf_list[['t_soil_5_C']]
t_soil6 <- climatedf_list[['t_soil_6_C']]

Rh_high <- climatedf_list[['RH_high_per']]

Rh_low <- climatedf_list[['RH_low_per']]

t_water2 <- climatedf_list[['t_water_2_C']]
t_water <- climatedf_list[['t_water_C']]

water_depth_m <- climatedf_list[['water_depth_m']]
water_depth_corr_m <- climatedf_list[['water_depth_corr_m']]

# #missing rain values
# 
# rain_missing <- rain %>%
#   dplyr::filter(station_name == "Tibbitt muskeg",
#                 cd_year == 2024, cd_month >=6)
# 
# rain_missing2 <- rain %>%
#   dplyr::filter(station_name == "Tibbitt Pine"|station_name == "Tuk"|
#                   station_name == "Walker Bay"|station_name == "Winter Lake FTS"|
#                   station_name == "YK Ski Club")
# 
# rain_missing <- rbind(rain_missing, rain_missing2)
# 
# t_air_missing <- t_air %>%
#   dplyr::filter(station_name == "Tibbitt muskeg",
#                 cd_year == 2024, cd_month >=6)
# t_air_missing2 <- t_air %>%
#   dplyr::filter(station_name == "Tibbitt Pine"|station_name == "Tuk"|
#                   station_name == "Walker Bay"|station_name == "Winter Lake FTS"|
#                   station_name == "YK Ski Club")
# 
# t_air_missing <- rbind(t_air_missing, t_air_missing2)
# 
# write.csv(rain_missing, "C:/Users/emma_riley.corp/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/separated/updateddata/missing/rain_missing.csv", row.names = F)
# write.csv(t_air_missing, "C:/Users/emma_riley.corp/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/separated/updateddata/missing/t_air_missing.csv", row.names = F)
# 
# sw_inc_missing2 <- Inc_rad %>%
#   dplyr::filter(station_name == "Salmita"|station_name == "Tibbitt Pine"|station_name == "Walker Bay"|station_name == "Winter Lake FTS")
# 
# write.csv(sw_inc_missing2, "C:/Users/emma_riley.corp/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/separated/updateddata/missing/sw_in_missing.csv", row.names = F)
# 
# rh_high_missing <- Rh_high %>%
#   dplyr::filter(station_name == "Silver Bear"|station_name == "Tibbitt Pine"|station_name == "Walker Bay"|station_name == "Winter Lake FTS"|station_name == "YK Ski Club")
# 
# write.csv(rh_high_missing, "C:/Users/emma_riley.corp/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/separated/updateddata/missing/rh_high_missing.csv", row.names = F)
# 
# wsmstns <- c("Taglu","Tibbitt muskeg","Tibbitt Pine","Tuk","Walker Bay","Winter Lake FTS", "YK Ski Club")
# wind_spd_missing <- windspd %>%
#   dplyr::filter(station_name %in% wsmstns)
# 
# write.csv(wind_spd_missing, "C:/Users/emma_riley.corp/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/separated/updateddata/missing/wind_spd_missing.csv", row.names = F)
# 
# waterdepthstns <- c(  "Nanisivik",   "Pocket"   ,   "Salmita"  ,   "Silver Bear")
# water_depth_missing <- water_depth_m %>%
#   dplyr::filter(station_name %in% waterdepthstns)
# 
# write.csv(water_depth_missing, "C:/Users/emma_riley.corp/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/separated/updateddata/missing/water_depth_missing.csv", row.names = F)
# 
# water_depth_adj <- water_depth_m %>%
#   dplyr::filter(station_name == "Nanisivik"|station_name == "Lupin") %>%
#   dplyr::mutate(water_depth_m = water_depth_m/1000)
# 
# write.csv(water_depth_adj, "C:/Users/emma_riley.corp/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/separated/updateddata/missing/water_depth_adj.csv", row.names = F)
# 
# water_depth_check <- water_depth_m %>%
# dplyr::filter(station_name == "Silver Bear") 
# 
# ##################################################
# #load missing data into database
# #For Tibbitt station
# climatedf_Tibbitt <- climatedf %>%
#   dplyr::filter(station_name == "Tibbitt muskeg")
# 
# climatedf_list_tibbitt <- split_df(
#   df = climatedf_Tibbitt,
#   additional_columns = additional_columns,
#   main_columns = main_columns,
#   flag_col = flag_col
# )
# 
# 
# # remove all rows where variable is NA in each df
# remove_na_rows <- function(climatedf_list_tibbitt) {
#   for(df_name in names(climatedf_list_tibbitt)){
#     df <- climatedf_list_tibbitt[[df_name]]
#     col_name <- df_name
#     
#     df <- df[!is.na(df[[col_name]]),] # remove NAs where colname = dfname
#     
#     climatedf_list_tibbitt[[df_name]] <- df # update list
#   }
#   return(climatedf_list_tibbitt)
# }
# 
# climatedf_list_tibbitt <- remove_na_rows(climatedf_list_tibbitt)
# # save as separate csv's
# savepath <- "C:/Users/emma_riley.corp/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/separated/updateddata/missing/"
# 
# for (df_name in names(climatedf_list_tibbitt)) {
#   df <- climatedf_list_tibbitt[[df_name]]
#   filename <- paste0(savepath, df_name, ".csv")
#   write.csv(df, file = filename, row.names = FALSE)
#   cat("df", df_name, "saved as", filename, "\n")
# }
# 
# #load in SW values that are adjusted for Salmita 
# 
# inc_sw_adj <- Inc_rad %>%
#   dplyr::filter(station_name == "Salmita"|station_name == "Daring Lake"|station_name == "Lupin"|station_name=="Nanisivik"|station_name == "Walker Bay"|station_name == "Tibbitt Pine") %>%
#   dplyr::mutate(sw_in_Wm2 = sw_in_Wm2 * 1000)
# 
# write.csv(inc_sw_adj, "C:/Users/emma_riley.corp/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/separated/updateddata/missing/inc_sw_adj.csv", row.names = F)
# 
# unique(Inc_rad$station_name)
