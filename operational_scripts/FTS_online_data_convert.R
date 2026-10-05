#Online FTS data processing
#File conversion for database compatibility

#load req. libraries
library(readxl)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)

#set pathways
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME"))) 
data_path <- paste0(user, "/Documents/NT_Hydrology/Abiotic Monitoring/Data/FTSdata/Forestry/NWT_ENR_Forestry/")
package_path <- paste0(user, "/Documents/R_Scripts/Packages/") #added /Documents/ here before R_Scripts
nwtclimate_path <- paste0(package_path, "nwtclimate/")
R_path <- paste0("R/")

#source necessary functions
source(paste0(nwtclimate_path, R_path, "dependencies_functions.R"))

#opposite of %in%:
`%!in%` = Negate(`%in%`)

#bring in data
csv_files <- list.files(path = data_path, pattern = "\\.csv$", full.names = TRUE) 

# Read and combine all csv files
# Define a common set of standardized column names (adjust this to match expected structure)
column_names <- c("pres_tend_char_pst3hrs",
                  "avg_globl_solr_radn_pst1hr",
                  "max_wnd_spd_10m_pst1hr",
                  "max_rel_hum_pst1hr",
                  "cum_rnfl_amt",
                  "wnd_dir_pst1hr_max_spd",
                  "pres_tend_amt_pst3hrs",
                  "min_air_temp_pst1hr",
                  "rnfl_amt_pst6hrs",
                  "avg_air_temp_pst1hr",
                  "min_rel_hum_pst24hrs",
                  "max_rel_hum_pst24hrs",
                  "avg_wnd_spd_10m_pst1hr",
                  "avg_rel_hum_pst1hr",
                  "rnfl_amt_pst24hrs",
                  "rnfl_amt_pst1hr",
                  "rnfl_amt_pst12hrs",
                  "avg_stn_pres_pst1hr",
                  "max_air_temp_pst24hrs",
                  "rnfl_amt_pst3hrs",
                  "avg_wnd_dir_pst1hr",
                  "min_rel_hum_pst1hr",
                  "min_air_temp_pst24hrs",
                  "avg_mslp_pst1hr",
                  "max_air_temp_pst1hr",
                  "avg_wnd_spd_10m_pst1hrA"
)

#Read and clean each file
read_and_clean <- function(file_path) {
  header <- read_csv(file_path, skip = 1, n_max = 1, col_names = FALSE, show_col_types = FALSE) %>%
    unlist(use.names = FALSE) %>%
    make.names(unique = TRUE)
  
  df <- read_csv(file_path, skip = 2, col_names = header, show_col_types = FALSE)
  
  # Convert all columns to character to ensure binding works
  df <- df %>% mutate(across(everything(), as.character))
  
  # Replace "None" with NA in all character columns
  df <- df %>% mutate(across(everything(), ~ na_if(., "None")))
  
  # Add file name
  df$file_source <- basename(file_path)
  
  return(df)
}

# Read all files individually
all_dfs <- map(csv_files, read_and_clean)

# Get union of all column names
all_columns <- unique(unlist(map(all_dfs, names)))

# Ensure all data frames have the same columns, pad missing ones with NA
standardized_dfs <- map(all_dfs, function(df) {
  missing_cols <- setdiff(all_columns, names(df))
  df[missing_cols] <- NA
  df <- df[, all_columns]
  return(df)
})

all_data <- bind_rows(standardized_dfs)

if ("time" %in% names(all_data)) {
  all_data <- all_data %>%
    mutate(
      time = parse_date_time(time, orders = c("ymd HMS", "ymd HM", "ymd H", "ymd", "Ymd HMS")),
      across(
        .cols = -c(time, file_source),
        .fns = ~ as.numeric(.)
      )
    )
}

# Coerce columns: parse time and convert all others to numeric
if ("time" %in% names(all_data)) {
  all_data <- all_data %>%
    mutate(
      time = parse_date_time(time, orders = c("ymd HMS", "ymd HM", "ymd H", "ymd", "Ymd HMS")),
      across(
        .cols = -c(time, file_source),
        .fns = ~ suppressWarnings(as.numeric(.))
      )
    )
} else {
  all_data <- all_data %>%
    mutate(
      across(
        .cols = -file_source,
        .fns = ~ suppressWarnings(as.numeric(.))
      )
    )
}

#remove .csv from column "source name"
all_data <- all_data %>%
  mutate(file_source = str_remove(file_source, "\\.csv$"))

#rename column names to those expected in climate_bindfiles.R lines
all_data <- all_data %>%
  dplyr::mutate("year" = lubridate::year(time),
                "JD" = lubridate::yday(time),
                "month" = lubridate::month(time),
                "day" = lubridate::day(time),
                "hour" = lubridate::hour(time)) %>%
  dplyr::rename("station_name"="file_source",
                "date" = "time",
                "t_air" = "avg_air_temp_pst1hr",
                "RH" = "avg_rel_hum_pst1hr",
                "total_precip" = "rnfl_amt_pst1hr",
                "wind_sp" = "avg_wnd_spd_10m_pst1hr",
                "wind_dir" = "avg_wnd_dir_pst1hr")

all_data$wind_sp <- ifelse(is.na(all_data$wind_sp) == TRUE, all_data$avg_wnd_spd_10m_pst1hrA, all_data$wind_sp)

all_data <- all_data %>%
  dplyr::select(-c(avg_wnd_spd_10m_pst1hrA, rnfl_amt_pst6hrs, min_rel_hum_pst1hr,
                   rnfl_amt_pst3hrs, rnfl_amt_pst24hrs, rnfl_amt_pst12hrs,
                   min_rel_hum_pst24hrs, min_air_temp_pst24hrs,
                   min_air_temp_pst1hr, max_wnd_spd_10m_pst1hr, max_rel_hum_pst24hrs,
                   max_rel_hum_pst1hr, max_air_temp_pst24hrs, max_air_temp_pst1hr,
                   pres_tend_amt_pst3hrs, pres_tend_char_pst3hrs, avg_mslp_pst1hr,
                   avg_stn_pres_pst1hr, wnd_dir_pst1hr_max_spd, cum_rnfl_amt))

#add in other variable columns (all values will be NA)
all_data["sw_in"]<- NA
all_data["RH_2"]<- NA
all_data["t_air_2"]<-NA
all_data["net_SW"]<-NA
all_data["net_LW"]<-NA
all_data["rn"]<-NA
all_data["sr50"]<-NA
all_data["t_water"]<-NA
all_data["t_water_2"]<-NA
all_data["water_depth"]<-NA
all_data["t_air_flag"]<-NA
all_data["RH_flag"]<-NA
all_data["rain_flag"]<-NA
all_data["wind_sp_flag"]<-NA
all_data["wind_dir_flag"]<-NA
all_data["sw_in_flag"]<-NA
all_data["sr50_flag"]<-NA
all_data["rn_flag"]<-NA
all_data["net_SW_flag"]<-NA
all_data["net_LW_flag"]<-NA
all_data["t_soil_1"]<-NA
all_data["t_soil_2"]<-NA
all_data["t_soil_3"]<-NA
all_data["t_soil_4"]<-NA
all_data["t_soil_5"]<-NA
all_data["t_soil_6"]<-NA
all_data["notes"]<-NA
all_data["water_depth_corr"]<-NA

climatedf <- all_data

#remove -6999 or -9999 values
NANvalues = c(-6999, 6999, -99999)
climatedf$t_air[climatedf$t_air %in% NANvalues] <- NA
climatedf$total_precip[climatedf$total_precip %in% NANvalues] <- NA
climatedf$RH[climatedf$RH %in% NANvalues] <- NA
climatedf$wind_dir[climatedf$wind_dir %in% NANvalues] <- NA
climatedf$wind_sp[climatedf$wind_sp %in% NANvalues] <- NA

#change FTS wind speed values from km hr-1 to m s-1
climatedf$wind_sp = dplyr::if_else(!is.na(climatedf$wind_sp),  climatedf$wind_sp/3.6, climatedf$wind_sp, missing = climatedf$wind_sp)

#Bring in QAQC df (for manual QAQC melting snow in tipping bucket)
file_path <- "C:/Users/emma_gregory/Documents/NT_Hydrology/Abiotic Monitoring/Data/cleaned"
file_name <- "QAQCraindf.rds"
QAQCraindf<- readRDS(paste0(file_path, "/", file_name ))

#create data flags for data
climatedf_flagged <- flag_data(climatedf)

# Hour & Time columns
climatedf_flagged$hour <- base::sprintf("%02d:00", climatedf_flagged$hour)
climatedf_flagged$time <- climatedf_flagged$hour

#make sure rain check flags are transitioned to "rain_flag"
climatedf_flagged <- climatedf_flagged %>%
  dplyr::mutate(rain_flag = ifelse(!is.na(Rain_check_flag) & Rain_check_flag == "QC", "QC_PSM", rain_flag))

# remove Rain_check_flag column if present
climatedf_flagged <- climatedf_flagged[, !names(climatedf_flagged) %in% c("Rain_check_flag")]

#rename time column
climatedf_flagged<- climatedf_flagged %>%
  dplyr::rename("cd_time"="time")

#rename to match database fieldnames
climatedf_flagged <- climatedf_flagged %>%
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

climatedf_flagged[,new_flag_cols] <- NA

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

#remove duplicates
climatedf_flagged <- climatedf_flagged %>%
  distinct(station_name, cd_year, cd_month, cd_day, cd_time, .keep_all = TRUE)

#

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
  df = climatedf_flagged,
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
savepath <- "C:/Users/emma_riley/Documents/NT_Hydrology/Abiotic Monitoring/Data/raw/forDB/FTS/"

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