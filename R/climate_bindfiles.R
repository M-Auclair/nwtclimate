# Binding raw waters climate files

path <- "C:/Users/maucl/Documents/Data/csv_data/waters" #set to filepath where data is stored
savepath <- "C:/Users/maucl/Documents/Data/R_data" # path where new file will be saved



# need readxl package installed for below:
#note: the following cannot be performed in a loop, it produces errors that delete data (!)
{BB <- readxl::read_excel(paste0(path, "/BB.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  BDL <- readxl::read_excel(paste0(path, "/BDL.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Colomac <- readxl::read_excel(paste0(path, "/Colomac.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  #Daring_FTS <- readxl::read_excel(paste0(path, "/Daring FTS.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Daring_Lake <- readxl::read_excel(paste0(path, "/Daring Lake.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Dempster85 <- readxl::read_excel(paste0(path, "/Dempster85.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Dempster515 <- readxl::read_excel(paste0(path, "/Dempster515.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Discovery <- readxl::read_excel(paste0(path, "/Discovery.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Giant_Mine <- readxl::read_excel(paste0(path, "/Giant Mine.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  #Harry_FTS <- readxl::read_excel(paste0(path, "/Harry FTS.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Harry <- readxl::read_excel(paste0(path, "/Harry.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  #Hoarfrost_FTS <- readxl::read_excel(paste0(path, "/Hoarfrost FTS.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  #ITH_FTS <- readxl::read_excel(paste0(path, "/ITH FTS.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Lupin <- readxl::read_excel(paste0(path, "/Lupin.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Mile_222 <- readxl::read_excel(paste0(path, "/Mile 222.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Nanisivik <- readxl::read_excel(paste0(path, "/Nanisivik.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  #Peel_FTS <- readxl::read_excel(paste0(path, "/Peel FTS.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Peel <- readxl::read_excel(paste0(path, "/Peel.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Pocket <- readxl::read_excel(paste0(path, "/Pocket.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Salmita <- readxl::read_excel(paste0(path, "/Salmita.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Silver_Bear <- readxl::read_excel(paste0(path, "/Silver Bear.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Snare_Rapids <- readxl::read_excel(paste0(path, "/Snare Rapids.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Taglu <- readxl::read_excel(paste0(path, "/Taglu.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Tibbitt_muskeg <- readxl::read_excel(paste0(path, "/Tibbitt muskeg.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Tibbitt_Pine <- readxl::read_excel(paste0(path, "/Tibbitt Pine.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Tuktoyaktuk <- readxl::read_excel(paste0(path, "/Tuktoyaktuk.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  Walker_Bay <- readxl::read_excel(paste0(path, "/Walker Bay.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  #Winter_Lake_FTS <- readxl::read_excel(paste0(path, "/Winter Lake FTS.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
  YK_Ski_Club <- readxl::read_excel(paste0(path, "/YK Ski Club.xlsx"), col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric","date", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))
}
# reading in new FTS data:
# NOTE: may need to change path for FTS files below to where they are saved on your directory
{Daring_FTS <- readRDS(paste0(savepath, "/Daring FTS.rds"))
  Harry_FTS <- readRDS(paste0(savepath, "/Harry FTS.rds"))
  Hoarfrost_FTS <- readRDS(paste0(savepath, "/Hoarfrost FTS.rds"))
  ITH_FTS <- readRDS(paste0(savepath, "/ITH FTS.rds"))
  Peel_FTS <- readRDS(paste0(savepath, "/Peel FTS.rds"))
  Winter_Lake_FTS <- readRDS(paste0(savepath, "/WinterLake FTS.rds"))

  # applying same convert_columns function as in FTS_bind.R
  convert_classes <- function(x) {

    char_cols <- c("Station","dataflag_AT","dataflag_RH","dataflag_Rn","dataflag_WS","dataflag_WD",
                   "dataflag_ISW","dataflag_SD","dataflag_NR","dataflag_NSW","dataflag_NLW","notes")

    num_cols <- c("Year","JD","Month","Day","Hour","Air Temperature","Relative Humidity","Rainfall",
                  "Wind Speed","Wind direction","Net SW Radiation", "Net LW Radiation","Net Radiation",
                  "Incoming SW Radiation","Snow depth","TS_1","TS_2", "TS_3","TS_4","TS_5", "TS_6",
                  "Air Temperature_2","Relative Humidity_2","Twater","Twater_2","DepthTW","WDep_corr")

    char_cols <- intersect(char_cols, names(x))
    num_cols <- intersect(num_cols, names(x))

    x[char_cols] <- lapply(x[char_cols], as.character)
    x[num_cols] <- lapply(x[num_cols], as.numeric)

    return(x)
  }

  # note ignore "NAs introduced by coercion" msg - due to some NA values recognized as a character "NA" instead of missing value
  Daring_FTS <- convert_classes(Daring_FTS)
  Harry_FTS <- convert_classes(Harry_FTS)
  Hoarfrost_FTS <- convert_classes(Hoarfrost_FTS)
  ITH_FTS <- convert_classes(ITH_FTS)
  Peel_FTS <- convert_classes(Peel_FTS)
  Winter_Lake_FTS <- convert_classes(Winter_Lake_FTS)


}
#Binding the rows from these files together:
climatedf<-dplyr::bind_rows(BB,
                            BDL,
                            Colomac,
                            Daring_FTS,
                            Daring_Lake,
                            Dempster85,
                            Dempster515,
                            Discovery,
                            Giant_Mine,
                            Harry_FTS,
                            Harry,
                            Hoarfrost_FTS,
                            ITH_FTS,
                            Lupin,
                            Mile_222,
                            Nanisivik,
                            Peel_FTS,
                            Peel,
                            Pocket,
                            Salmita,
                            Silver_Bear,
                            Snare_Rapids,
                            Taglu,
                            Tibbitt_muskeg,
                            Tibbitt_Pine,
                            Tuktoyaktuk,
                            Walker_Bay,
                            Winter_Lake_FTS,
                            YK_Ski_Club)
# add lat lon and elev using appendvar function
sites <- read_csv(paste0(path,"/sites.csv"))
lat = appendvar(climatedf$Station, sites$Station, sites$latitude)
lon = appendvar(climatedf$Station, sites$Station, sites$longitude)
elev = appendvar(climatedf$Station, sites$Station, sites$elevation)

# change col names
colnames <- c(
  "station_name",
  "year",
  "JD",
  "month",
  "day",
  "hour",
  "date",
  "t_air",
  "RH",
  "total_precip",
  "wind_sp",
  "wind_dir",
  "net_SW",
  "net_LW",
  "rn",
  "sw_in",
  "sr50",
  "t_soil_1",
  "t_soil_2",
  "t_soil_3",
  "t_soil_4",
  "t_soil_5",
  "t_soil_6",
  "t_air_2",
  "RH_2",
  "t_water",
  "t_water_2",
  "water_depth",
  "t_air_flag",
  "RH_flag",
  "rain_flag",
  "wind_sp_flag",
  "wind_dir_flag",
  "sw_in_flag",
  "sr50_flag",
  "rn_flag",
  "net_SW_flag",
  "net_LW_flag",
  "notes",
  "water_depth_corr",
  "lat",
  "lon",
  "elev")
colnames(climatedf) <- colnames

#Fill in Date, JD and month-year values using lubridate
climatedf$date = dplyr::if_else(is.na(climatedf$date), as.Date(climatedf$JD-1, origin = paste0(climatedf$year, "-01-01")), as.Date(climatedf$date))
climatedf$month =dplyr::if_else(is.na(climatedf$month), lubridate::month(climatedf$date), climatedf$month)
climatedf$day =dplyr::if_else(is.na(climatedf$day), lubridate::day(climatedf$date), as.integer(climatedf$day))
climatedf$JD = dplyr::if_else(is.na(climatedf$JD), lubridate::yday(climatedf$date), climatedf$JD)
climatedf$year = dplyr::if_else(is.na(climatedf$year), lubridate::year(climatedf$date), climatedf$year)
climatedf$hour = dplyr::if_else(is.na(climatedf$hour), lubridate::hour(climatedf$date), as.integer(climatedf$hour))

#remove -6999 or -9999 values
NANvalues = c(-6999, 6999, -99999)
#climatedf[, 8:28][climatedf[, 8:28] %in% NANvalues] <- NA #takes too long to run
climatedf$t_water[climatedf$t_water %in% NANvalues] <- NA
climatedf$RH[climatedf$RH %in% NANvalues] <- NA
climatedf$wind_dir[climatedf$wind_dir %in% NANvalues] <- NA
climatedf$wind_sp[climatedf$wind_sp %in% NANvalues] <- NA
climatedf$rn[climatedf$rn %in% NANvalues] <- NA
climatedf$sw_in [climatedf$sw_in  %in% NANvalues] <- NA
climatedf$sr50 [climatedf$sr50 %in% NANvalues] <- NA
climatedf$t_soil_1 [climatedf$t_soil_1 %in% NANvalues] <- NA
climatedf$t_soil_2 [climatedf$t_soil_2 %in% NANvalues] <- NA
climatedf$t_soil_3 [climatedf$t_soil_3 %in% NANvalues] <- NA
climatedf$t_soil_4 [climatedf$t_soil_4 %in% NANvalues] <- NA
climatedf$water_depth[climatedf$water_depth %in% NANvalues] <- NA

FTS_stns = c("Daring FTS",
             "Harry FTS",
             "Hoarfrost FTS",
             "ITH FTS",
             "Peel FTS",
             "Winter Lake")

#change FTS wind speed values from km hr-1 to m s-1
climatedf$wind_sp = dplyr::if_else(!is.na(climatedf$wind_sp)& climatedf$station_name %in% FTS_stns,  climatedf$wind_sp/3.6, climatedf$wind_sp, missing = climatedf$wind_sp)

# flag data & prep for climate db
# remove Walker Bay & other rows of NAs
climatedf <- climatedf %>%
  dplyr::filter((is.na(hour)==FALSE & is.na(date) == FALSE))

# remove trailing zeros from hour col, replace 24 with 0
climatedf$hour <- ifelse(climatedf$hour == "0", climatedf$hour, sub("0+$", "", climatedf$hour))
climatedf$hour[climatedf$hour == "24"] <- "0"

# Hour & Time columns
climatedf$time <- lubridate::ymd_hms(base::paste(climatedf$date,base::sprintf("%02d:00:00",climatedf$hour)))
climatedf$hour <- base::sprintf("%02d:00", climatedf$hour)
climatedf <- climatedf[, !names(climatedf) %in% c("time")]

# note flag data function is in "dependencies_functions.R"
climatedf_flagged <- flag_data(climatedf)

# save file in savepath defined above
saveRDS(climatedf, paste0(savepath, "/allstations_flagged_raw.rds"))



