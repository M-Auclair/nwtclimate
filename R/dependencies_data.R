# Data required to run functions

# Create a list of NWT communities to include
# Ensure that each community listed here has stations identified in the stitch() function
# Define station_id of all stations.
## Ensure that the order is the correct order to have stitched

locations <- c(
  rep("Aklavik", 3),
  rep("Athabasca", 5),
  rep("Cape Providence", 1),
  rep("Colville Lake", 2),
  rep("Deadmen Valley", 1),
  rep("Deline", 3),
  rep("Fort Chipewyan", 4),
  rep("Fort Good Hope", 4),
  rep("Fort Liard", 3),
  rep("Fort McPherson", 4),
  rep("Fort Nelson", 6),
  rep("Fort Providence", 2),
  rep("Fort Reliance", 2),
  rep("Fort Simpson", 7),
  rep("Fort Smith", 6),
  rep("Fort St John", 7),
  rep("Gameti", 3),
  rep("Hanbury River", 1),
  rep("Hay River", 5),
  rep("High Level", 5),
  rep("Holman", 2),
  rep("Inner Whalebacks", 1),
  rep("Inuvik", 7),
  rep("Lindburg Landing", 1),
  rep("Little Chicago", 1),
  rep("Lower Carp Lake", 1),
  rep("Mackenzie", 4),
  rep("Mould Bay", 3),
  rep("Nangmagvik", 1),
  rep("Norman Wells", 3),
  rep("Paulatuk", 3),
  rep("Peace River", 4),
  rep("Pelly Island", 1),
  rep("Rabbit Kettle", 1),
  rep("Sachs Harbour", 3),
  rep("Sambaa Ke", 1),
  rep("Thomsen River", 1),
  rep("Trail Valley", 1),
  rep("Tulita", 2),
  rep("Tuktoyaktuk", 6),
  rep("Tuktut Nogait", 1),
  rep("Whati", 1),
  rep("Yellowknife", 5),
  rep("Yohin", 1)
)

ids <- c(
  52962, 1623, 1624, # Aklavik
  47047, 2459, 2466, 2467, 2470, # Athabasca
  53298, # Cape Providence
  10867, 52899, # Colville Lake
  31187, # Deadmen Valley
  27749, 6850, 52964, # Deline
  50757, 2704, 2703, 31608, # Fort Chipewyan
  27549, 1644, 1645, 53580, # Fort Good Hope
  10687, 1646, 52965, # Fort Liard
  52963, 1648, 1647, 1649, # Fort McPherson
  54098, 50819, 1455, 8248, 1456, 1458, # Fort Nelson
  10902, 1651, # Fort Providence
  8935, 1652, # Fort Reliance
  41944, 1656, 1655, 52780, 54159, 27609, 1657, # Fort Simpson
  41884, 1660, 1659, 1658, 53119, 54218, # Fort Smith
  55198, 50837, 1413, 1411, 1388, 50997, 1414, # Fort St John
  10926, 27798, 52966, # Gameti
  10897, # Hanbury River
  41885, 1664, 1663, 52600, 54140, # Hay River
  55118, 2726, 49928, 2725, 2727, # High Level
  29715, 1788, # Holman
  10209, # Inner Whalebacks
  41883, 1669, 31470, 51477, 8938, 27222, 1670, # Inuvik
  27608, # Lindburg Landing
  10760, # Little Chicago
  27610, # Lower Carp Lake
  51517, 1423, 1425, 1424, # Mackenzie
  27324, 1792, 10908, # Mould Bay
  27118, # Nangmagvik
  43004, 1680, 50717, # Norman Wells
  26986, 1685, 53420, # Paulatuk
  52258, 2770, 2772, 2774, # Peace River
  10091, # Pelly Island
  26895, # Rabbit Kettle
  10076, 1794, 53326, # Sachs Harbour
  10880, # Sambaa Ke
  10598, # Thomsen River
  27620, # Trail Valley
  26987, 1700, 1699, 1698, 53582, 10845, # Tuktoyaktuk
  52967, 1650, # Tulita
  27626, # Tuktut Nogait
  1674, # Whati,
  55358, 51058, 1706, 27338, 45467, # Yellowknife
  1635 # Yohin
)

df_locations <- data.frame(location = locations,
                   station_id = ids)

###########################################

# Station operators

# ECCC <- read.csv("C:/Users/Ryan_Connon/Documents/NT_Hydrology/Climate/MSC-AWS_Station Metadata_NWT_02-07-2023.csv")[,3]

# NAVCAN <- c(
#   52780, # Fort Simpson
#   1656,  # Fort Simpson
#   53119, # Fort Smith
#   1660,  # Fort Smith
#   52600, # Hay River
#   1664,  # Hay River
#   51477, # Inuvik
#   31470, # Inuvik
#   1669,  # Inuvik
#   50717, # Norman Wells
#   1680,  # Norman Wells
#   53326, # Sachs Harbour
#   1794,  # Sachs Harbour
#   51058, # Yellowknife
#   1706   # Yellowknife
#
# )

############################################################################################


# How to update the data sheet with data from new locations

# weathercan::stations_search("Mackenzie", interval = "day")
# ids <- c(47047, 2459, 2466, 2467, 2470)
#
# df <- weathercan::weather_dl(station_id = ids,
#                              start = "1990-01-01",
#                              end = "2024-12-31",
#                              interval = "day")
#
# df[,12] <- as.numeric(unlist(df[,12]))
# df[,13] <- as.numeric(unlist(df[,13]))
# df[,14] <- as.numeric(unlist(df[,14]))
#
# data <- readRDS(paste0(data_path, updated_data, extension))
# data <- dplyr::bind_rows(data, df)
# saveRDS(data, file = paste0(data_path, updated_data, extension))
#
#

############################################################################################
waterssites = c("Blueberry",
                "BDL",
                "Colomac",
                "Daring FTS",
                "Daring Lake",
                "Dempster515",
                "Dempster85",
                "Discovery",
                "Giant Mine",
                "Harry FTS",
                "Harry",
                "Hoarfrost FTS",
                "ITH FTS",
                "Lupin",
                "Mile222",
                "Nanisivik",
                "Peel FTS",
                "Peel",
                "Pocket",
                "Salmita",
                "Silver Bear",
                "Snare Rapids",
                "Taglu",
                "Tibbitt",
                "Tibbitt Pine",
                "Tuktoyaktuk",
                "Walker Bay",
                "Winter Lake",
                "YK Ski Club",
                "Scotty Creek",
                "Powder Lake",
                "Yaltea Lake",
                "Ninelin Lake",
                "Forestry Lake",
                "Wrigley")

