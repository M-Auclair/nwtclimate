# Climate stations map

library(data.table)
library(leaflet)

# --- Load stations ---------------------------------------------------------
connection <- odbc::dbConnect(drv = odbc::odbc(), dsn = "ECC_CLIMATE", uid = Sys.getenv("ORACLE_CLIM_USER"), pwd = Sys.getenv("ORACLE_CLIM_PWD"))
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")), "/Documents/")
save_path <- paste0(user, "R_Scripts/Packages/nwtclimate/data/")

#stations table
cd_stations<-odbc::dbGetQuery(connection, "SELECT * FROM ECC_CLIMATE.CD_STATIONS")

cd_stations <- as.data.table(cd_stations)

# --- Station type labels & colours -----------------------------------------
cd_stations[, stn_type := fifelse(STATION_TYPE_ID == 1, "FTS", "Campbell Scientific")]

type_colours <- c("FTS" = "#1b7837", "Campbell Scientific" = "#762a83")
cd_stations[, colour := type_colours[stn_type]]

# --- Activity (active vs decommissioned) → opacity -------------------------
cd_stations[, active := is.na(END_YEAR)]
cd_stations[, opacity := fifelse(active, 1.0, 0.35)]

# --- Popup labels ----------------------------------------------------------
cd_stations[, popup := paste0(
  "<b>", STATION_NAME, "</b><br>",
  "Type: ", stn_type, "<br>",
  "Start: ", fifelse(START_YEAR == -99, "Unknown", as.character(START_YEAR)), "<br>",
  "End: ",   fifelse(is.na(END_YEAR), "Active", as.character(END_YEAR)), "<br>",
  "Elev: ",  fifelse(is.na(ELEVATION), "—", paste0(ELEVATION, " m"))
)]

# --- Build leaflet map -----------------------------------------------------
map <- leaflet(cd_stations) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng          = ~LONGITUDE,
    lat          = ~LATITUDE,
    radius       = 6,
    color        = ~colour,
    fillColor    = ~colour,
    fillOpacity  = ~opacity,
    opacity      = ~opacity,
    stroke       = TRUE,
    weight       = 1,
    popup        = ~popup
  ) %>%
  addLegend(
    position = "bottomright",
    colors   = type_colours,
    labels   = names(type_colours),
    title    = "Station Type",
    opacity  = 1
  )

map
