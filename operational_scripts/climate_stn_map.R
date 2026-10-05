#bring in sites from local backup

sites <- read.csv("C:/Users/emma_riley.CORP/Documents/NT_Hydrology/Abiotic Monitoring/Data/sites.csv")

# Define `%>%` operator in current environment

`%>%` <- magrittr::`%>%`

map <- leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap.Mapnik, group = "Open Street Map") %>% 
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI Aerial") %>% 
  leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "CartoDB")  %>%
  leaflet::addLayersControl(
    baseGroups = c("CartoDB", "Open Street Map", "ESRI Aerial"),
    options = leaflet::layersControlOptions(collapsed = T))

map <- map %>%
  leaflet::addCircleMarkers(data = sites, 
                            lng = sites$longitude,
                            lat = sites$latitude,
                            fillColor = ifelse(sites$activity == "A", "darkblue", "orange"),
                            fill = TRUE,
                            fillOpacity = 1,
                            label = sites$Station,
                            color = "black",
                            radius = 10,
                            weight = 1,
                            opacity = 1,
                            popup =~ paste0("Site name: ", sites$Station, "<br>",
                                            "Activity: ", sites$activity))

map <- map %>%
  leaflet::addLegend(position = "bottomleft",
                     values = c("A", "IA"),
                     labels = c("Active", "Inactive"),
                     colors = c("darkblue", "orange"))
map


