#' Map station data
#'
#' Produce a map of all station data
#' @param parameter The parameter to map
#' @param select_year The year to map
#' @param months The months to map
#' @param start_year The first year to map
#' @param max_missing_days The maximum number of missing days over the time period
#' @param percent_of_normal Logical. Determines whether map produces percent of normal or absolute value
#' @param years_of_record Numeric. The minimum number of years of record for station to appear
#' @param circle_radius Numeric. The radius of the circle in leaflet
#' @param water_year Logical. Determines whether map uses a water year or calendar year
#' @param filename Character. The name of the file to save
#' @return A map of all stations in the NWT
#' @export

map_station_data <- function(
    parameter = "rain",
    select_year = 2023, 
    months = c(5:9),
    start_year = NULL,
    max_missing_days = 20, 
    percent_of_normal = TRUE,
    years_of_record = 5,
    circle_radius = 10,
    water_year = TRUE,
    filename = NA
) 
  
{
  
  savepath_climate <- "C:/Users/Emma_riley/Documents/NT_Hydrology/Climate/Figures/"
  
  # if(exists("cd_rds") == F) {
    
    directory = "C:/Users/Emma_riley/Documents/NT_Hydrology/Climate/ECCC_station_data/"
    merged_data = "merged_data/ECCC_Climate_Data_Merged.rds"
    cd_rds <- readRDS(paste0(directory, merged_data)) %>%
      dplyr::mutate(date = as.Date(date),
                    year = as.numeric(year),
                    month = as.numeric(month),
                    day = as.numeric(day),
                    merged_name = as.character(merged_name))
    
  # }
  
  winter_months <- c(1:4, 10:12)
  this.year <- lubridate::year(Sys.Date())
  this.month <- lubridate::month(Sys.Date())
  this.day <- lubridate::day(Sys.Date())
  
  if(percent_of_normal == T & grepl(paste0("(?i)", parameter), "Mean Temperature") == T) {
    legend_units <- " Anomaly"
  } else if(percent_of_normal == T) {
    legend_units <- " (% of normal)"
  } else if(percent_of_normal == F & grepl(paste0("(?i)", parameter), "Snow Depth") == T) {
    legend_units <- " (cm)"
  } else if(percent_of_normal == F & grepl(paste0("(?i)", parameter), "Snow Depth") == F) {
    legend_units <- " (mm)"
  } else if(percent_of_normal == F & grepl(paste0("(?i)", parameter), "Mean Temperature") == T) {
    legend_units <- " (\u00B0C)"
  }
  
  # Define parameters for plot titles:
  if(grepl(paste0("(?i)", parameter), "Precipitation") == T)  {
    legend_title <- paste0("Precipitation", legend_units)
    parameter <- "total_precip"
    variable <- "precipitation"
    normal_units <- " (mm)"
    operator <- "sum"
  } else if(grepl(paste0("(?i)", parameter), "SWE") == T) {
    legend_title <- paste0("SWE", legend_units)
    parameter <- "SWE_mm"
    variable <- "SWE"
    normal_units <- " (mm)"
    operator <- "sum"
  } else if(grepl(paste0("(?i)", parameter), "Snow Depth") == T) {
    legend_title <- paste0("Snow Depth", legend_units)
    parameter <- "snow_depth_cm"
    variable <- "snow depth"
    normal_units <- " (cm)"
    operator <- "sum"
  } else if(grepl(paste0("(?i)", parameter), "Rainfall") == T) {
    legend_title <- paste0("Rain", legend_units)
    parameter <- "rain_mm"
    variable <- "rain"
    normal_units <- " (mm)"
    operator <- "sum"
  } else if(grepl(paste0("(?i)", parameter), "Mean Temperature") == T) {
    legend_title <- paste0("Temperature <br>", legend_units)
    parameter <- "mean_temp"
    variable <- "temperature"
    normal_units <- " (\u00B0C)"
    operator <- "mean"
  }
  
  if(select_year == this.year & (this.month < max(months)) & water_year == FALSE) {
    stop("No data available. You have selected the current year, but with months in the future")
  }
  
  if(percent_of_normal == T & parameter == "mean_temp") {
    legend_numbers <-  c(-100, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 100)
    legend_labels <-  c("< -2 \u00B0C", "-1.5 \u00B0C", "-1 \u00B0C", 
                        "-0.5 \u00B0C", "0 \u00B0C", "+0.5\u00B0C", 
                        "+1\u00B0C", "+1.5\u00B0C", "+2 \u00B0C", "> 2\u00B0C")
  } else if(percent_of_normal == T) {
    legend_numbers <-  c(0, 50, 70, 90, 110, 130, 150, 200, 10000)
    legend_labels <-  c("< 50", "50 - 69", "70 - 89", "99 - 109", "110 - 129", 
                        "130 - 149", "150 - 199", "> 200")
  } else {
    legend_numbers <- c(0, 30, 50, 70, 90, 110, 130, 150, 10000)
    legend_labels <-  c("< 30", "30 - 49", "50 - 69", "70 - 89", "90 - 109", 
                        "110 - 129", "130 - 149", "> 150")
  }
  
  # Ensure there is only one lat/lon per site
  data_adj <- NULL
  for(i in seq_along(unique(cd_rds$merged_name))) {
    data_parse <- cd_rds %>%
      dplyr::filter(merged_name == unique(cd_rds$merged_name)[i],
                    year >= start_year,
                    month %in% months) %>%
      dplyr::mutate(lat = tail(lat, n = 1),
                    lon = tail(lon, n = 1))
    data_adj <- dplyr::bind_rows(data_adj, data_parse)
  }
  
  data <- data_adj %>%
    dplyr::select(merged_name, lat, lon, date, year, month, day, mean_temp, total_precip, total_snow) %>%
    dplyr::mutate(SWE_mm = ifelse(mean_temp < 0, total_precip, 0),
                  snow_depth_cm = total_snow,
                  rain_mm = ifelse(mean_temp >= 0, total_precip, 0),
                  water_year = wtr_yr(date, 10)) %>%
    dplyr::relocate(water_year, .before = year) %>%
    dplyr::filter(lat < 69.5)
  
  if(water_year == TRUE) {
    data <- data %>%
      dplyr::filter(month %in% winter_months) %>%
      dplyr::select(-year) %>%
      dplyr::rename(year = water_year)
  }
    
    if(select_year == this.year & this.month %in% months) {
      if(water_year == T) {
        
        if(this.month == 10) {
          data <- data %>%
            dplyr::filter(month == 10)
        } else if(this.month == 11) {
          data <- data %>%
            dplyr::filter(month %in% c(10:11))
        } else if(this.month == 12) {
          data <- data %>%
            dplyr::filter(month %in% c(10:12))
        } else if(this.month == 1) {
          data <- data %>%
            dplyr::filter(month %in% c(10:12, 1))
        } else if(this.month == 2) {
          data <- data %>%
            dplyr::filter(month %in% c(10:12, 1:2))
        } else if(this.month == 3) {
          data <- data %>%
            dplyr::filter(month %in% c(10:12, 1:3))
        } else if(this.month == 4) {
          data <- data %>%
            dplyr::filter(month %in% c(10:12, 1:4))
        }
        
      } else {
        
        data <- data %>%
          dplyr::filter(month <= this.month)
        
      }
      
      data <- data %>%
        dplyr::mutate(new_day = ifelse(month == this.month, day, NA)) %>%
        dplyr::filter(is.na(new_day) | new_day <= this.day) %>%
        dplyr::select(-new_day)
      
    }
      
      df_summary <- data %>%
        dplyr::group_by(merged_name, year) %>%
        dplyr::summarize(value_normal = ifelse(get({{operator}})(is.na(get({{parameter}}))) > max_missing_days, 
                                               NA, 
                                               get({{operator}})(get({{parameter}}), na.rm = T)),
                         .groups = "drop_last") 
      
      df_normal <- df_summary %>%
        dplyr::filter(!is.na(value_normal)) %>%
        dplyr::group_by(merged_name) %>%
        dplyr::summarize(value_normal = round(mean(value_normal), 1), 
                         record_length = length(merged_name),
                         .groups = "drop") %>%
        dplyr::filter(record_length >= years_of_record)
      
      station_list <- unique(df_normal$merged_name)
      
      df_select_year <- data %>%
        dplyr::filter(merged_name %in% station_list) %>%
        dplyr::filter(year == select_year) %>%
        dplyr::group_by(merged_name, lat, lon, year) %>%
        dplyr::summarize(value_select_year = ifelse(sum(is.na(get({{parameter}}))) > max_missing_days, 
                                                    NA, 
                                                    get({{operator}})(get({{parameter}}), na.rm = T)),
                         .groups = "drop") %>%
        dplyr::mutate(value_select_year = round(value_select_year, 1))
      
      df_rank <- df_summary %>%
        dplyr::filter(merged_name %in% station_list) %>%
        dplyr::mutate(value_rank = rank(value_normal, na.last = "keep")) %>%
        dplyr::filter(year == select_year) %>%
        dplyr::select(merged_name, value_rank) 
      
      df <- dplyr::left_join(df_normal, df_select_year, by = "merged_name") %>%
        dplyr::left_join(., df_rank, by = "merged_name") 
      
      if(parameter == "mean_temp") {
        df <- df %>%
          dplyr::mutate(value_percent_normal = round((value_select_year - value_normal), 1)) %>%
          dplyr::filter(!is.na(value_percent_normal)) %>%
          dplyr::rename(longitude = lon, latitude = lat)
      } else {
        df <- df %>%
          dplyr::mutate(value_percent_normal = round((value_select_year / value_normal) * 100, 1)) %>%
          dplyr::filter(!is.na(value_percent_normal)) %>%
          dplyr::rename(longitude = lon, latitude = lat)
      }
      
      if(percent_of_normal == T) {
      df$bin <- cut(df$value_percent_normal,
                    legend_numbers, 
                    include.lowest = T,
                    labels = legend_labels)
      } else {
        df$bin <- cut(df$value_select_year,
                      legend_numbers, 
                      include.lowest = T,
                      labels = legend_labels)
      }
        
      if(parameter == "mean_temp") {
      bin_colours <- leaflet::colorFactor(palette = "RdYlBu", df$bin, reverse = T)
      } else {
        bin_colours <- leaflet::colorFactor(palette = "RdYlBu", df$bin)
      }
      
      # Add NA values for empty bins so that the legend includes all bins
      new_values <- levels(df$bin)[!c(levels(df$bin) %in% unique(df$bin))]
      if(length(new_values) > 0) {
        
        m <- matrix(ncol = ncol(df) - 1, nrow = length(new_values))
        df_add <- data.frame(m, new_values)
        colnames(df_add) <- colnames(df)
        df <- dplyr::bind_rows(df, df_add)
        df$bin <- factor(df$bin, levels = legend_labels)
        
      }
      
      # Define proj variable to project in Leaflet
      proj <- '+proj=longlat +datum=WGS84'
      NWT <- sf::st_read("C:/Users/ryan_connon/Documents/Shapefiles/NWT_Boundary/NWT_ENR_BND_FND.shp",
                         layer = "NWT_ENR_BND_FND")
      NWT <- sf::st_transform(NWT, sp::CRS(proj))
      NWT <- sf::st_zm(NWT)
    
    map <- leaflet::leaflet() %>%
      leaflet::addPolygons(data = NWT, color = "black", weight = 2, opacity = 0.5, fillOpacity = 0, group = "Mackenzie Basin") %>%
      leaflet::addLayersControl(
        baseGroups = c("CartoDB", "Open Street Map", "ESRI Aerial"),
        overlayGroups = c("NWT Boundary"),
        options = leaflet::layersControlOptions(collapsed = T)) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "CartoDB") %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap.Mapnik, group = "Open Street Map") %>% 
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI Aerial") %>%
      leaflet::addLayersControl(
        baseGroups = c("CartoDB", "Open Street Map", "ESRI Aerial"),
        options = leaflet::layersControlOptions(collapsed = T)) %>%
        leaflet::addCircleMarkers(data = df,
                                  color = "black", 
                                  fillColor = ~bin_colours(bin),
                                  radius = circle_radius, 
                                  label = ~merged_name,
                                  weight = 1,
                                  opacity = 0.8,
                                  fillOpacity = 0.8,
                                  popup =~ paste0("Station name: ", merged_name, "<br>",
                                                  select_year, " ", variable, " ", legend_title, " ", value_percent_normal, "<br>",
                                                  select_year, " ", variable, normal_units, ": ", value_select_year, "<br>",
                                                  "Average ", variable, normal_units, " ", value_normal, "<br>",
                                                  "Years of record: ", record_length, "<br>",
                                                  "Rank: ", value_rank, "<br>")) %>%
      leaflet::addLegend(position = 'bottomright', pal = bin_colours, values = df$bin,
                         title = legend_title,
                         opacity = 1)  %>%
      leaflet::addScaleBar()
    
    if(is.na(filename)) {
      filename <- paste0(select_year, "_")
    } 
    
    htmlwidgets::saveWidget(map, file = paste0(savepath_climate, filename, ".html"))
    
    map
    
}
