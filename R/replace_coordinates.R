#' Amend coordinates of select site
#'
#' @param df dataframe
#' @param site Site of coordinates to change (in quotations)
#' @param lat New latitude value of the site
#' @param lon New longitude value of the site
#'
#' @return dataframe with updated coordinates for input site
#' @examples
#' # Example usage:
#' df <- replace_coordinates(df, "Fort McPherson", 67.45, -134.88)
#' @export
#'



replace_coordinates <- function(df, site, lat, lon) {
  # Check if the site exists in the merged_name column
  if (site %in% df$merged_name) {
    # Find rows where merged_name matches the specified site
    site_rows <- df$merged_name == site
    # Replace lat and lon values for the specified site
    df$lat[site_rows] <- lat
    df$lon[site_rows] <- lon
  } else {
    warning(paste(site, " not found in the data frame. Check spelling, and use the name of the site instead of the station name"))
  }
  return(df)
}
