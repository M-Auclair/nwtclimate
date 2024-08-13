# API connection test - with baby names dataset (gnwt)

# libraries / packages needed: httr, and jsonlite

library(httr)
library(jsonlite)

test = GET("https://opendata.gov.nt.ca/dataset/top-baby-names-of-2020")

test #content-type = text/html; charset=uft-8


# extract headers from test
testheaders <- test[['headers']]

test2 <- as.data.frame(test2)


test2 = GET("https://opendata.gov.nt.ca/dataset/0b257f5a-c849-4273-98ac-357ef50c8774/resource/3e453dc1-a58e-48c5-a296-6696e61101e6/download/top-baby-names-with-metadata.xlsx")



# Action API items

#1. URL always begins with:
# https://opendata.gov.nt.ca/api/3/action/

#2. followed by the action item: create (to create), upsert (update/insert), search(query), or search_sql(query w SQL)

#3. then "=API id" goes within the URL after action item^

# example: baby names API ID:
# 3e453dc1-a58e-48c5-a296-6696e61101e6

# 4. then "&"



# testing it out:

base_url <- "https://opendata.gov.nt.ca/api/3/action/"
api_id <- "3e453dc1-a58e-48c5-a296-6696e61101e6"


api_url <- paste0(base_url, "?id=", api_id)

response <- GET(api_url)

# Check if the request was successful
if (http_status(response)$category == "Success") {
  # Parse the JSON content into a list
  data_list <- content(response, "text") %>%
    fromJSON(flatten = TRUE)

  # Extract the relevant part of the data (this will vary based on API structure)
  dataset <- data_list$result$resources

  # If the data is in a CSV file, download it
  csv_url <- dataset$url[dataset$format == "CSV"]
  if (length(csv_url) > 0) {
    csv_response <- GET(csv_url)
    if (http_status(csv_response)$category == "Success") {
      temp_file <- tempfile(fileext = ".csv")
      writeBin(content(csv_response, "raw"), temp_file)
      dataset <- read_csv(temp_file)
      unlink(temp_file)
    } else {
      stop("Failed to download CSV file: ", http_status(csv_response)$reason)
    }
  } else {
    stop("No CSV file found in the resources.")
  }

  # Display the dataframe
  print(dataset)
} else {
  stop("Failed to fetch the dataset metadata: ", http_status(response)$reason)
}

