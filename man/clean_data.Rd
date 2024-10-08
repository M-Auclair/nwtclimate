% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dependencies_functions.R
\name{clean_data}
\alias{clean_data}
\title{Data cleaning function
Assigns NA to values based on data flags}
\usage{
clean_data(df)
}
\arguments{
\item{df}{flagged climate dataframe to be cleaned}
}
\value{
cleaned climate dataframe
}
\description{
Data cleaning function
Assigns NA to values based on data flags
}
\examples{
df <- clean_data(df)
}
