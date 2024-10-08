% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractors.R
\name{extract_recipient_data}
\alias{extract_recipient_data}
\title{Extract Recipient Data}
\usage{
extract_recipient_data(year, nrows = NULL)
}
\arguments{
\item{year}{The year of the data.}

\item{nrows}{The number of rows to read from the index file. Default is unlimited (NULL).}
}
\value{
A data frame containing the extracted recipient data.
}
\description{
This function extracts recipient data from XML files for a specific year.
}
\keyword{internal}
