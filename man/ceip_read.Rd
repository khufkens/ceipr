% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ceip_read.r
\name{ceip_read}
\alias{ceip_read}
\title{Load CEIP zipped data into a tidy data frame}
\usage{
ceip_read(pollutant = "NOx", sector = LETTERS[1:13],
  year = 2000:2016, country = NULL, path = tempdir())
}
\arguments{
\item{pollutant}{which pollutant to include (use "ALL" for all pollutants
or "NT" for National Totals)}

\item{sector}{CEIP sectors (default is all sectors from A to M)}

\item{year}{which years to include in the data compilation}

\item{country}{ISO2 country code (two letters), the euro zone 18 can be
downloaded with the following string c("AT","BE","CY","EE","FI", "FR","DE",
"GR","IE","IT","LT","LV","LU","NL","PT","SK","SI","ES") (default = NULL)}

\item{path}{path with original CEIP zip files (default = tempdir())}
}
\value{
Returns a data frame (tibble) of CEIP data. This data is tidy
and can be easily used in statistical analysis. Or converted to geospatial
data using included functions.
}
\description{
Load data for specified pollutant(s), for specified sector(s) and all years from
a directory of zip files, or an online path. For the latter option your,
milage might vary and it is recommended to download files of interest first
using the ceip_download() function as it is less likely to cause time-out
issues and will limit overall traffic to the server.
}
