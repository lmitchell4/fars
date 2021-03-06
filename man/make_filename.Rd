% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Generate FARS data filename}
\usage{
make_filename(year)
}
\arguments{
\item{year}{A numeric constant giving the year for which a filename 
should be generated. Should be in four-digit format, e.g., 2013.
Can alternatively be a string, e.g., "2013".}
}
\value{
This function returns a string giving the filename. The string 
follows this pattern: \code{accident_YEAR.csv.bz2}
}
\description{
This function generates the name of an FARS data file for a given 
year. The resulting filename follows the pattern 
\code{accident_YEAR.csv.bz2}, where YEAR is the year value passed to 
the function.
}
\examples{
my_file <- make_filename(year=2013)

}
