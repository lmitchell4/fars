% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Create frequency table of accidents by month and year}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A numeric vector of years to include in the frequency 
table. Can alternatively be a list of numeric values. The years should  
be in four-digit format, e.g., 2013. Can alternatively be a vector of 
strings, e.g., c("2013","2014","2015")}
}
\value{
This function returns a \code{tbld_df} object showing the 
number of accidents by month and year.
}
\description{
This function uses FARS datasets to calculate the number of 
accidents by month and year, and displays the results as a table.
}
\examples{
fars_summarize_years(years=2013:2015)

}
