
#' Read in a data set
#' 
#' This function reads in data from a csv file.
#' 
#' @param filename A character string giving the name of the file
#' 
#' @return This function returns a \code{tbl_df} object.
#' 
#' @note An error message is printed if the file is not found.
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @examples
#' my_data <- fars_read(filename="accident_2013.csv.bz2")
#' 
fars_read <- function(filename) {
  if(!file.exists(paste0("inst/extdata/",filename)))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(paste0("inst/extdata/",filename), progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Generate FARS data filename
#' 
#' This function generates the name of an FARS data file for a given 
#' year. The resulting filename follows the pattern 
#' \code{accident_YEAR.csv.bz2}, where YEAR is the year value passed to 
#' the function.
#' 
#' @param year A numeric constant giving the year for which a filename 
#' should be generated. Should be in four-digit format, e.g., 2013.
#' Can alternatively be a string, e.g., "2013".
#' 
#' @return This function returns a string giving the filename. The string 
#' follows this pattern: \code{accident_YEAR.csv.bz2}
#' 
#' @examples
#' my_file <- make_filename(year=2013)
#' 
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Create a list of FARS datasets separated by year
#' 
#' This function reads in FARS datasets for select years and returns 
#' each dataset as a separate element in a list.
#' 
#' @param years A numeric vector of years for which data are desired.
#' Can alternatively be a list of numeric values. The years should be 
#' in four-digit format, e.g., 2013. Can alternatively be a vector of 
#' strings, e.g., c("2013","2014","2015")
#' 
#' @return This function returns a list. Each element of the list is 
#' a \code{tbl_df} object showing the month and year columns of a  
#' FARS dataset corresponding to a given year.
#' 
#' @note If an error occurs while trying to read in a FARS dataset, a 
#' warning is given and the list element for that year is NULL. This 
#' function uses the functions \code{make_filename} and \code{fars_read}.
#' 
#' @importFrom dplyr mutate_ select_
#' @importFrom magrittr "%>%"
#' 
#' @examples
#' my_list <- fars_read_years(years=2013:2015)
#' 
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat, year = ~year) %>% 
                dplyr::select_(.dots = c("MONTH", "year"))
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Create frequency table of accidents by month and year
#' 
#' This function uses FARS datasets to calculate the number of 
#' accidents by month and year, and displays the results as a table.
#'
#' @param years A numeric vector of years to include in the frequency 
#' table. Can alternatively be a list of numeric values. The years should  
#' be in four-digit format, e.g., 2013. Can alternatively be a vector of 
#' strings, e.g., c("2013","2014","2015")
#' 
#' @return This function returns a \code{tbld_df} object showing the 
#' number of accidents by month and year.
#' 
#' @importFrom dplyr bind_rows group_by_ summarize_ n
#' @importFrom tidyr spread_
#' @importFrom magrittr "%>%"
#' 
#' @examples
#' fars_summarize_years(years=2013:2015)
#' 
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
      dplyr::group_by_(~year, ~MONTH) %>% 
      dplyr::summarize_(n = n()) %>%
      tidyr::spread_(key_col = 'year', value_col = 'n')
}


#' Plot a map showing locations of accidents
#' 
#' This function uses FARS datasets to show a map of where accidents 
#' have occurred in a specified state and year.
#' 
#' @param state.num A numeric constant giving the state id
#' @param year A numeric constant giving the year in four-digit 
#' format, e.g., 2013. Can alternatively be a string, e.g., "2013".
#' 
#' @return This function plots a map and does not return anything.
#' 
#' @note The function stops if state.num is not a valid state id.
#' If there are no accidents, a message is printed to the console.
#' This function uses the functions \code{make_filename} and 
#' \code{fars_read}.
#' 
#' @importFrom dplyr filter_
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @examples
#' fars_map_state(state.num=1, year=2013)
#' 
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
