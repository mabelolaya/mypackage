
# Documenting Code- Week 2
# ---------------------------------------------------------------------------

#________________________________
#'Title: fars_read
#'_______________________________
#' This function:
#'-accepts a csv file
#'-returns a tbl_df wrapper around the data
#'-An Show ERROR message when the file doesn't exist
#'_______________________________
#'-----1-------
#'
#' @param filename:  a csv file  (may or may not exist)
#' @return a  tbl_df, a wrapper around the data that came from the csv file. (package:dplyr)
#' @importFrom readr read_csv
#' @importFrom tbl_df (package:dplyr)
#' @source extdate/accident_year.csv.bz2
#' @export
#' @examples
#' \dontrun{x <- fars_read('myFile.csv')}
#' \dontrun{system.file("extdata", "accident_year.csv.bz2", package = "packFars"). }


fars_read <- function(filename) {

  filedir <- system.file("extdata", filename, package = "mypackage")

  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'_______________________________
#' Title: make_filename
#'_______________________________
#' This function:
#'- Allows the user to create accident data files by year.
#'-don't support the tar/tarball format
#'-----2-------
#' @param year  A year (string), dont have restrictions on format
#' @return a filename in the format 'accident_year.csv.bz2'.
#' @source extdate-accident_year.csv.bz2
#' @export
#' @examples newFileName <- make_filename('2015')
#' \dontrun{system.file("extdata", "accident_year.csv.bz2", package = "packFars")}


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'_______________________________
#' Title: fars_read_years
#'_______________________________
#' This function:
#'-accepts one or more years as a list
#'-calls the function make_filename() with each of the years
#'-asociate those files with data associated with that specific year
#'-An error will be thrown and the function will be halted if the year is invalid
#'-----3-------
#' @param years one or more years as an atomic value or a list
#' @return Creates one or more datasets based on year number.  Returns NULL when occur a error
#'@importFrom  mutate (package:dplyr)
#' @importFrom select (package:dplyr)
#' @export
#' @examples
#' \dontrun{
#'      fars_read_years(2000)
#'      fars_read_years(as.list(1998, 1999, 2000))
#'      fars_read_years(1995:2013)
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#'_______________________________
#' Title: fars_summarize_years
#'_______________________________
#' This function:
#'-Accepts a list of one or more years
#'-Passes that list to fars_read_years().
#'-Receives from that function a data set of data in the month and year column
#' -Uses dplyr functions to count the number of observations by month for a year.
#'-----4-------
#' @param years One or more years, no error checking
#' @return A wide data frame of counts by month and year
#' @importFrom   bind_rows (package:dplyr)
#' @importFrom   group_by (package:dplyr)
#' @importFrom   summarize (package:dplyr)
#' @importFrom   spread (package:tidyr)
#' @export
#' @examples
#' \dontrun{
#'      fars_summarize_years(1995)
#'      fars_summarize_years(as.list(2017, 2018, 2019))
#'      fars_summarize_years(1997:2012)
#' }

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#'_______________________________
#' Title: fars_map_state
#'_______________________________
#' This function:
#' -Accepts a state number and year from the user/calling program
#' -Makes the appropriate filename using the year
#' -the make_filename function gets a data frame from fars_read()
#' -Error checks to make sure the state number exists
#' -uses maps and graphics to create plots
#'-----5-------
#' @param state.num Number of a state
#' @param year The year in question
#' @return A plot or set of plots based on latitude and longitude from the data file
#' @export
#' @importFrom  filter (package:dplyr)
#' @importFrom  map (package:maps)
#' @importFrom graphics points
#' @examples
#' \dontrun{
#' fars_map_state(1, 2000)
#' }


fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
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
