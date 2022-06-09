#' Building R Packages: Week 2 Peer-Graded Assignment  Adrian Accurso 5/25/2022
#'
#' The assignment will be graded based on how closely the documentation reflects
#' the actual functioning of the code presented in the script file. In particular,
#' you will be expected to document:

#' -what each function does, in general terms;
#' -the function arguments (inputs);
#' -each function's return value;
#' -conditions that may result in an error;
#' -functions that need to be imported from external packages;
#' -examples of each function's usage.
#' ############################################################################
#' fars_read: Read a file with FARS (Fatality Analysis Reporting System) data.
#' Create exception if file does not exist, otherwise changes the file contents
#' from a csv to a tibble
#'
#' @param filename A character string representing a file name
#' @return A tibble with fars data from the file named 'filename', or an error
#' if the filepath is incorrect or if the file is not in csv format.
#'
#' @importFrom readr read_csv
#' @importFrom tidyr as_tibble
#'
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")}
#' \dontrun{x<-fars_read('farsFile.csv')}
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  tidyr::as_tibble(data)
}

#' make_filename: Return a character vector containing a formatted combination
#' of text and variables from an inputted year. The corresponding file must be
#' available in the working directory as this helper function does not check if
#' it is present.
#'
#' @param year A number or a string
#' @return character vector of filename formatted as 'accident_year.csv'.
#' @examples
#' make_filename(2013)
#' \dontrun{newFile <- make_filename('2021')}
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv", year)
}

#' fars_read_years: Accepts one or more years as an inputted vector of years,
#' and then calls make_filename() on each of those years. Finally it fills those
#' newly made files with data from the associated year in the main data set.
#'
#' @param years A numeric vector of years for which the FARS data is required
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @return a data.frame including month and year entries, or NULL if the
#' supplied year is not valid.
#' @note this function will return NULL if the input is not a valid year.
#' @export

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

#' fars_summarize_years: Returns a tibble of complete observations where
#' the year is included as a column. Data from multiple years is expressed
#' in one tibble with the fatalities summarized by month and year over the
#' years supplied.
#'
#' @param years A numeric vector of the years for the required FARS data.
#' @return A tibble with the number of accidents each year summarized by month.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @importFrom dplyr n
#'
#' @examples
#' \dontrun{plot(fars_summarize_years(2014))}
#' \dontrun{fars_summarize_years(yearsVector)}
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state: Returns a map of the accidents in a state for a given year
#' from an inputted state number and an inputted year.
#' If state number does not exist it will return INVALID state number message.
#'
#' @param state.num A number corresponding to the alphabetical order of US state
#' @param year A number, string or integer, for which FARS data is desired
#'
#' @return NULL
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points

#'@examples
#'\dontrun{fars_map_state(1,2013)}
#'\dontrun{fars_map_state(36,2013)}
#'@export

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
