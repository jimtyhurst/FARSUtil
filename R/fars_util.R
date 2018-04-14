# Hack to avoid 'Note' produced by 'R CMD check' for variable usage
# in dplyr functions.
utils::globalVariables(c("MONTH", "STATE", "year"))

#' Reads a CSV file as a tibble (tbl_df).
#' @details Data is from the US National Highway Traffic Safety
#'     Administration's Fatality Analysis Reporting System
#'     (\href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS}).
#' @param filename string path to a CSV file to be read.
#'     The path may be absolute or relative to the current working directory.
#' @return tbl_df representation of the data in the input CSV file.
#' @note Fails when there is no file with the given 'filename'.
#' @importFrom readr read_csv
#' @import dplyr
#' @examples
#'     # my_tbl <- fars_read("inst/extdata/fars_data/accident_2013.csv.bz2")
#' @export
fars_read <- function(filename) {
  if (!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Builds a string filename for a
#'     \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS}
#'     file for the given year.
#' @param year of data to be read. May be string or integer.
#' @return string filename, following the naming convention of
#'     \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS}
#'     files.
#' @examples
#'     fars_2013_path <- make_filename("2013")
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file("extdata", "fars_data", sprintf("accident_%d.csv.bz2", year), package = "FARSUtil")
}

#' Reads a list of FARS files for the given years.
#' @description The list returned by this function is useful for
#'     summarizing numbers of accidents by month for certain years.
#' @param years vector of integer or string years for data to be read.
#' @return list of tibbles with columns MONTH and year, corresponding
#'     to the original rows in the data files.
#'     When there is no file for a given year, that entry will
#'     be NULL and a warning is given.
#' @import dplyr
#' @examples
#'     fars_read_years(c(2013, 2014))
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

#' Summarizes the number of operations by month and year.
#' @param years vector of integer or string years for data to be read.
#' @return list of tibbles with columns MONTH and year.
#' @note If there is no data for one of the input years, a warning is
#'     given and that year will be ignored.
#' @import dplyr
#' @importFrom tidyr spread
#' @examples
#'     by_month <- fars_summarize_years(c(2013, 2014))
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots accidents on an outline of a state map.
#' @description Plots dots on an outline of a state map for accidents
#'     in the give state during the given year.
#' @param state.num integer identifier of a state in the
#'     United States.
#' @param year integer year for which to plot accidents.
#' @return NULL, but has side-effect of creating a plot.
#' @import dplyr
#' @importFrom maps map
#' @importFrom graphics points
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if (!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if (nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map(
      "state",
      ylim = range(LATITUDE, na.rm = TRUE),
      xlim = range(LONGITUD, na.rm = TRUE)
    )
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
