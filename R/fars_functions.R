#' Reads a csv file and transforms it into a table
#'
#' This function reads a file having \code{filename} as file name. If the file exists,
#' the csv file is read and converted into a table format.
#'
#' @param filename A character string indicating the name of the file
#'
#' @return This function returns the table with the data present in the file
#'
#' @examples
#' \dontrun{
#' fars_read(filename="accident_2014.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#'
#' @importFrom dplyr tbl_df
#'
#' @note The function returns an error if the file does not exist
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates the file name starting from the year
#'
#' This function creates the file name giving the \code{year} as input.
#'
#' @param year A number indicating the year
#'
#' @return This function returns the string with the file name associated with the year
#'
#' @examples
#' \dontrun{
#' make_filename(year=2014)
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Creates a list of tables corrisponding to different years
#'
#' This function receives a list of years as input and retrieves the associated files.
#' For each file it retrieves the corrisponding data table and selects the month and year columns.
#'
#' @param years A list of years
#'
#' @return This function returns a list of tables corrisponding to a list of years
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' fars_read_years(years=c(2014,2015))
#' }
#'
#' @note The function returns an error if the year is invalid:
#' the corrispondent file does not exist
#'
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

#' Summarizes results from multiple years
#'
#' This function receives a list of years as input and retrieves the associated data tables
#' with \code{fars_read_years}. Then it summarizes the results per year.
#'
#' @inheritParams fars_read_years
#'
#' @return This function returns a table with summarized results for different years
#'
#' @import dplyr
#'
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(years=c(2014,2015))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Creates graph for a specific state and year
#'
#' This function receives a specific state and year, retrieving the associated data.
#' If the state exists, it filters the data per state and creates the related graph.
#' The function needs the library maps.
#'
#' @param state.num A number indicating the state
#'
#' @param year A number indicating the year
#'
#' @return This function creates a graph of a specific state and year
#'
#' @importFrom dplyr filter
#'
#' @importFrom maps map
#'
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(state.num=1,year=2014)
#' }
#'
#' @note The function returns an error if the state number is invalid (not present)
#'
#' @export
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
