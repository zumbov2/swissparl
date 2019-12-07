#' Retrieve the first rows of a table
#'
#' \code{get_glimpse} retrieves the first rows of a table of the Swiss Parliament WebServices and
#'     allows a first insight into the data structure.
#'
#' @param table name of the table to glimpse into. For an overview of available tables use \code{\link{get_tables}()}.
#' @param rows number of records to download. Maximum is 1000.
#' @param Language filter rows by language. Possible are \code{DE}, \code{FR}, \code{IT}, \code{RM}, and \code{EN}.
#'
#' @return A tibble of different length and variable composition.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Short excerpt of table "Person"
#' get_glimpse(table = "Person")
#' }
get_glimpse <- function(table, rows = 20, Language = "DE") {

  # Check number of rows
  if (rows > 1000) stop("rows must not be larger than 1000")

  # Fetch data
  fetch_data(
    skip = 0,
    pb_pos =  NULL,
    table = table,
    package_size = rows,
    pb = NULL,
    pb_max = NULL,
    stop = TRUE,
    attempts = 10,
    wtf = 1,
    silent = FALSE,
    path = "normal",
    Language = Language
  )

}
