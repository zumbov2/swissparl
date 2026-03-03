#' Retrieve the first rows of a table
#'
#' \code{get_glimpse} retrieves the first rows of a table of the Swiss Parliament WebServices and
#'     allows a first insight into its data structure.
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
#' \dontrun{
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

#' Retrieve the first rows of an OpenParlData resource
#'
#' \code{get_glimpse2} retrieves the first rows of a resource provided by the
#' OpenParlData.ch REST API and allows a first insight into its data structure.
#'
#' @param table name of the OpenParlData resource to glimpse
#'   into. For an overview of available endpoints use
#'   \code{\link{get_tables2}()}.
#' @param rows number of records to download. Maximum is 1000.
#'
#' @return A tibble containing up to \code{rows} records. Column composition
#'   depends on the selected OpenParlData resource.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Short excerpt of OpenParlData resource "persons"
#' get_glimpse2(table = "persons")
#' }
get_glimpse2 <- function(table, rows = 20) {
  
  # Check number of rows
  if (rows > 1000) stop("rows must not be larger than 1000")
  
  fetch_data2(
    table = table,
    package_size = rows,
    max_rows = rows,
    silent = TRUE
    )
  
}
