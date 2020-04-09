#' Retrieve overview of all tables and variables
#'
#' \code{get_overview} retrieves the names of all available tables of the Swiss Parliament WebServices and
#'     the variables they contain.
#'
#' @importFrom tibble tibble
#' @importFrom purrr map2
#' @importFrom tidyr unnest
#' @importFrom magrittr "%>%"
#' @importFrom utils txtProgressBar
#' @importFrom crayon silver
#'
#' @param silent if \code{TRUE}, no progress bar and messages are displayed.
#'
#' @return A tibble with the 2 columns \code{table} and \code{variable}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_overview()
#' }
get_overview <- function(silent = F) {

  # Get available tables
  tables <- get_tables()

  # Feedback on progress
  if (!silent) {
    cat(crayon::silver("\n\n   Fetching data from 'https://ws.parlament.ch/odata.svc/'", "\n\n"))
    pb <- utils::txtProgressBar(1, length(tables), style = 3)
  } else {
    pb <- NULL
  }

  # Get variables for each table
  res <- tibble::tibble(
    table = tables,
    variable = purrr::map2(tables, 1:length(tables), get_variables, pb = pb)
    ) %>%
    tidyr::unnest(cols = variable)

  # Close Progress Bar
  if (!silent) close(pb)

  # Return
  return(res)

}
