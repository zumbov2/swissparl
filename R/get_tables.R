#' Retrieve available tables
#'
#' \code{get_tables} retrieves the names of the available tables of the Swiss Parliament WebServices.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr pull
#' @importFrom magrittr "%>%"
#'
#' @return A character vector that contains all the names of the available tables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all available tables
#' get_tables()
#' }
get_tables <- function() {

  # Query
  tables <- jsonlite::fromJSON("https://ws.parlament.ch/odata.svc/") %>%
    as.data.frame() %>%
    dplyr::pull(EntitySets) %>%
    as.character() %>%
    sort()

  return(tables)

}
