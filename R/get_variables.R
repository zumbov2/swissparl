#' Retrieve available variables
#'
#' \code{get_variables} retrieves the variable names of a table of the Swiss Parliament WebServices.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select_if rename_all
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_remove_all
#' @importFrom utils setTxtProgressBar
#'
#' @param table name of the table to be searched. For an overview of available tables use \code{\link{get_tables}()}.
#' @param pb.pos value for the progress bar. Not to be specified outside of \code{\link{get_overview}()}.
#' @param pb progress bar. Not to be specified outside of \code{\link{get_overview}()}.
#'
#' @return A character vector that contains the names of the variables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get variables of table "Person"
#' get_variables(table = "Person")
#' }
get_variables <- function(table, pb.pos = NULL, pb = NULL) {

  # Build URL
  url <- paste0(
    "https://ws.parlament.ch/odata.svc/",
    table,
    "?$top=1"
    )

  # Fetch Data
  variables <- jsonlite::fromJSON(url) %>%
    as.data.frame() %>%
    dplyr::select_if(list(~ !is.data.frame(.))) %>%
    dplyr::rename_all(list(~ stringr::str_remove_all(., "d\\."))) %>%
    names() %>%
    sort()

  # Progress Bar
  if (!is.null(pb)) utils::setTxtProgressBar(pb, pb.pos)

  # Return
  return(variables)

}
