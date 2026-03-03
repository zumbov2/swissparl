#' Retrieve available variables
#'
#' \code{get_variables} retrieves the variable names of a table of the Swiss Parliament WebServices.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select_if rename_all
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_remove_all
#' @importFrom utils setTxtProgressBar
#' @importFrom glue glue
#'
#' @param table name of the table to be queried. For an 
#'    overview of available tables use \code{\link{get_tables}()}.
#' @param pb.pos value for the progress bar. Not to be 
#'    specified outside of \code{\link{get_overview}()}.
#' @param pb progress bar object. Not to be specified 
#'    outside of \code{\link{get_overview}()}.
#'
#' @return A sorted character vector containing the names of the variables.
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
  url <- glue::glue("https://ws.parlament.ch/odata.svc/{table}?$top=1")

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

#' Retrieve available variables from an OpenParlData endpoint
#'
#' \code{get_variables2} retrieves the field names of a resource provided by
#' the OpenParlData.ch REST API.
#'
#' @param table name of the OpenParlData resource to be
#'   queried. For an overview of available endpoints use
#'   \code{\link{get_tables2}()}.
#' @param pb.pos value for the progress bar. Not to be
#'   specified outside of \code{\link{get_overview}()}.
#' @param pb progress bar object. Not to be specified outside of
#'   \code{\link{get_overview}()}.
#'
#' @return A sorted character vector containing the names of the fields
#'   available in the selected OpenParlData endpoint.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom utils setTxtProgressBar
#' @importFrom magrittr "%>%"
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Get variables of OpenParlData resource "persons"
#' get_variables2(table = "persons")
#' }
get_variables2 <- function(table, pb.pos = NULL, pb = NULL) {
  
  url <- glue::glue("https://api.openparldata.ch/v1/{table}?limit=1&lang_format=flat")
  
  raw <- jsonlite::fromJSON(url)
  
  variables <-
    raw[["data"]] %>%
    as.data.frame() %>%
    names() %>%
    sort()
  
  # Progress Bar
  if (!is.null(pb)) utils::setTxtProgressBar(pb, pb.pos)
  
  # Return
  return(variables)

}
