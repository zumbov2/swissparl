#' Retrieve available tables
#'
#' \code{get_tables} retrieves the names of the available tables of the Swiss Parliament WebServices.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr pull
#' @importFrom magrittr "%>%"
#'
#' @return A sorted character vector containing the names of the available tables.
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

#' Retrieve available OpenParlData endpoints
#'
#' \code{get_tables2} retrieves the names of the main REST API endpoints
#' provided by the OpenParlData.ch API.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_extract str_remove
#' @importFrom magrittr "%>%"
#'
#' @return A sorted character vector containing the names of the available
#'   OpenParlData REST API endpoints.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all available OpenParlData endpoints
#' get_tables2()
#' }
get_tables2 <- function() {
  
  spec <- jsonlite::fromJSON("https://api.openparldata.ch/openapi.json")
  
  main_endpoints <- 
    names(spec[["paths"]]) %>% 
    stringr::str_extract("^/v1/[^/]+") %>%
    unique() %>% 
    stringr::str_remove("/v1/") %>% 
    sort()
  
  # Remove analytics
  main_endpoints <- main_endpoints[!main_endpoints == "analytics"]
    
  return(main_endpoints)
  
  }
