#' Retrieve data on political affairs
#'
#' \code{get_affair_details} retrieves detailed data on political affairs of the Swiss Parliament provided by
#'     the WebServices of the Swiss Parliament.
#'
#' @importFrom utils txtProgressBar
#' @importFrom purrr map2_dfr
#'
#' @param affair_id numeric or character vector of affair IDs to query.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 17 columns, including
#' \itemize{
#' \item \code{affairType} a list with data on the affair type.
#' \item \code{author} a list with data on the author of the affair.
#' \item \code{draft} a list with data on the consultation of the affair.
#' \item \code{texts} a list with associated texts.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_affair_details(affair_id = c(20130468, 20123678))
#' }
get_affair_details <- function(affair_id, lang = "de", silent = F) {

  # Initiate
  if (!silent) cat("Fetching data from http://ws-old.parlament.ch/affairs\n")

  # Feedback on progress
  if (!silent & length(affair_id) > 1) {

    pb <- utils::txtProgressBar(1, length(affair_id), style = 3)

  } else {

    pb <- NULL
    silent <- T

  }

  # Fetch data
  res <- purrr::map2_dfr(affair_id, c(1:length(affair_id)), get_affair_detail, prog_bar = pb, lang = lang, silent = silent)


  # Return
  return(res)

}

#' Retrieve data on all political affairs
#'
#' \code{get_affair_details2} is wrapper around \code{get_affair_details} and enables the mass retrieval of detailed data
#'     on political affairs provided by the WebServices of the Swiss Parliament.
#'
#' @importFrom stringr str_sub
#' @importFrom dplyr filter
#'
#' @param year numeric or character vector of years to query. If \code{is.null(year)} all available affairs are retrieved.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 17 columns, including:
#' \itemize{
#' \item \code{affairType} a list with data on the affair type.
#' \item \code{author} a list with data on the author of the affair.
#' \item \code{draft} a list with data on the consultation of the affair.
#' \item \code{texts} a list with associated texts.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_affair_details2(year = c(1999:2003))
#' }
get_affair_details2 <- function(year = NULL, lang = "de", silent = F) {

  # Collecting affair IDs
  ids <- get_ids_affairs(silent = silent)

  # Subset
  if (!is.null(year)) {

    ids <- ids %>%
      dplyr::filter(stringr::str_sub(id, 0, 4) %in% year)

  }

  # Fetching data
  res <- get_affair_details(ids$id, lang = lang, silent = silent)

  # Return
  return(res)

}
