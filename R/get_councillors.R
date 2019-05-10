#' Retrieve councillor details
#'
#' \code{get_councillors} retrieves detailed data on councillors provided by the WebServices of the Swiss Parliament.
#'
#' @importFrom utils txtProgressBar
#' @importFrom purrr map2_dfr
#'
#' @param councillor_id numeric or character vector of councillor IDs to query.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 40 columns, including \code{canton}, \code{birthDate}, \code{committeeMemberships},
#'     \code{maritalStatus}, \code{numberOfChildren} and \code{militaryGrade}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_councillors(councillor_id = c(823, 316))
#' }
get_councillors <- function(councillor_id, lang = "de", silent = F) {

  # Initiate
  if (!silent) cat("Fetching data from http://ws-old.parlament.ch/councillors\n")

  # Feedback on progress
  if (!silent & length(councillor_id) > 1) {

    pb <- utils::txtProgressBar(1, length(councillor_id), style = 3)

  } else {

    pb <- NULL
    silent <- T

  }

  # Fetch data
  res <- purrr::map2_dfr(councillor_id, c(1:length(councillor_id)), get_councillor_details, prog_bar = pb, lang = lang, silent = silent)

  # Return
  return(res)

}

#' Retrieve councillor details
#'
#' \code{get_councillors2} is wrapper around \code{get_councillors} and enables the mass retrieval of
#'     detailed data on councillors provided by the WebServices of the Swiss Parliament.
#'
#' @param current if \code{TRUE} only current councillors are retrieved.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 40 columns, including \code{canton}, \code{birthDate}, \code{committeeMemberships},
#'     \code{maritalStatus}, \code{numberOfChildren} and \code{militaryGrade}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_councillors2(current = T)
#' }
get_councillors2 <- function(current = FALSE, lang = "de", silent = F) {

  # Collecting IDs
  if (current) {

    if (!silent) cat("Collecting IDs from http://ws-old.parlament.ch/councillors/basicdetails\n")
    ids <- get_councillors_basics(silent = T)

  } else {

    if (!silent) cat("Collecting IDs from http://ws-old.parlament.ch/councillors/historic\n")
    ids <- get_councillors_historic_basics(silent = T)

  }

  # Fetching data
  res <- get_councillors(unique(ids$id), lang = lang, silent = silent)

  # Return
  return(res)

}
