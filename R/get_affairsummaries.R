#' Retrieve affair summaries
#'
#' \code{get_affairsummaries} retrieves summaries of political affairs provided by the WebServices of the Swiss Parliament.
#'
#' @importFrom utils txtProgressBar
#' @importFrom purrr map2_dfr
#'
#' @param affair_id numeric or character vector of affair IDs to query.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 7 columns, including \code{description}, \code{initialSituation} and \code{proceedings}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_affairsummaries(affair_id = c(20130468, 19950056))
#' }
get_affairsummaries <- function(affair_id, lang = "de", silent = F) {

  # Initiate
  if (!silent) cat("Fetching data from http://ws-old.parlament.ch/affairsummaries\n")

  # Feedback on progress
  if (!silent & length(affair_id) > 1) {

    pb <- utils::txtProgressBar(1, length(affair_id), style = 3)

  } else {

    pb <- NULL
    silent <- T

  }

  # Fetch data
  res <- purrr::map2_dfr(affair_id, c(1:length(affair_id)), get_affairsummaries_detail, prog_bar = pb, lang = lang, silent = silent)

  # Return
  return(res)

}

#' Retrieve all affair summaries
#'
#' \code{get_affairsummaries2} is wrapper around \code{get_affairsummaries} and enables the mass retrieval of
#'     summaries of political affairs provided by the WebServices of the Swiss Parliament.
#'
#' @importFrom stringr str_sub
#' @importFrom dplyr filter
#'
#' @param year numeric or character vector of years to query. If \code{is.null(year)} all available summaries are retrieved.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 7 columns, including \code{description}, \code{initialSituation} and \code{proceedings}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_affairsummaries2(year = c(1999:2003))
#' }
get_affairsummaries2 <- function(year = NULL, lang = "de", silent = F) {

  # Collecting affair IDs
  ids <- get_ids_affairsummaries(silent = silent)

  # Subset
  if (!is.null(year)) {

    ids <- ids %>%
      dplyr::filter(stringr::str_sub(id, 0, 4) %in% year)

  }

  # Fetching data
  res <- get_affairsummaries(ids$id, lang = lang, silent = silent)

  # Return
  return(res)

}
