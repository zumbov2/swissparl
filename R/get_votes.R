#' Retrieve individual vote decisions
#'
#' \code{get_votes_councillors} retrieves individual vote decisions of members of the National Council provided by the
#'     WebServices of the Swiss Parliament.
#'
#' @importFrom purrr map_dfr
#'
#' @param councillor_id numeric or character vector of councillor IDs to query.
#' @param from specifies start of observation period, filters votes by date. Recommended date format \code{YYYY/MM/DD}.
#' @param to specifies end of observation period, filters votes by date. Recommended date format \code{YYYY/MM/DD}.
#' @param legislative_period specifies observation period by legislative period.  See \code{get_legislative_periods} for IDs.
#' @param session specifies observation period by session. See \code{get_sessions} for IDs.
#' @param search_text character string, subsets votes by search text.
#' @param affair_id affair ID. See \code{get_affairs} for IDs.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 16 columns, including \code{councillorVote.decision}, \code{meaningYes}, \code{meaningNo} and \code{divisionText}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # All votes of Thomas Aeschi and Cédric Wermuth on USRIII
#' get_votes_councillors(councillor_id = c(2758, 2759), affair_id = 20150049)
#'
#' # All votes of Pascale Bruderer Wyss during her time in the National Council
#' get_votes_councillors(councillor_id = 2573)
#' }
get_votes_councillors <- function(councillor_id, from = NULL, to = NULL, legislative_period = NULL, session = NULL,
                                  search_text = NULL, affair_id = NULL, lang = "de", silent = F) {

  # Modify query
  add <- paste0(
    query_add("dateFromFilter", from),
    query_add("dateToFilter", to),
    query_add("legislativePeriodFilter", legislative_period),
    query_add("sessionFilter", session),
    query_add("searchTextFilter", search_text),
    query_add("affairNumberFilter", affair_id)
  )

  # Fetch data
  res <- purrr::map_dfr(councillor_id, get_votes_councillor, lang = lang, silent = silent, add = add)

  # Return
  return(res)

}

#' Retrieve all individual vote decisions
#'
#' \code{get_votes_councillors2} is wrapper around \code{get_votes_councillors} and enables the mass retrieval of
#'     individual vote decisions of members of the National Council provided by the WebServices of the Swiss Parliament.
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 16 columns, including \code{councillorVote.decision}, \code{meaningYes}, \code{meaningNo} and \code{divisionText}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_votes_councillors2()
#' }
get_votes_councillors2 <- function(lang = "de", silent = F) {

  # Collecting affair IDs
  ids <- get_ids_votes_councillors(silent = silent)

  # Fetching data
  res <- get_votes_councillors(ids$id)

  # Return
  return(res)

}

#' Retrieve votes on specific affairs
#'
#' \code{get_votes_affairs} retrieves affair specific vote data of the National Council provided by the
#'     WebServices of the Swiss Parliament.
#'
#' @importFrom purrr map_dfr
#'
#' @param affair_id numeric or character vector of affair IDs to query. See \code{get_affairs} for IDs.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 17 columns, including the voting results.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # All votes on USRIII
#' get_votes_affairs(affair_id = 20150049)
#' }
get_votes_affairs <- function(affair_id, lang = "de", silent = F) {

  # Initiate
  if (!silent) cat("Fetching data from http://ws-old.parlament.ch/votes/affairs/\n")

  # Feedback on progress
  if (!silent & length(affair_id) > 1) {

    pb <- utils::txtProgressBar(1, length(affair_id), style = 3)

  } else {

    pb <- NULL
    silent <- T

  }

  # Fetch data
  res <- purrr::map2_dfr(affair_id, c(1:length(affair_id)), get_votes_affair, prog_bar = pb, lang = lang, silent = silent, sec = T)


  # Return
  return(res)

}

#' Retrieve all votes on affair level
#'
#' \code{get_votes_affairs2} is wrapper around \code{get_votes_affairs} and enables the mass retrieval of
#'     vote data of the National Council provided by the WebServices of the Swiss Parliament.
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 17 columns, including the voting results.
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_votes_affairs2()
#' }
get_votes_affairs2 <- function(lang = "de", silent = F) {

  # Collecting affair IDs
  ids <- get_ids_votes_affairs(silent = silent)

  # Fetching data
  res <- get_votes_affairs(ids$id)

  # Return
  return(res)

}
