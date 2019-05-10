#' Retrieve committee memberships
#'
#' \code{get_councillors} retrieves data on committee memberships provided by the WebServices of the Swiss Parliament.
#'
#' @importFrom utils txtProgressBar
#' @importFrom purrr map2_dfr
#'
#' @param committee_id numeric or character vector of committee IDs to query.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 24 columns, including \code{faction}, \code{gender}, \code{language} and \code{committeeFunctionName}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_committee_members(committee_id = c(1:3))
#' }
get_committee_members <- function(committee_id, lang = "de", silent = F) {

  # Initiate
  if (!silent) cat("Fetching data from http://ws-old.parlament.ch/committees/\n")

  # Feedback on progress
  if (!silent & length(committee_id) > 1) {

    pb <- utils::txtProgressBar(1, length(committee_id), style = 3)

  } else {

    pb <- NULL
    silent <- T

  }

  # Fetch data
  res <- purrr::map2_dfr(committee_id, c(1:length(committee_id)), get_committee_members_single, prog_bar = pb, lang = lang, silent = silent)

  # Return
  return(res)

}

#' Retrieve all committee memberships
#'
#' \code{get_committee_members2} is wrapper around \code{get_committee_members} and enables the mass retrieval of
#'     data on committee memberships provided by the WebServices of the Swiss Parliament.
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 24 columns, including \code{faction}, \code{gender}, \code{language} and \code{committeeFunctionName}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_committee_members2()
#' }
get_committee_members2 <- function(lang = "de", silent = F) {

  # Collecting affair IDs
  if (!silent) cat("Collecting IDs from http://ws-old.parlament.ch/committees\n")
  ids <- get_committees(silent = T)

  # Fetching data
  res <- get_committee_members(ids$id, lang = lang, silent = silent)

  # Return
  return(res)

}

#' Retrieve faction memberships
#'
#' \code{get_councillors} retrieves data on faction memberships provided by the WebServices of the Swiss Parliament.
#'
#' @importFrom utils txtProgressBar
#' @importFrom purrr map2_dfr
#'
#' @param faction_id numeric or character vector of faction IDs to query.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 22 columns, including \code{canton}, \code{council}, \code{party} and \code{factionFunction}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_faction_members(faction_id = c(1:3))
#' }
get_faction_members <- function(faction_id, lang = "de", silent = F) {

  # Initiate
  if (!silent) cat("Fetching data from http://ws-old.parlament.ch/factions/\n")

  # Feedback on progress
  if (!silent & length(faction_id) > 1) {

    pb <- utils::txtProgressBar(1, length(faction_id), style = 3)

  } else {

    pb <- NULL
    silent <- T

  }

  # Fetch data
  res <- purrr::map2_dfr(faction_id, c(1:length(faction_id)), get_faction_members_single, prog_bar = pb, lang = lang, silent = silent)

  # Return
  return(res)

}

#' Retrieve all faction memberships
#'
#' \code{get_faction_members2} is wrapper around \code{get_faction_members} and enables the mass retrieval of
#'     data on faction memberships provided by the WebServices of the Swiss Parliament.
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 22 columns, including \code{canton}, \code{council}, \code{party} and \code{factionFunction}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_faction_members2()
#' }
get_faction_members2 <- function(lang = "de", silent = F) {

  # Collecting affair IDs
  if (!silent) cat("Collecting IDs from http://ws-old.parlament.ch/committees\n")
  ids <- get_factions(silent = T)

  # Fetching data
  res <- get_faction_members(ids$id, lang = lang, silent = silent)

  # Return
  return(res)


}
