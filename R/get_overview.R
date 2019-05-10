#' Retrieve overview data
#'
#' \code{get_overview} retrieves overview data from the parent entities of the Swiss Parliament WebServices.
#'
#' @importFrom purrr map_dfr
#' @importFrom utils txtProgressBar
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @importFrom magrittr "%>%"
#'
#' @param ws_page url of one of the service pages contained in the menu under \url{http://ws-old.parlament.ch/}.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param add additional query modifiers.
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble. The number of columns depends on \code{ws_page}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_overview(ws_page = "http://ws-old.parlament.ch/affairs")
#' }
get_overview <- function(ws_page, lang = "de", add = NULL, silent = F) {

  # URL
  ws_page <- paste0(ws_page, query_lang(lang))
  if (!is.null(add)) ws_page <- paste0(ws_page, add)

  # Get number of pages
  number_of_pages <- get_number_of_pages(ws_page)

  # Feedback on progress
  if (!silent & number_of_pages > 1) {

    pb <- utils::txtProgressBar(1, number_of_pages, style = 3)

  } else {

    pb <- NULL
    silent <- T

  }

  # Collect pages
  res <- purrr::map_dfr(1:number_of_pages, get_overview_page, ws_page = ws_page, prog_bar = pb, silent = silent) %>%
    tibble::as_tibble()

  # Return
  return(res)

}
