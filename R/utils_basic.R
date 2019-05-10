# FALSE to 'false' etc.
#' @noRd
log2log <- function(log) {

  if (log) {
    log <- "true"
  } else {
    log <- "false"
  }

  return(log)

}

# Null to NA
#' @importFrom rlang is_empty
#' @noRd
empty2NA <- function(value) {

  if (rlang::is_empty(value)) value <- NA
  return(value)

}

# list to list
list2list <- function(l) {

  list(unlist(l, recursive = F))

}

# Check if logical if !NULL
#' @noRd
check_null_log <- function(name, value) {

  if (!is.null(value)) {

    if (!is.logical(value)) stop (name, " must be either NULL or of type logical")

  }


}

# Vector to ;-separated
#' @noRd
vec2semi <- function(vec) {

  if (!is.null(vec)) paste(vec, collapse = ";")

}

# Query modifier for language
#' @noRd
query_lang <- function(lang = "de") {

  if (length(lang) > 1) stop ("Please choose one of 'de', 'fr', 'it', 'en'")
  if (!lang %in% c("de", "fr", "it", "en")) stop ("Unknown language. Please choose one of 'de', 'fr', 'it', 'en'")
  lang <- paste0("?lang=", lang)
  return(lang)

}

# Query modifier with log2log
#' @noRd
query_add_log <- function(name, value) {

  # Check
  check_null_log(name, value)

  # Generate
  if (is.logical(value)) paste0("&", name, "=", log2log(value))

}

# Unspecific query modifier
#' @noRd
query_add <- function(name, value) {

  # Generate
  if (!is.null(value)) paste0("&", name, "=", value)

}

# Get number of pages
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_attr
#' @importFrom stringr str_remove_all
#' @importFrom magrittr "%>%"
#' @noRd
get_number_of_pages <- function(ws_page) {

  res <- xml2::read_html(ws_page) %>%
    rvest::html_node("#main > div.paging > a:nth-child(4)") %>%
    rvest::html_attr("href") %>%
    stringr::str_remove_all("[^0-9]") %>%
    as.numeric()

  if (is.na(res)) res <- 1

  return(res)

}

# Get single overview page
#' @importFrom jsonlite fromJSON
#' @importFrom utils setTxtProgressBar
#' @noRd
get_overview_page <- function(page, ws_page, prog_bar, silent) {

  # Get page
  res <- jsonlite::fromJSON(paste0(ws_page, "&pageNumber=", page), flatten = T)

  # Progress
  if (!silent) utils::setTxtProgressBar(prog_bar, page)

  # Return
  return(res)

}

# Check availability
#' @importFrom httr GET content
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_detect
#' @noRd
check404 <- function(page) {

  httr::GET(page) %>%
    httr::content(encoding = "UTF-8") %>%
    paste0(collapse = " ") %>%
    stringr::str_detect("404 - File or directory not found.")

}

# Check availability
#' @importFrom purrr map
#' @importFrom magrittr "%>%"
#' @noRd
get_result <- function(fTV) {

  res <- purrr::map(fTV, "count") %>%
    unlist() %>%
    matrix(ncol = 6, byrow = T) %>%
    as.data.frame()

  names(res) <- c("Yes", "No", "EH", "NT", "ES", "P")
  return(tibble::as_tibble(res))
}

