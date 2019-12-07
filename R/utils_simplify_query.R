# cumsum with overhead and reset after length of skip
#' @noRd
cumsum_with_reset <- function(out, input, overhead, skip) {

  # Normal
  s <- out + input

  # Reset
  if (s > skip) s <- overhead + input
  return(s)
}

# Split into subqueries
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter pull
#' @noRd
build_simple_query <- function(x, url_length, problem_filter, args_ohne) {

  values <- url_length %>%
    dplyr::filter(batch == x) %>%
    dplyr::pull(value) %>%
    c()

  args_ohne[[problem_filter]] <- values
  return(args_ohne)

}

# Get simplified subqueries for complex queries (filter)
#' @importFrom crayon silver green
#' @importFrom purrr map accumulate
#' @importFrom tibble tibble
#' @importFrom dplyr mutate lag filter
#' @importFrom magrittr "%>%"
#' @noRd
get_simplified_queries <- function(table, package_size, stop, silent, ...) {

  # Feedback
  if (!silent) cat(crayon::silver("\n\n   Complex filter structure. Attempting to split query into subqueries."))

  # Catch ...
  args <- list(...)

  # Check length and get longest filter (problem_filter)
  args_length <- purrr::map(args, length)
  problem_filter <- names(which(args_length == max(unlist(args_length))))[1]

  # Prep overhead-check (args without problem filter)
  args2 <- args
  args2$skip <- 0
  args2$package_size <- package_size
  args2$table <- table
  args2[[problem_filter]] <- NULL

  # Check overhead (base length of URL without longest argument)
  overhead <- nchar(do.call(get_url_skip, args2))

  # Error if already overhead is too long
  if (overhead > 1700) {

    if (stop) {

      stop (
        "Filter structure is too complex!",
        paste0(" Adjust filter (",problem_filter, ")"),
        " or retrieve entire table and create subset locally."
      )

    } else {

      return("suppressed-error")

    }

  }

  # Build table wih filter name and value
  url_length <- tibble::tibble(
    filter_name = problem_filter,
    value = args[[problem_filter]]
    ) %>%
    dplyr::filter(!is.na(value))

  # Cumulative length of url with overhead and resulting batches
  url_length <- url_length %>%
    dplyr::mutate(
      len = purrr::accumulate(
        nchar(url_length$filter_name)[2:nrow(url_length)] + nchar(url_length$value)[2:nrow(url_length)] + 15,
        cumsum_with_reset,
        overhead = overhead,
        skip = 1900,
        .init = overhead + nchar(url_length$filter_name)[1] + nchar(url_length$value)[1] + 15
        ),
      batch = ifelse(len > dplyr::lag(len), 0, 1),
      batch = ifelse(is.na(batch), 1, batch),
      batch = cumsum(batch)
      )

  # Prep for split into batches
  num_batches <- max(url_length$batch, na.rm = T)
  args_ohne <- args
  args_ohne[problem_filter] <- NULL

  # Split into batches
  args_batches <- purrr::map(1:num_batches, build_simple_query, url_length, problem_filter, args_ohne)
  if (!silent) cat(crayon::green("\n   Successfully split query into", length(args_batches), "subqueries.\n\n"))

  # Return
  return(args_batches)

}
