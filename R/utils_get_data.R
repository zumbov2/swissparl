# Fetch a single data package
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select_if rename_all mutate_at
#' @importFrom tibble as_tibble
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_remove_all str_which
#' @importFrom utils setTxtProgressBar
#' @importFrom crayon	magenta
#' @importFrom httr GET content
#' @noRd
fetch_data <- function(skip, pb_pos, table, package_size, pb, pb_max, stop, attempts, wtf, silent, path, ...) {

  # Catch ... (workaround for both ways)
  if (...length() > 0) {
    args <- ...elt(...length())
    if (length(args) > 0) if (!is.list(args[1])) args <- list(...)
  } else {
    args <- NULL
  }

  # Get URL
  url <- get_url_skip(
    table = table,
    package_size = package_size,
    skip = skip,
    args
  )

  # Initiate GET while-loop
  res <- "suppressed-error"
  counter <- 0

  # Maximum number of attempts
  while(res[1] == "suppressed-error" & counter < attempts) {

    # Pausing wait-time-factor * counter seconds after each failed attempt
    Sys.sleep(wtf * counter)

    # Update counter
    counter <- counter + 1

    # Feedback
    if (!silent){
      if (counter == 2) cat(crayon::magenta("\n\n   Failed to fetch data.\n\n"))
      if (counter == 2) cat(crayon::magenta("   Attempting to retrieve data...\n"))
      if (counter > 1) cat(crayon::magenta("   ...attempt", counter, "\n"))
    }

    # Query
    res <- check_http(httr::GET(url), table = table, stop = stop)

    # Quasi-break feedback
    if (counter == attempts) {

      cat(
        crayon::magenta(
          "\n   Unable to fetch data from table",
          table,
          "(rows",
          skip + 1,
          "-",
          paste0(skip + package_size, ").\n")
        )
      )

    }
    if (counter == attempts) counter <- attempts + 1
  }

  # Extract data
  if (counter <= attempts) {

    # Progress Bar
    res <- jsonlite::fromJSON(httr::content(res, "text")) %>%
      as.data.frame() %>%
      dplyr::select_if(list(~ !is.data.frame(.))) %>%
      dplyr::rename_all(list(~ stringr::str_remove_all(., "d\\."))) %>%
      dplyr::mutate_at(dplyr::vars(stringr::str_which(names(.), "(?<!Meeting)Date|Since|Deadline")), list(~ convert_date(.))) %>%
      dplyr::mutate_at(dplyr::vars(stringr::str_which(names(.), "Modified|Start$|End$")), list(~ convert_datetime(.))) %>%
      dplyr::mutate_at(dplyr::vars(stringr::str_which(names(.), "Timezone$")), list(~ convert_datetime_tz(.))) %>%
      tibble::as_tibble()

    if (path == "normal" & !is.null(pb)) {

      utils::setTxtProgressBar(pb, pb_pos)
      if (pb_pos == pb_max) close(pb)

    }

    return(res)

    } else {

      if (path == "normal" & !is.null(pb)) {

        utils::setTxtProgressBar(pb, pb_pos)
        if (pb_pos == pb_max) close(pb)

      }

      return(invisible())


  }

}

# Query data
#' @importFrom httr GET content
#' @importFrom utils txtProgressBar
#' @importFrom purrr map2_dfr
#' @noRd
query_data <- function(args, pb_pos, table, package_size, pb, pb_max, stop, attempts, wtf, silent, path) {

  # Get number of entries
  url_count <- do.call(get_url_count, c(list(table = table), args))
  res <- check_http(httr::GET(url_count), table, stop = stop, args)
  if (!res[1] == "suppressed-error") {
    entries <- as.numeric(httr::content(res))
  } else {
    entries <- 0
  }

  # Feedback (only show, when non-complex query)
  if (!silent & path == "normal") cat_entries(entries, table)

  # Execute download with fetch_data
  if (entries > 0) {

    # Define skips
    skip <- cumsum(c(0, rep(package_size, floor(entries / package_size))))
    if (entries %% package_size == 0) skip <- skip[-length(skip)] # Prevent extra query

    # ProgressBar
    if (path == "normal") {

      if (length(skip) > 1 & !silent) {

        pb2 <- utils::txtProgressBar(min = 0, max = length(skip), style = 3)
        pb_pos2 <- c(1:length(skip))
        pb_max2 <- length(skip)

        } else {

          pb2 <- NULL
          pb_pos2 <- c(1:length(skip))
          pb_max2 <- length(skip)

          }

    }
    if (path == "complex") {

      pb2 <- NULL
      pb_pos2 <- c(1:length(skip))
      pb_max2 <- NULL

      if (!is.null(pb)) {

        utils::setTxtProgressBar(pb, pb_pos)
        if (pb_pos == pb_max) close(pb)

      }

    }

    # Fetch Data
    purrr::map2_dfr(
      skip,
      pb_pos2,
      fetch_data,
      table = table,
      package_size = package_size,
      pb = pb2,
      pb_max = pb_max2,
      stop = stop,
      attempts = attempts,
      wtf = wtf,
      silent = silent,
      path = path,
      args
    )

  } else {

    return(invisible())

  }

}
