# Build filter from arguments
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_remove_all str_replace_all str_c str_detect
#' @noRd
build_filter <- function(arg, input) {

  # Suppress exponential notation for numerics and reset
  scipen_default <- options(scipen = 999)
  on.exit(options(scipen_default))

  if (length(input) > 0) {

    # Deleting NA and duplicates
    input <- input[!is.na(input)]
    input <- unique(input)

    # Single input
    if (length(input) == 1 & !stringr::str_detect(input[1], "[><~]") & is.na(get_date(input[1]))) {

      if (is.numeric(input)) {

        filter <- paste0(arg, " eq ", input)

      } else {

        filter <- paste0(arg, " eq '", input, "'")

      }

      # Return
      return(filter)

    }

    # One date with '><'
    if (length(input) == 1 & !is.na(get_date(input[1])) & stringr::str_detect(input[1], "[><]")) {

    filter <- paste(
      arg,
      input %>%
      stringr::str_remove_all("\\s") %>%
      stringr::str_replace_all("<", "lt datetime'") %>%
      stringr::str_replace_all(">", "gt datetime'") %>%
      stringr::str_c("'")
    )

    return(filter)

  }

    # Two dates with '><' (Period)
    if (length(input) == 2 & !is.na(get_date(input[1])) & !is.na(get_date(input[2])) & sum(stringr::str_detect(input, "[><]")) == 2) {

    # Dates
    filter <- paste(
      arg,
      input %>%
        stringr::str_remove_all("\\s") %>%
        stringr::str_replace_all("<", "lt datetime'") %>%
        stringr::str_replace_all(">", "gt datetime'") %>%
        stringr::str_c("'"),
      collapse = " and "
      )

    # Return
    return(filter)

  }

    # Multiple dates (or-chain)
    if (!is.na(get_date(input[1]))) {

      # Feedback
      if (sum(stringr::str_detect(input, "[><]")) > 0) stop ("> and < can only be used with a maximum of 2 dates.")

      # Dates
      filter <- paste0(arg, " eq datetime'", input, "'", collapse = " or ")
      filter <- paste0("(", filter, ")")

      # Return
      return(filter)

    }

    # One quasi-numeric input with '><'
    if (length(input) == 1 & stringr::str_detect(input[1], "[><]")) {

      filter <- paste(
        arg,
        input %>%
          stringr::str_remove_all("\\s") %>%
          stringr::str_replace_all("<", "lt ") %>%
          stringr::str_replace_all(">", "gt ")
        )

      return(filter)

    }

    # Two quasi-numeric inputs with '><'
    if (length(input) == 2 & stringr::str_detect(input[1], "[><]")) {

      # Dates
      filter <- paste(
        arg,
        input %>%
          stringr::str_remove_all("\\s") %>%
          stringr::str_replace_all("<", "lt ") %>%
          stringr::str_replace_all(">", "gt "),
        collapse = " and "
      )

      # Return
      return(filter)

    }

    # Single input with ~(substring of)
    if (length(input) == 1 & stringr::str_detect(input[1], "[~]")) {

      text <- stringr::str_remove_all(input, "~")
      filter <- paste0("substringof('", text, "', ", arg, ") eq true")

      return(filter)

    }

    # Multiple input with ~(substring of)
    if (length(input) > 1 & stringr::str_detect(input[1], "[~]")) {

      # First
      start <- stringr::str_remove_all(input[1], "~")
      filter <- paste0("substringof('", start, "', ", arg, ") eq true")

      # Or-Statements
      ors <- input[stringr::str_detect(input, "or ")]
      ors <- stringr::str_remove_all(ors, "or ")
      if (length(ors) > 0) {

        ors <- paste0("substringof('", ors, "', ", arg, ") eq true", collapse = " or ")
        filter <- paste0("(", filter, " or ", ors, ")")

        }

      # And-Statments
      ands <- input[stringr::str_detect(input, "and ")]
      ands <- stringr::str_remove_all(ands, "and ")
      if (length(ands) > 0) {

        ands <- paste0("substringof('", ands, "', ", arg, ") eq true", collapse = " and ")
        filter <- paste0(filter, " and ", ands)

        }

      # Return
      return(filter)

    }

    # Multiple inputs (or-chains)
    if (length(input) > 1) {

      if (sum(stringr::str_detect(input, "[><]")) > 0) stop ("> and < can only be used with a maximum of 2 numbers.")
      if (is.numeric(input[1])) {

        filter <- paste0(arg, " eq ", input, collapse = " or ")
        filter <- paste0("(", filter, ")")
        return(filter)

      } else {

        filter <- paste0(arg, " eq '", input, "'", collapse = " or ")
        filter <- paste0("(", filter, ")")
        return(filter)

      }

    }

    } else {

      return(invisible())

    }

  }

# Assemble filter for URL
#' @importFrom purrr map2
#' @noRd
assemble_filter <- function(...) {

  # Suppress exponential notation for numerics and reset
  scipen_default <- options(scipen = 999)
  on.exit(options(scipen_default))

  # Catch ...
  args <- list(...)

  # Assemble filter
  filter <- paste0(
    suppressWarnings(
      purrr::map2(names(args), unname(args), build_filter)
    ),
    collapse = " and "
  )

  # Return
  return(filter)

}

# Get URL for count query
#' @importFrom utils URLencode
#' @noRd
get_url_count <- function(table, ...) {

  # Suppress exponential notation for numerics and reset
  scipen_default <- options(scipen = 999)
  on.exit(options(scipen_default))

  # Catch ... (workaround for both ways)
  if (...length() > 0) {
    args <- ...elt(...length())
    if (length(args) > 0) if (!is.list(args[1])) args <- list(...)
  } else {
    args <- NULL
  }

  # Assemble filter
  filter <- ""
  if (length(args) > 0) filter <- do.call(assemble_filter, args)

  # Build URL
  url <- paste0(
    "https://ws.parlament.ch/odata.svc/",
    table,
    "/$count",
    ifelse(filter == "", "", paste0("?$filter=", filter))
  )

  # Return URL
  utils::URLencode(url)

}

# Get URL for skip query
#' @importFrom utils URLencode
#' @noRd
get_url_skip <- function(table, package_size, skip, ...) {

  # Suppress exponential notation for numerics and reset
  scipen_default <- options(scipen = 999)
  on.exit(options(scipen_default))

  # Catch ... (workaround for both ways)
  if (...length() > 0) {
    args <- ...elt(...length())
    if (length(args) > 0) if (!is.list(args[1])) args <- list(...)
  } else {
    args <- NULL
  }

  # Assemble filter
  filter <- ""
  if (length(args) > 0) filter <- do.call(assemble_filter, args)

  # Build URL
  url <- paste0(
    "https://ws.parlament.ch/odata.svc/",
    table,
    "?$top=", package_size,
    "&$skip=", skip,
    ifelse(filter == "", "", paste0("&$filter=", filter))
  )

  # Return URL
  utils::URLencode(url)

}
