# Cat for number of entries
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_remove_all str_replace_all str_c str_detect
#' @importFrom crayon green silver
#' @noRd
cat_entries <- function(entries, table) {

  if (entries == 0) cat(crayon::silver("\n\n   No entries found."))
  if (entries == 1) cat(crayon::green("\n\n   1 entry found\n"))
  if (entries > 1) cat(crayon::green(paste0("\n\n   ", entries), "entries found\n"))
  if (entries > 0) cat(crayon::silver(paste0("   Fetching data from table '", table, "'\n\n")))

}

# Check of http status
#' @importFrom httr status_code http_status
#' @noRd
check_http <- function(res, table, stop, ...) {

  # Catch ... (workaround for both ways)
  if (...length() > 0) {
    args <- ...elt(...length())
    if (length(args) > 0) if (!is.list(args[1])) args <- list(...)
  } else {
    args <- NULL
  }

  # Status code
  code <- httr::status_code(res)

  if (stop) {

    if (code >= 400) {

      # Check for wrong table
      if (!table %in% get_tables()) {

        stop (
          "The requested table '",
          table,
          "' does not exist.\nUse get_tables() to check for available tables."
        )

      }

      # Check for wrong variables
      var_diff <- setdiff(names(args), get_variables(table))
      if (length(var_diff) > 0) {

        # 1 mistake
        if (length(var_diff) == 1) {

          stop (
            "Unknow filter variable '",
            var_diff,
            "'. \nUse get_variables(table = \"",
            table,
            "\") to check for available filter variables."
          )

        }

        # >1 mistakes
        if (length(var_diff) > 1) {

          stop (
            "Unknow filter variables: '",
            paste0(var_diff, collapse = "', '"),
            "'. \nUse get_variables(table = \"",
            table,
            "\") to check for available filter variables."
          )

        }

      }

      # Other reasons
      status <- httr::http_status(res)
      stop(status$message)

    } else {

      return(res)

    }

  } else {

    if (code >= 400) {

      return(invisible("suppressed-error"))

    } else {

      return(res)

    }

  }

}

# Check length of url
#' @noRd
check_url_length <- function(table, package_size, ...) {

  nchar(get_url_skip(table, package_size, 0, ...))

}
