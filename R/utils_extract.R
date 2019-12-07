# Get date from input
#' @importFrom stringr str_remove_all
#' @noRd
get_date <- function(input) {

  suppressWarnings(
    as.Date(
      stringr::str_remove_all(input, "[^0-9-]"),
      "%Y-%m-%d"
      )
    )

}

# Get numeric from input
#' @importFrom stringr str_remove_all
#' @noRd
get_numeric <- function(input, pattern) {

  suppressWarnings(
    as.numeric(
      stringr::str_remove_all(input, pattern)
    )
  )

}

# Convert date
#' @importFrom stringr str_remove_all
#' @noRd
convert_date <- function(date) {

  as.Date(
    as.POSIXct(
      as.numeric(
        stringr::str_remove_all(
          date, "[^0-9-]")
        ) / 1000,
      tz = "Europe/London",
      origin = "1970-01-01"
      )
    )

}

# Convert datetime
#' @importFrom stringr str_remove_all
#' @noRd
convert_datetime <- function(date) {

  as.POSIXct(
    as.numeric(
      stringr::str_remove_all(
        date, "[^0-9-]")
      ) / 1000,
    tz = "Europe/London",
    origin = "1970-01-01"
    )

}

# Convert datetime with timezone
#' @importFrom stringr str_remove_all
#' @noRd
convert_datetime_tz <- function(date) {

  date <- stringr::str_sub(date, 0, 19)

  as.POSIXct(
    as.numeric(
      stringr::str_remove_all(
        date, "[^0-9-]")
      ) / 1000 - 3600,
    tz = "Europe/London",
    origin = "1970-01-01"
  )

}
