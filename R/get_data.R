#' Retrieve data from WebServices
#'
#' \code{get_data} retrieves data from the WebServices of the Swiss Parliament.
#'
#' @importFrom httr GET content
#' @importFrom utils txtProgressBar
#' @importFrom purrr map_dfr
#' @importFrom crayon silver
#'
#' @param table name of the table to download. For an overview of available tables use \code{\link{get_tables}()}.
#' @param package_size number of rows to download at once (maximum = 1000). If a query exceeds \code{package_size},
#'     it is internally split into multiple subqueries of size \code{package_size}.
#' @param stop if \code{TRUE}, the query process is interrupted if the query is invalid. It also indicates whether
#'     a non-existent table or variable was used in the query. If \code{FALSE}, nothing is returned.
#' @param attempts maximum number of repetitions of a single subquery if it was not successful.
#' @param wtf factor for extending the waiting time after unsuccessful queries. If \code{wtf = 1}, the waiting
#'     time corresponds to the number of unsuccessful attempts in seconds. For \code{attemps = 10} and
#'     \code{wtf = 1}, a query is repeated for a maximum of 45 seconds. The waiting time increases proportionally
#'     with \code{wtf}.
#' @param silent if \code{TRUE}, no progress bar and messages are displayed.
#' @param ... optional filter arguments with values. Since all entries are available in several languages, it is
#'     recommended to filter the calls by language., e.g. \code{get_data(table = "Person", Language = "DE")}.
#'     For a table-specific preview use \code{\link{get_glimpse}()} or \code{\link{get_variables}()}. The following
#'     things are to consider:
#'     \itemize{
#'     \item numbers for identification numbers, for example, must be entered as numeric vectors: e.g.
#'     \code{get_data(table = "Voting", PersonNumber = c(21, 4167), Language = "DE")}.
#'     \item dates must be entered as character vectors in yyyy-mm-dd format. \code{>} and \code{<} can be
#'     used to query periods: e.g. \code{get_data(table = "Bill", SubmissionDate = c(">2018-12-31", "<2019-02-01"),
#'      Language = "DE")}.
#'     \item the '~' can be used as substring search for character variables: e.g. \code{get_data(table = "Bill",
#'     Title = "~CO2", Language = "DE")}.
#'     }
#'
#' @return A tibble of different length and variable composition.
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve data on the members of the Swiss Parliament
#' get_data(table = "Person", Language = "DE")
#'
#' # Retrieve voting behavior of selected councillors
#' get_data(
#'    table = "Voting",
#'    PersonNumber = c(21, 4167),
#'    Language = "DE"
#'    )
#'
#' # Retrieve businesses submitted during a specified period
#' get_data(
#'     table = "Business",
#'     SubmissionDate = c(">2018-12-31", "<2019-02-01"),
#'     Language = "DE"
#'     )
#'
#' # Retrieve businesses on the subject of CO2
#' get_data(
#'     table = "Business",
#'     Title = "~CO2",
#'     Language = "DE"
#'     )
#' }
get_data <- function(table, package_size = 1000, stop = T, attempts = 10, wtf = 1, silent = F, ...) {

  # Check package size
  if (package_size > 1000) stop("rows must not be larger than 1000")

  # Check url length
  url_length <- check_url_length(table, package_size, ...)

  # Catch ... (workaround for both ways)
  if (...length() > 0) {
    args <- ...elt(...length())
    if (length(args) > 0) if (!is.list(args[1])) args <- list(...)
  } else {
    args <- NULL
  }

  # Choose a method
  if (url_length <= 2000) {

    # Query data for non-complex filter queries (short URLs)
    query_data(
      args,
      NULL,
      table = table,
      package_size = package_size,
      pb = NULL,
      pb_max = NULL,
      stop = stop,
      attempts = attempts,
      wtf = wtf,
      silent = silent,
      path = "normal"
    )

    } else {

      # Simplify complex filter queries (long URLs)
      subqueries <- get_simplified_queries(
        table = table,
        package_size = package_size,
        stop = stop,
        silent = silent,
        ...
        )

      # Suppressed Error
      if (subqueries[1] == "suppressed-error") {
        if (!silent) cat(crayon::silver("\n   No entries found."))
        return(invisible())
      }

      # ProgressBar
      if (!silent) {

        pb <- utils::txtProgressBar(min = 0, max = length(subqueries), style = 3)
        pb_pos <- c(1:length(subqueries))
        pb_max <- length(subqueries)

      } else {

        pb <- NULL
        pb_pos <- NULL
        pb_max <- NULL

      }

      # Query data with simplified sub-queries (shortened URLs)
      res <- purrr::map2_dfr(
        subqueries,
        pb_pos,
        query_data,
        table = table,
        package_size = package_size,
        pb = pb,
        pb_max = pb_max,
        stop = stop,
        attempts = attempts,
        wtf = wtf,
        silent = silent,
        path = "complex"
      )

      # In compliance with normal queries
      if (nrow(res) == 0) {
        if (!silent) cat(crayon::silver("\n   No entries found."))
        return(invisible())
      } else {
        return(res)
      }

    }

}
