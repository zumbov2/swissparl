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

#' Retrieve data from the OpenParlData.ch REST API
#'
#' \code{get_data2} retrieves data from the OpenParlData.ch REST API for a 
#'    given resource.
#'
#' @param table name of the OpenParlData resource to download. For an overview of available
#'   endpoints use \code{\link{get_tables2}()}.
#' @param max_rows maximum number of rows to return. If omitted, all
#'   available rows matching the query are downloaded.
#' @param package_size number of rows to download per request (mapped to
#'   the API parameter \code{limit}). Default is 1000. If the result set exceeds 
#'   \code{package_size}, multiple requests are made using the API's pagination links.
#' @param silent if \code{TRUE}, no progress bar and messages are displayed.
#' @param ... additional query parameters passed to the OpenParlData endpoint as
#'   URL query parameters. Common parameters include:
#'   \itemize{
#'     \item \code{search}: search query string.
#'     \item \code{search_mode}: one of \code{"partial"} (default), \code{"exact"},
#'       \code{"natural"}, \code{"boolean"}.
#'     \item \code{search_scope}: where to search
#'     \item \code{search_language}: language-specific search (\code{de}, \code{fr},
#'       \code{it}, \code{rm}, \code{en}).
#'     \item \code{sort_by}: field(s) to sort by; prefix with \code{-} for descending.
#'   }
#'   Resource-specific filters (e.g. \code{body_key}, \code{lang}, etc.) can also
#'   be supplied. Multiple values can be provided as an R vector and are encoded
#'   as a comma-separated query value (e.g. \code{body_key = c("AI", "AR")}).
#'
#' @return A tibble containing up to \code{max_rows} records. Column composition
#'   depends on the selected resource and query parameters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve first 10 persons
#' get_data2("persons", max_rows = 10)
#' 
#' # Retrieve a specific person by first and last name
#' get_data2(
#'   "persons",
#'   firstname = "Karin",
#'   lastname = "Keller-Sutter"
#' )
#'
#' # Partial search (default mode) in affairs
#' get_data2("affairs", max_rows = 10, search = "Budget", search_mode = "partial")
#'
#' # Boolean search with grouping (note: '&' is an operator in boolean mode)
#' get_data2(
#'   "affairs",
#'   max_rows = 10,
#'   search = "(Klima | Umwelt) & Schweiz",
#'   search_mode = "boolean"
#' )
#'
#' # Combine search with scope/language and sorting
#' get_data2(
#'   "affairs",
#'   max_rows = 10,
#'   search = "Bundesrat Parlament",
#'   search_mode = "natural",
#'   search_language = "de",
#'   sort_by = "-begin_date"
#' )
#'
#'
#' }
get_data2 <- function(table, max_rows, package_size = 1000, silent = FALSE, ...) {
  
  fetch_data2(
    table = table,
    package_size = package_size,
    max_rows = max_rows,
    silent = silent,
    ...
  )
}

#' List related tables available
#'
#' \code{get_related_tables2} returns the names of related tables that
#' are available for an OpenParlData record.
#'
#' @param res an OpenParlData record (typically one row) as returned by
#'   \code{\link{get_data2}()}.
#'
#' @return A sorted character vector containing the names of available related
#'   tables for the provided record.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve one person
#' res <- get_data2("persons", firstname = "Gerhard", lastname = "Andrey")
#'
#' # List available related resources for that record
#' get_related_tables2(res)
#' }
get_related_tables2 <- function(res) {
  
  sort(names(res[["links"]]))
  
}

#' Retrieve related data for an OpenParlData record
#'
#' \code{get_related_data2} retrieves related records that are available for an
#' OpenParlData record via its linked resources.
#'
#' The function downloads the related data for a specified related table and
#' combines the results into a single tibble. If multiple entities are
#' present, the function iterates over them and optionally displays a progress bar.
#'
#' @param res an OpenParlData record (typically one row) as returned by
#'   \code{\link{get_data2}()}.
#' @param table name of the related table to retrieve. Use
#'   \code{\link{get_related_tables2}()} to see which related tables are available.
#' @param silent if \code{TRUE}, no progress bar and messages are displayed.
#'
#' @return A tibble containing the related records.
#'
#' @importFrom purrr map_dfr
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve one person
#' res <- get_data2("persons", firstname = "Gerhard", lastname = "Andrey")
#'
#' # List available related tables
#' get_related_tables2(res)
#'
#' # Retrieve related data (replace "memberships" with an available table)
#' get_related_data2(res, table = "memberships")
#' }
get_related_data2 <- function(res, table, silent = FALSE) {
  
  valid_tables <- get_related_tables2(res)
  
  if (!table %in% valid_tables) {
    stop(
      sprintf(
        "Invalid related table '%s'. Available related tables: %s.",
        table,
        paste(valid_tables, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  urls <- res[["links"]][[table]]
  
  if (length(urls) == 1) return(fetch_related_data2(url = urls))
  
  if (!silent) {
    cat_related_entities(length(urls), table)
    
    pb <- utils::txtProgressBar(min = 0, max = length(urls), style = 3)
    pb_pos <- 0
    }
  
  out <- purrr::map_dfr(seq_along(urls), function(i) {
    
    result <- fetch_related_data2(url = urls[[i]])
    
    if (!silent) {
      pb_pos <<- pb_pos + 1
      utils::setTxtProgressBar(pb, pb_pos)
      }
    
    result
    
    })
  
  if (!silent) close(pb)
  
  out
}

  