#' Retrieve affair types
#'
#' \code{get_affair_types} retrieves all existing types of affairs (parliamentary instruments etc.) used in the Swiss Parliament.
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of affair type.
#' \item \code{updated} date of last update.
#' \item \code{abbreviation} language-specific short form of affair type.
#' \item \code{code} code of affair type.
#' \item \code{name} full language-specific description of affair type.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_affair_types(lang = "fr")
#' }
get_affair_types <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/affairs/types"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent)

}

#' Retrieve affair states
#'
#' \code{get_affair_states} retrieves all possible states of affairs of the Swiss Parliament.
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of affair state.
#' \item \code{updated} date of last update.
#' \item \code{code} code of affair state.
#' \item \code{name} full language-specific description of affair type.
#' \item \code{sorting} sorting vector.
#' \item \code{comment} additional information on use of affair state.
#' \item \code{parent.id} additional ID.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_affair_states(lang = "fr")
#' }
get_affair_states <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/affairs/states"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent)

}

#' Retrieve affair topics
#'
#' \code{get_affair_topics} retrieves all topics/keywords used to classify the content of affairs of the Swiss Parliament.
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of topic/keyword.
#' \item \code{updated} date of last update.
#' \item \code{code} code of topic/keyword.
#' \item \code{name} language-specific topic/keyword.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_affair_topics(lang = "fr")
#' }
get_affair_topics <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/affairs/topics"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent)

}

#' Retrieve affair descriptors
#'
#' \code{get_affair_descriptors} retrieves all descriptors used to classify the content of affairs of the Swiss Parliament.
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of descriptors.
#' \item \code{updated} date of last update.
#' \item \code{code} code of descriptor.
#' \item \code{name} language-specific descriptor.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_affair_descriptors(lang = "fr")
#' }
get_affair_descriptors <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/affairs/descriptors"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent)

}

#' Retrieve basic committee information
#'
#' \code{get_committees} retrieves basic information on all committees of the Swiss Parliament.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param main if \code{TRUE} only main committees are retrieved.
#' @param permanent if \code{TRUE} only permanent committees are retrieved.
#' @param current if \code{TRUE} only current committees are retrieved.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 17 columns, including:
#' \itemize{
#' \item \code{id} ID of committee.
#' \item \code{abbreviation} code of committee.
#' \item \code{from} date of first convocation.
#' \item \code{isActive} status of activity.
#' \item \code{name} full language-specific description of committee.
#' \item \code{to} date of dissolution.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_committees(lang = "fr")
#' }
get_committees <- function(main = F, permanent = F, current = F, lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/committees"

  # Define additional query modifiers
  add <-  paste0(query_add_log("mainOnly", main), query_add_log("permanentOnly", permanent), query_add_log("currentOnly", current))

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, add = add, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve basic cantonal information
#'
#' \code{get_cantons} retrieves basic information on cantons.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of canton.
#' \item \code{updated} date of last update.
#' \item \code{abbreviation} short form of canton.
#' \item \code{code} code of canton.
#' \item \code{name} full language-specific description of canton.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_cantons(lang = "fr")
#' }
get_cantons <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/cantons"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve current councillors
#'
#' \code{get_councillors} retrieves the current councillors of the Swiss Parliament.
#'
#' @param council specifies council. Use \code{council = "N"} for the National Council and \code{council = "S"} for
#'     the Council of States to obtain council-specific results.
#' @param canton specifies canton. Use two-letter abbreviations to obtain canton-specific results.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 16 columns, including:
#' \itemize{
#' \item \code{id} ID of councillor.
#' \item \code{biographyUrl} link to official biography.
#' \item \code{canton} canton affiliation.
#' \item \code{council} council affiliation.
#' \item \code{faction} faction affiliation.
#' \item \code{party} party affiliation.
#' \item \code{homepage} url of personal homepage.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_councillors(lang = "fr")
#' }
get_councillors <- function(council = NULL, canton = NULL, lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/councillors/basicdetails"

  # Define additional query modifiers
  add <- paste0(
    query_add("councilFilter", council),
    query_add("cantonFilter", canton)
    )

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, add = add, silent = silent)

}

#' Retrieve all councillors
#'
#' \code{get_councillors_historic} retrieves all councillors in the history of the Swiss Parliament.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param date_of_death_from filters by date of death after a given date. Recommended date format \code{YYYY/MM/DD}.
#' @param date_of_death_to filters by date of death before a given date. Recommended date format \code{YYYY/MM/DD}.
#' @param faction specifies faction affiliation.
#' @param legislative_period_from specifies legislative period.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 28 columns, including:
#' \itemize{
#' \item \code{id} ID of councillor.
#' \item \code{birthDate} date of birth.
#' \item \code{dateOfDeath} date of death.
#' \item \code{membership.entryDate} Council specific terms of office, start.
#' \item \code{membership.leavingDate} Council specific terms of office, end.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_councillors_historic(lang = "fr")
#' }
get_councillors_historic <- function(
  date_of_death_from = NULL, date_of_death_to = NULL, faction = NULL, legislative_period_from = NULL, lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/councillors/historic"

  # Define additional query modifiers
  add <- paste0(
    query_add("dateOfDeathFromFilter", date_of_death_from),
    query_add("dateOfDeathToFilter", date_of_death_to),
    query_add("factionFilter", faction),
    query_add("legislativePeriodFromFilter", legislative_period_from)
  )

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, add = add, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve basic council information
#'
#' \code{get_councils} retrieves basic information on the councils of the Swiss Parliament.
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#'@return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of council.
#' \item \code{updated} date of last update.
#' \item \code{abbreviation} short form of council.
#' \item \code{code} code of council.
#' \item \code{name} language-specific name of council.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_councils(lang = "fr")
#' }
get_councils <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/councils"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve basic information on the current departments
#'
#' \code{get_departments} retrieves basic information on the current departments of the Swiss government.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#'@return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of department.
#' \item \code{updated} date of last update.
#' \item \code{abbreviation} short form of department.
#' \item \code{code} code of department.
#' \item \code{name} language-specific name of department.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_departments(lang = "fr")
#' }
get_departments <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/departments"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve historical information on the departments
#'
#' \code{get_departments_historic} retrieves basic information on all departments in the history of the Swiss government.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#'@return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of department.
#' \item \code{updated} date of last update.
#' \item \code{abbreviation} short form of department.
#' \item \code{code} code of department.
#' \item \code{name} language-specific name of department.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_departments_historic(lang = "fr")
#' }
get_departments_historic <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/departments/historic"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve basic information on the current factions
#'
#' \code{get_factions} retrieves basic information on the current factions of the Swiss Parliament.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#'@return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of faction.
#' \item \code{updated} date of last update.
#' \item \code{abbreviation} short form of faction.
#' \item \code{code} code of faction.
#' \item \code{name} language-specific name of faction.
#' \item \code{shortName} language-specific short name of faction.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_factions(lang = "fr")
#' }
get_factions <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/factions"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve historical information on the factions
#'
#' \code{get_factions_historic} retrieves basic information on all factions in the history of the Swiss Parliament.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#'@return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of faction.
#' \item \code{updated} date of last update.
#' \item \code{abbreviation} short form of faction.
#' \item \code{code} code of faction.
#' \item \code{from} first presence in Parliament.
#' \item \code{name} language-specific name of faction.
#' \item \code{shortName} language-specific short name of faction.
#' \item \code{to} last presence in Parliament.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_factions_historic(lang = "fr")
#' }
get_factions_historic <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/factions/historic"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve legislative periods
#'
#' \code{get_legislative_periods} retrieves basic information on all available legislative periods.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of legislative period.
#' \item \code{updated} date of last update.
#' \item \code{code} code of legislative period.
#' \item \code{from} start date of legislative period.
#' \item \code{name} full language-specific description of legislative period.
#' \item \code{to} end date of legislative period.
#' }
#' @export
#'
#' @examples
#' \donttest{
#'get_legislative_periods(lang = "fr")
#' }
get_legislative_periods <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/legislativeperiods"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve historical party information
#'
#' \code{get_parties_historic} retrieves basic information on all parties in the history of the Swiss Parliament.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#'@return A tibble with the following columns:
#' \itemize{
#' \item \code{id} ID of party.
#' \item \code{updated} date of last update.
#' \item \code{abbreviation} short form of party.
#' \item \code{code} code of party.
#' \item \code{name} language-specific name of party.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_parties_historic(lang = "fr")
#' }
get_parties_historic <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/parties/historic"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve yearly schedules
#'
#' \code{get_schedules} retrieves yearly schedules of the Swiss Parliament.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param year specifies year. No data available for \code{year > 2015}.
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with 12 columns.
#' @export
#'
#' @examples
#' \donttest{
#' get_schedules(lang = "fr")
#' }
get_schedules <- function(year = 2010, lang = "de", silent = F) {

  # Define page
  ws_page = paste0("http://ws-old.parlament.ch/schedules/", year, "/ALL")

  # Check availability
  if (check404(ws_page)) stop ("No schedule data available for ", year)

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve sessions
#'
#' \code{get_sessions} retrieves basic information on all sessions of the Swiss Parliament since 1990.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with the following columns:
#' \itemize{
#' \item \code{to} end of session.
#' \item \code{name} language-specific session name.
#' \item \code{from} start of session.
#' \item \code{code} code of session.
#' \item \code{updated} date of last update.
#' \item \code{id} session ID.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_sessions(lang = "fr")
#' }
get_sessions <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/sessions"

  # Initiate
  if (!silent) cat("\nFetching data from", ws_page, "\n")

  # Fetch data
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' Retrieve affairs
#'
#' \code{get_affairs} retrieves basic information on all affairs of the Swiss Parliament since 1990.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @param lang specifies language. Available are German ("de"), French ("fr"), Italian ("it") and English ("en").
#' @param silent if \code{TRUE}, no progress bar is displayed.
#'
#' @return A tibble with the following columns:
#' \itemize{
#' \item \code{to} end of session.
#' \item \code{name} language-specific session name.
#' \item \code{from} start of session.
#' \item \code{code} code of session.
#' \item \code{updated} date of last update.
#' \item \code{id} session ID.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' get_affairs(lang = "fr")
#' }
get_affairs <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/affairs"

  # Initiate
  if (!silent) cat("\n Fetching data from", ws_page, "\n")

  # Data collection
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}
