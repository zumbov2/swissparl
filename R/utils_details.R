#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom utils setTxtProgressBar
#' @noRd
get_affair_details <- function(affair_id, pos, prog_bar, lang, silent) {

  # Define
  ws_page <- paste0("http://ws-old.parlament.ch/affairs/", affair_id, query_lang(lang))

  # Get data
  if (!check404(ws_page)) {

    # Get data
    dt <- jsonlite::fromJSON(ws_page, flatten = F)

    # Progress
    if (!silent) utils::setTxtProgressBar(prog_bar, pos)

    # Output
    res <- tibble::tibble(
      id = empty2NA(dt[["id"]]),
      updated = empty2NA(dt[["updated"]]),
      additionalIndexing = empty2NA(dt[["additionalIndexing"]]),
      affairType = empty2NA(dt["affairType"]),
      author = empty2NA(dt["author"]),
      deposit = empty2NA(dt["deposit"]),
      descritpors = empty2NA(dt["descritpors"]),
      drafts = empty2NA(dt["drafts"]),
      handling = empty2NA(dt["handling"]),
      language = empty2NA(dt[["language"]]),
      priorityCouncils = empty2NA(dt["priorityCouncils"]),
      relatedAffairs = empty2NA(dt["relatedAffairs"]),
      roles = empty2NA(dt["roles"]),
      sequentialNumber = empty2NA(dt[["sequentialNumber"]]),
      shortId = empty2NA(dt[["shortId"]]),
      state = empty2NA(dt["state"]),
      texts = empty2NA(dt["texts"]),
      title = empty2NA(dt[["title"]])
    )

    return(res)

  }

}

#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom utils setTxtProgressBar
#' @noRd
get_affairsummaries_details <- function(affair_id, pos, prog_bar, lang, silent) {

  # Define
  ws_page <- paste0("http://ws-old.parlament.ch/affairsummaries/", affair_id, query_lang(lang))

  # Get data
  if (!check404(ws_page)) {

    # Get data
    dt <- jsonlite::fromJSON(ws_page)

    # Progress
    if (!silent) utils::setTxtProgressBar(prog_bar, pos)

    # Output
    tibble::tibble(
      id = empty2NA(dt[["id"]]),
      updated = empty2NA(dt[["updated"]]),
      description = empty2NA(dt[["description"]]),
      formattedId = empty2NA(dt[["formattedId"]]),
      initialSituation = empty2NA(dt[["initialSituation"]]),
      proceedings = empty2NA(dt[["proceedings"]]),
      title = empty2NA(dt[["id"]])
    )

  }

}

#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom utils setTxtProgressBar
#' @noRd
get_councillor_details <- function(councillor_id, pos, prog_bar, lang, silent) {

  # Define
  ws_page <- paste0("http://ws-old.parlament.ch/councillors/", councillor_id, query_lang(lang))

  # Get data
  if (!check404(ws_page)) {

    # Get data
    dt <- jsonlite::fromJSON(ws_page)

    # Progress
    if (!silent) utils::setTxtProgressBar(prog_bar, pos)

    # Output
    tibble::tibble(
      id = empty2NA(dt[["id"]]),
      updated = empty2NA(dt[["updated"]]),
      canton = empty2NA(dt[["canton"]]),
      cantonName = empty2NA(dt[["cantonName"]]),
      council = empty2NA(dt[["council"]]),
      faction = empty2NA(dt[["faction"]]),
      factionName = empty2NA(dt[["factionName"]]),
      firstName = empty2NA(dt[["firstName"]]),
      lastName = empty2NA(dt[["lastName"]]),
      funktion = empty2NA(dt[["function"]]),
      number = empty2NA(dt[["number"]]),
      party = empty2NA(dt[["party"]]),
      partyName = empty2NA(dt[["partyName"]]),
      active = empty2NA(dt[["active"]]),
      birthDate = empty2NA(dt[["birthDate"]]),
      birthPlace = empty2NA(dt["birthPlace"]),
      code = empty2NA(dt[["code"]]),
      committeeMemberships = empty2NA(dt["committeeMemberships"]),
      concerns = empty2NA(dt["concerns"]),
      contact = empty2NA(dt["contact"]),
      councilMemberships = empty2NA(dt["councilMemberships"]),
      displayLanguage = empty2NA(dt[["displayLanguage"]]),
      domicile = empty2NA(dt["domicile"]),
      factionId = empty2NA(dt[["factionId"]]),
      gender = empty2NA(dt[["gender"]]),
      homePlaces = empty2NA(dt["homePlaces"]),
      language = empty2NA(dt[["language"]]),
      mandate = empty2NA(dt["mandate"]),
      maritalStatus = empty2NA(dt[["maritalStatus"]]),
      militaryGrade = empty2NA(dt[["militaryGrade"]]),
      numberOfChildren = empty2NA(dt[["numberOfChildren"]]),
      officialDenomination = empty2NA(dt[["officialDenomination"]]),
      partyId = empty2NA(dt[["partyId"]]),
      postalAddress = empty2NA(dt["postalAddress"]),
      professions = empty2NA(dt["professions"]),
      salutationLetter = empty2NA(dt[["salutationLetter"]]),
      salutationTitle = empty2NA(dt[["salutationTitle"]]),
      title = empty2NA(dt[["title"]]),
      workLanguage = empty2NA(dt[["workLanguage"]]),
      dateOfDeath = empty2NA(dt[["dateOfDeath"]])
    )

  }

}

#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom utils setTxtProgressBar
#' @noRd
get_committee_members_single <- function(committee_id, pos, prog_bar, lang, silent) {

  # Define
  ws_page <- paste0("http://ws-old.parlament.ch/committees/", committee_id, query_lang(lang))

  # Get data
  dt <- jsonlite::fromJSON(ws_page)

  # Progress
  if (!silent) utils::setTxtProgressBar(prog_bar, pos)

  # Output
  if (!rlang::is_empty(dt[["members"]])) {

    dt[["members"]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        id = empty2NA(dt[["id"]]),
        updated = empty2NA(dt[["updated"]]),
        abbreviation = empty2NA(dt[["abbreviation"]]),
        code = empty2NA(dt[["code"]]),
        committeeNumber = empty2NA(dt[["committeeNumber"]]),
        council = empty2NA(dt["council"]),
        name = empty2NA(dt[["name"]])
      )

    }

}

#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom utils setTxtProgressBar
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @importFrom rlang is_empty
#' @noRd
get_faction_members_single <- function(faction_id, pos, prog_bar, lang, silent) {

  # Get data
  dt <- jsonlite::fromJSON(paste0("http://ws-old.parlament.ch/factions/", faction_id, query_lang(lang)))

  # Progress
  if (!silent) utils::setTxtProgressBar(prog_bar, pos)

  # Output
  if (!rlang::is_empty(dt[["members"]])) {

    dt[["members"]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        id = empty2NA(dt[["id"]]),
        updated = empty2NA(dt[["updated"]]),
        abbreviation = empty2NA(dt[["abbreviation"]]),
        code = empty2NA(dt[["code"]]),
        name = empty2NA(dt[["name"]]),
        shortName = empty2NA(dt[["shortName"]])
        )

  }

}

#' @importFrom dplyr select
#' @noRd
get_ids_affairs <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/affairs"

  # Initiate
  if (!silent) cat("Collecting ids from", ws_page, "\n")

  # Data collection
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' @importFrom dplyr select
#' @noRd
get_ids_affairsummaries <- function(lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/affairsummaries"

  # Initiate
  if (!silent) cat("Collecting ids from", ws_page, "\n")

  # Data collection
  get_overview(ws_page = ws_page, lang = lang, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' @importFrom dplyr select
#' @noRd
get_ids_votes_affairs <- function(
  from = NULL, to = NULL, legislative_period = NULL, session = NULL,
  search_text = NULL, lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/votes/affairs"

  # Modify query
  add <- paste0(
    query_add("dateFromFilter", from),
    query_add("dateToFilter", to),
    query_add("legislativePeriodFilter", legislative_period),
    query_add("sessionFilter", session),
    query_add("searchTextFilter", search_text)
  )

  # Initiate
  if (!silent) cat("Collecting ids from", ws_page, "\n")

  # Data collection
  get_overview(ws_page = ws_page, lang = lang, add = add, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' @importFrom dplyr select
#' @noRd
get_ids_votes_councillors <- function(
  from = NULL, to = NULL, legislative_period = NULL, session = NULL, canton = NULL,
  faction = NULL, decisions = NULL, search_text = NULL, lang = "de", silent = F) {

  # Define page
  ws_page = "http://ws-old.parlament.ch/votes/councillors"

  # Modify query
  add <- paste0(
    query_add("dateFromFilter", from),
    query_add("dateToFilter", to),
    query_add("legislativePeriodFilter", legislative_period),
    query_add("sessionFilter", session),
    query_add("cantonFilter", canton),
    query_add("factionFilter", faction),
    query_add("searchTextFilter", search_text)
  )

  # Initiate
  if (!silent) cat("Collecting ids from", ws_page, "\n")

  # Data collection
  get_overview(ws_page = ws_page, lang = lang, add = add, silent = silent) %>%
    dplyr::select(-hasMorePages)

}

#' @importFrom jsonlite fromJSON flatten
#' @importFrom tibble as_tibble
#' @importFrom utils setTxtProgressBar
#' @importFrom dplyr select mutate rename
#' @importFrom magrittr "%>%"
#' @noRd
get_votes_councillor_page <- function(page, ws_page, prog_bar, silent) {

  # Get data
  dt <- jsonlite::fromJSON(paste0(ws_page, "&pageNumber=", page))

  # Progress
  if (!silent) utils::setTxtProgressBar(prog_bar, page)

  # Output
  votes <- jsonlite::flatten(dt[["affairVotes"]]) %>%
    jsonlite::flatten("councillorVote") %>%
    dplyr::select(-hasMorePages) %>%
    dplyr::rename(vote.id = id)

  res <- votes %>%
    dplyr::mutate(
    councillor.id = empty2NA(dt[["id"]]),
    updated = empty2NA(dt[["updated"]]),
    elanId = empty2NA(dt[["elanId"]]),
    firstName = empty2NA(dt[["firstName"]]),
    lastName = empty2NA(dt[["lastName"]])
    ) %>% select(councillor.id:lastName, vote.id:councillorVote.decision) %>%
    tibble::as_tibble()

  # Return
  return(res)

}

#' @importFrom utils txtProgressBar
#' @importFrom purrr map_dfr
get_votes_councillor <- function(councillor_id, lang, add, silent) {

  # URL
  ws_page <- paste0("http://ws-old.parlament.ch/votes/councillors/", councillor_id, query_lang(lang))
  if (!is.null(add)) ws_page <- paste0(ws_page, add)

  # Check resource
  if (!check404(ws_page)) {

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
    if (!silent) cat("\nFetching data from", ws_page, "\n")
    res <- purrr::map_dfr(1:number_of_pages, get_votes_councillor_page, ws_page = ws_page, prog_bar = pb, silent = silent)

    # Return
    return(res)

  } else {

    if (!silent) cat("\nNo data found for", ws_page, "\n")

  }

}

#' @importFrom jsonlite fromJSON flatten
#' @importFrom utils setTxtProgressBar
#' @importFrom dplyr select mutate rename bind_cols
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @noRd
get_votes_affair <- function(affair_id, pos, prog_bar, lang, silent, sec = F) {

  # Define
  ws_page <- paste0("http://ws-old.parlament.ch/votes/affairs/", affair_id, query_lang(lang))

  # Check resource
  if (sec) {

    if (!check404(ws_page)) {

      # Get data
      dt <- jsonlite::fromJSON(ws_page)

      # Progress
      if (!silent) utils::setTxtProgressBar(prog_bar, pos)

      # Output
      votes <- jsonlite::flatten(dt[["affairVotes"]]) %>%
        dplyr::select(-hasMorePages)

      res <- tibble::tibble(
        id = empty2NA(dt[["id"]]),
        updated = empty2NA(dt[["updated"]]),
        title = empty2NA(dt[["title"]]),
        vote.id = empty2NA(votes[["id"]]),
        councillorVotes = empty2NA(votes[["councillorVotes"]]),
        date = empty2NA(votes[["date"]]),
        divisionText = empty2NA(votes[["divisionText"]]),
        meaningNo = empty2NA(votes[["meaningNo"]]),
        meaningYes = empty2NA(votes[["meaningYes"]]),
        registrationNumber = empty2NA(votes[["registrationNumber"]]),
        submissionText = empty2NA(votes[["submissionText"]])
      ) %>% bind_cols(
        get_result(votes[["filteredTotalVotes"]])
      )

      return(res)

      }

    } else {

      # Get data
      dt <- jsonlite::fromJSON(ws_page)

      # Progress
      if (!silent) utils::setTxtProgressBar(prog_bar, pos)

      # Output
      votes <- jsonlite::flatten(dt[["affairVotes"]]) %>%
        dplyr::select(-hasMorePages)

      res <- tibble::tibble(
        id = empty2NA(dt[["id"]]),
        updated = empty2NA(dt[["updated"]]),
        title = empty2NA(dt[["title"]]),
        vote.id = empty2NA(votes[["id"]]),
        councillorVotes = empty2NA(votes[["councillorVotes"]]),
        date = empty2NA(votes[["date"]]),
        divisionText = empty2NA(votes[["divisionText"]]),
        meaningNo = empty2NA(votes[["meaningNo"]]),
        meaningYes = empty2NA(votes[["meaningYes"]]),
        registrationNumber = empty2NA(votes[["registrationNumber"]]),
        submissionText = empty2NA(votes[["submissionText"]])
      ) %>% dplyr::bind_cols(
        get_result(votes[["filteredTotalVotes"]])
      )

      return(res)

  }


}
