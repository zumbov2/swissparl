# Check dataset for multiple variable names
#' @noRd
check_vars <- function(vars, data) {

  if (!sum(vars %in% names(data)) == length(vars)) {

    # Get missing variables
    var_diff <- vars[!vars %in% names(data)]

    # Error message
    if (length(var_diff) > 0) {

      # 1 missing
      if (length(var_diff) == 1) stop ("Variable '", var_diff , "' is missing from '", substitute(data),"'.")

      # >1 missing
      if (length(var_diff) > 1) stop ("Variables '", paste0(var_diff, collapse = "', '"), "' are missing from '", substitute(data),"'.")

      }

  }

}

# Highlight
#' @noRd
get_hgl <- function(x, conditions, data) {

  ifelse(data[[x]] %in% conditions, 1 * data[["highlight"]], 0)

}

# Build data
#' @importFrom dplyr left_join mutate select filter
#' @noRd
get_comeplete_data <- function(id, seats, votes) {

  swissparl::seating_plan %>%
    dplyr::left_join(seats %>% dplyr::select(-FirstName, -LastName, -ParlGroupName, -Language), by = "SeatNumber") %>%
    dplyr::left_join(votes %>% dplyr::filter(IdVote == id), by = "PersonNumber") %>%
    dplyr::mutate(Decision = ifelse(is.na(Decision), 8, Decision)) %>%
    dplyr::mutate(DecisionText = ifelse(Decision == 8, "Missing", DecisionText)) %>%
    dplyr::mutate(IdVote = id) %>%
    dplyr::mutate(highlight = 1)

}

