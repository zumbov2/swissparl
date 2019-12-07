#' \code{swissparl} package
#'
#' The Swiss Parliament Webservices R API
#'
#' See the README on
#' \href{https://github.com/zumbov2/swissparl#readme}{GitHub}
#'
#' @docType package
#' @name swissparl
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") {

  utils::globalVariables(
    c(".", "variable", "EntitySets", "filter_name", "value", "batch", "len",
      "Decision", "DecisionText", "Decision_f", "FirstName", "IdVote", "Language",
      "LastName", "ParlGroupName", "PersonNumber", "SeatNumber", "center_x",
      "center_y", "x", "y")
  )
}
