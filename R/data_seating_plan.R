#' Seating plan of the National Council
#'
#' A dataset containing the relative locations of the seats in the Swiss National Council
#'     to display schematic seating plans. A seat is defined by 4 corner points.
#'
#' @format A data frame with 800 rows and 5 variables:
#' \describe{
#'   \item{SeatNumber}{seat identifier.}
#'   \item{order}{corner identifier.}
#'   \item{x}{position of a corner point on the x-axis.}
#'   \item{y}{position of a corner point on the y-axis.}
#'   \item{center_x}{position of the seat center on the x-axis.}
#'   \item{center_y}{position of the seat center on the y-axis.}
#' }
#' @source \url{https://www.parlament.ch/en/organe/national-council/groups-chamber-nc}
"seating_plan"
