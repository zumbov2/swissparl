#' Plot voting results
#'
#' \code{ggswissparl} plots voting results of the Swiss National Council according to the latest seating order.
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr select distinct arrange bind_rows left_join group_by slice ungroup filter count
#' @importFrom tibble tibble
#' @importFrom ggplot2 theme_set theme_void theme_update element_rect element_text ggplot geom_polygon aes geom_point
#'     scale_color_manual scale_fill_manual scale_alpha_manual coord_equal annotate facet_wrap
#'
#' @param votes data of votes of the Swiss National Council as can be retrieved with \code{get_data(table = "Voting")}.
#'     The variables \code{PersonNumber}, \code{Decision}, and \code{DecisionText} must be available from the data.
#' @param seats data linking councillors (\code{PersonNumber}) to seats (\code{SeatNumber}). If \code{is.null},
#'     the most current seating order is retrieved via \code{get_data(table = "SeatOrganisationNr")}.
#' @param highlight named list with variable and values to specify highlighting of selected councillors.
#' @param result if \code{TRUE}, the result is annontated.
#' @param result_size font size of result.
#' @param point_shape shape of point as defined in \code{[ggplot2]{geom_point}}.
#' @param point_size size of point.
#' @param theme name of predefined plot theme:
#' \itemize{
#'     \item \code{"scoreboard"} imitates the scoreboard in the council hall: neon-red (yes-votes),
#'     neon-green (no-votes) and white (abstentions) dots on black ground in white frames.
#'     \item \code{"sym1"} colored symbols on light background in black frames.
#'     \item \code{"sym2"} colored symbols on light background without frames.
#'     \item \code{"poly1"} color-filled polygons with black edges.
#'     \item \code{"poly2"} color-filled polygons with white edges.
#'     \item \code{"poly3"} color-filled polygons without edges.
#'     }
#'
#' @return A ggplot object. If \code{votes} contains multiple ballots, \code{[ggplot2]{facet_wrap}} is used to
#'     create facets.
#' @export
#'
#' @examples
#' \donttest{
#' # Visualization of a vote of the 51st legislature
#' get_data("Voting", Language = "DE", IdVote = 23458) %>%
#'     ggswissparl()
#'
#' # Highlighting a parliamentary group
#' get_data("Voting", Language = "DE", IdVote = 23458) %>%
#'     ggswissparl(highlight = list("ParlGroupNumber" = 2))
#'}
ggswissparl <- function(votes, seats = NULL, highlight, result = F, result_size = 6,
                        point_shape = 16, point_size = 4, theme = "scoreboard") {

  # Check for seats
  if (is.null(seats)) seats <- swissparl::get_data(table = "SeatOrganisationNr", silent = T, Language = "DE")

  # Check inputs for relevant variables
  check_vars(c("SeatNumber", "PersonNumber"), seats)
  check_vars(c("PersonNumber", "Decision", "DecisionText"), votes)

  # Join data
  dt <- purrr::map_dfr(unique(votes$IdVote), get_comeplete_data, seats, votes)

  # Highligting
  if (!missing(highlight)) {

    # Check for availability of highlighting variable
    check_vars(names(highlight), dt)

    # Update highlight
    dt$highlight <- ifelse(dt[[names(highlight)[1]]] %in% highlight[[names(highlight)[1]]], 0, 1)

  }

  # Decisions
  decision <- dt %>%
    dplyr::select(Decision, DecisionText) %>%
    dplyr::distinct() %>%
    dplyr::arrange(Decision)

  # Complete to all 8 options
  dec_diff <- setdiff(c(1:8), decision$Decision)
  if (length(dec_diff) > 0) {

    decision <- dplyr::bind_rows(
      decision,
      tibble::tibble(
        Decision = dec_diff,
        DecisionText = ""
      )
    ) %>%
      dplyr::arrange(Decision)
  }

  # Define Themes
  if (theme == "scoreboard") {

    # Define decision colors
    colors <- tibble::tibble(
      Decision = c(1:8),
      color = c("green", "red", "white", "black", "black", "black", "black", "black")
      )

    # Define seat colors
    col_f <- "black"
    col_c <- "white"

    # Orientation
    dt$x <- -dt$x
    dt$center_x <- -dt$center_x

    # Theme
    suppressWarnings(ggplot2::theme_set(ggplot2::theme_void()))
    suppressWarnings(
      ggplot2::theme_update(
        legend.position = "none",
        plot.background = ggplot2::element_rect(fill = "black"),
        strip.text = ggplot2::element_text(colour = 'white')
        )
    )

    # Result position
    r_x = -2975
    r_y = c(5500, 5200, 4900)

  }
  if (theme == "sym1") {

    # Define decision colors
    colors <- tibble::tibble(
      Decision = c(1:8),
      color = c("#009E73", "#D55E00", "grey50", "white", "white", "white", "white", "white")
    )

    # Define seat colors
    col_f <- "white"
    col_c <- "black"

    # Orientation
    dt$y <- -dt$y
    dt$center_y <- -dt$center_y

    # Theme
    suppressWarnings(ggplot2::theme_set(ggplot2::theme_void()))
    suppressWarnings(
      ggplot2::theme_update(
        legend.position = "none"
      )
    )

    # Result position
    r_x = 896
    r_y = rev(c(-5500, -5200, -4900))

  }
  if (theme == "sym2") {

    # Define decision colors
    colors <- tibble::tibble(
      Decision = c(1:8),
      color = c("#009E73", "#D55E00", "grey50", "white", "white", "white", "white", "white")
    )

    # Define seat colors
    col_f <- "white"
    col_c <- "white"

    # Orientation
    dt$y <- -dt$y
    dt$center_y <- -dt$center_y

    # Theme
    suppressWarnings(ggplot2::theme_set(ggplot2::theme_void()))
    suppressWarnings(
      ggplot2::theme_update(
        legend.position = "none"
      )
    )

    # Result position
    r_x = 896
    r_y = rev(c(-5500, -5200, -4900))

  }
  if (theme == "poly1") {

    # Define decision colors
    colors <- tibble::tibble(
      Decision = c(1:8),
      color = c("#009E73", "#D55E00", "grey50", "grey95", "grey95", "grey95", "grey95", "grey95")
    )

    # Define seat frame
    col_s <- "black"

    # Orientation
    dt$y <- -dt$y
    dt$center_y <- -dt$center_y

    # Theme
    suppressWarnings(ggplot2::theme_set(ggplot2::theme_void()))
    suppressWarnings(
      ggplot2::theme_update(
        legend.position = "none"
      )
    )

    # Result position
    r_x = 896
    r_y = rev(c(-5500, -5200, -4900))

  }
  if (theme == "poly2") {

    # Define decision colors
    colors <- tibble::tibble(
      Decision = c(1:8),
      color = c("#009E73", "#D55E00", "grey50", "grey95", "grey95", "grey95", "grey95", "grey95")
    )

    # Define seat frame
    col_s <- "white"

    # Orientation
    dt$y <- -dt$y
    dt$center_y <- -dt$center_y

    # Theme
    suppressWarnings(ggplot2::theme_set(ggplot2::theme_void()))
    suppressWarnings(
      ggplot2::theme_update(
        legend.position = "none"
      )
    )

    # Result position
    r_x = 896
    r_y = rev(c(-5500, -5200, -4900))

  }
  if (theme == "poly3") {

    # Define decision colors
    colors <- tibble::tibble(
      Decision = c(1:8),
      color = c("#009E73", "#D55E00", "grey50", "grey95", "grey95", "grey95", "grey95", "grey95")
    )

    # Define seat frame
    col_s <- NA

    # Orientation
    dt$y <- -dt$y
    dt$center_y <- -dt$center_y

    # Theme
    suppressWarnings(ggplot2::theme_set(ggplot2::theme_void()))
    suppressWarnings(
      ggplot2::theme_update(
        legend.position = "none"
      )
    )

    # Result position
    r_x = 896
    r_y = rev(c(-5500, -5200, -4900))

  }

  # Join colors
  decision <- dplyr::left_join(decision, colors, by = "Decision")

  # Decision Variable
  dt$Decision_f <- factor(dt$Decision, levels = decision$Decision, labels = decision$DecisionText)

  # Base plot
  if (theme %in% c("scoreboard", "sym1", "sym2")) {

    p <- dt %>%
      ggplot2::ggplot() +
      ggplot2::geom_polygon(
        ggplot2::aes(
          x = x,
          y = y,
          group = SeatNumber
        ),
        fill = NA,
        color = col_c
      ) +
      ggplot2::geom_point(
        ggplot2::aes(
          x = center_x,
          y = center_y,
          color = Decision_f,
          alpha = as.factor(highlight)
        ),
        shape = point_shape,
        size = point_size
      ) +
      ggplot2::scale_color_manual(values = decision$color) +
      ggplot2::scale_alpha_manual(values = c(1, 0.1)) +
      ggplot2::coord_equal()

  }
  if (theme %in% c("poly1", "poly2", "poly3")) {

    p <- dt %>%
      ggplot2::ggplot() +
      ggplot2::geom_polygon(
        ggplot2::aes(
          x = x,
          y = y,
          group = SeatNumber,
          fill = Decision_f,
          alpha = as.factor(highlight)
          ),
        color = col_s
        ) +
      ggplot2::scale_fill_manual(values = decision$color) +
      ggplot2::scale_alpha_manual(values = c(1, 0.1)) +
      ggplot2::coord_equal()

  }

  # Add result
  if (result) {

    # Total result
    tot.res <- dt %>%
      dplyr::group_by(PersonNumber) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Decision %in% c(1:3)) %>%
      dplyr::group_by(Decision, DecisionText) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      dplyr::left_join(colors, by = "Decision")

    # Partial result
    part.res <- dt %>%
      dplyr::group_by(PersonNumber) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Decision %in% c(1:3)) %>%
      dplyr::filter(highlight == 0) %>%
      dplyr::group_by(Decision, DecisionText) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      dplyr::left_join(colors, by = "Decision")

    # Yes
    if (1 %in% tot.res$Decision) {

      res1 <- paste0(
        paste0(rep("0", 3 - nchar(tot.res$n[1])), collapse = ""),
        tot.res$n[1], " ",
        tot.res$DecisionText[1]
      )

      if (!missing(highlight)) {

        dt_rt <- part.res[part.res$DecisionText == tot.res$DecisionText[1],]
        if (nrow(dt_rt) == 0) dt_rt <- tibble::tibble(Decision = "", DecisionText = "", n = 0, color = NA)

        res1 <- paste0(
          paste0(rep("0", 3 - nchar(dt_rt$n[1])), collapse = ""),
          dt_rt$n[1], " / ", res1
        )

      }

      p <- p +
        ggplot2::annotate(
          geom = "text",
          x = r_x,
          y = r_y[1],
          hjust = 0,
          label =res1,
          color = tot.res$color[1],
          fontface = "bold",
          size = result_size
          )

    }

    # No
    if (2 %in% tot.res$Decision) {

      res2 <- paste0(
        paste0(rep("0", 3 - nchar(tot.res$n[2])), collapse = ""),
        tot.res$n[2], " ",
        tot.res$DecisionText[2]
      )

      if (!missing(highlight)) {

        dt_rt <- part.res[part.res$DecisionText == tot.res$DecisionText[2],]
        if (nrow(dt_rt) == 0) dt_rt <- tibble::tibble(Decision = "", DecisionText = "", n = 0, color = NA)

        res2 <- paste0(
          paste0(rep("0", 3 - nchar(dt_rt$n[1])), collapse = ""),
          dt_rt$n[1], " / ", res2
        )

      }

      p <- p +
        ggplot2::annotate(
          geom = "text",
          x = r_x,
          y = r_y[2],
          hjust = 0,
          label =res2,
          color = tot.res$color[2],
          fontface = "bold",
          size = result_size
        )

    }

    # Abstention
    if (3 %in% tot.res$Decision) {

      res3 <- paste0(
        paste0(rep("0", 3 - nchar(tot.res$n[3])), collapse = ""),
        tot.res$n[3], " ",
        tot.res$DecisionText[3]
      )

      if (!missing(highlight)) {

        dt_rt <- part.res[part.res$DecisionText == tot.res$DecisionText[3],]
        if (nrow(dt_rt) == 0) dt_rt <- tibble::tibble(Decision = "", DecisionText = "", n = 0, color = NA)

        res3 <- paste0(
          paste0(rep("0", 3 - nchar(dt_rt$n[1])), collapse = ""),
          dt_rt$n[1], " / ", res3
        )

      }

      p <- p +
        ggplot2::annotate(
          geom = "text",
          x = r_x,
          y = r_y[3],
          hjust = 0,
          label =res3,
          color = tot.res$color[3],
          fontface = "bold",
          size = result_size
        )

    }

  }

  # Facetting
  num_votes <- length(unique(dt$IdVote[!is.na(dt$IdVote)]))
  if (num_votes > 1) {

    p <- p +
      ggplot2::facet_wrap(.~IdVote)

  }

  # Return
  return(p)

}


