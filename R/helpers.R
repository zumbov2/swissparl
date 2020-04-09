# Clean texts
#' @importFrom stringr str_replace_all str_squish
#' @importFrom magrittr "%>%"
#' @noRd
clean_text_wh <- function(text, keep_round_brackets) {

  if (is.na(text)) return(NA)

  # Clean 1
  text <- text %>%
    stringr::str_replace_all("<.*?>", "  ") %>%
    stringr::str_replace_all("\\[.*?\\]", "  ") %>%
    stringr::str_replace_all("\n", "  ") %>%
    stringr::str_replace_all("\"", "'")

  # Clean 2
  if (!keep_round_brackets) text <- stringr::str_replace_all(text, "\\(.*?\\)", "  ")

  # Clean 3
  text <- stringr::str_squish(text)

  # Return
  return(text)

}

#' Clean texts retrieved from WebServices
#'
#' \code{clean_text} removes HTML code, brackets and their contents as well as line breaks from texts.
#'
#' @importFrom purrr map_chr
#'
#' @param text a character vector.
#' @param keep_round_brackets if \code{TRUE}, round brackets and their contents are not deleted.
#'
#' @return A character vector of same length as \code{text}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get clean version of transcripts
#' get_glimpse(table = "Transcript", rows = 1000, Language = "DE") %>%
#'    mutate(Text2 = clean_text(Text))
#' }
clean_text <- function(text, keep_round_brackets = T) {

  purrr::map_chr(text, clean_text_wh, keep_round_brackets = keep_round_brackets)

  }
