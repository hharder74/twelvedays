#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
#'
pluralize_gift <- function(gift) {
  if (str_detect(gift, "y$") == TRUE) {
    return(str_replace(gift, "y$", "ies"))
  } else if (str_detect(gift, "oose$")) {
    return(str_replace(gift, "oose$", "eese"))
  } else {
    return(str_glue("{gift}s"))
  }
}
