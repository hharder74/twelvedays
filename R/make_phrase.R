#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#'
#' @export
make_phrase <- function(num, num_word, item, verb, adjective, location){
  if (num == 1) {
    verb <- str_replace_na(verb, "")
    adjective <- str_replace_na(adjective, "")
    location <- str_replace_na(location, "")
    phrase <- str_glue("a {adjective} {item} {verb} {location}")
    phrase <- str_squish(phrase)
    return(phrase)
  } else {
    item <- pluralize_gift(item)
    verb <- str_replace_na(verb, "")
    adjective <- str_replace_na(adjective, "")
    location <- str_replace_na(location, "")
    phrase <- str_glue("{num_word} {adjective} {item} {verb} {location}")
    phrase <- str_squish(phrase)
    return(phrase)
  }
}

