#' Extract first text between two patterns (not including)
#'
#' Extracts the whatever happens after "start", and the first occurence of
#' "end" after that. White space can be removed using the rm.ws argument.
#'
#' @param string A character vector. Input vector.
#' @param start Pattern to match after.
#' @param end Pattern to match before.
#' @param rm.ws Removed leading and trailing white space if TRUE.
#' @return A character vector.
#' @examples
#' string <- "This is a test string where we do the testing."
#'
#' str_between(string, "where", "the")
#'
#' str_between(string, "where", "the", TRUE)
#' str_between(string, "where", "the", rm.ws = TRUE)
#' @export
#'
str_between <- function(string, start, end, rm.ws = FALSE) {
  out <- stringr::str_extract(string,
                       stringr::str_c(start, '(.*?)', end, collapse = '')) %>%
    stringr::str_replace(start, "") %>%
    stringr::str_replace(end, "")

  if(rm_ws) {
    out <- trimws(out)
  }
  out
}

#' Extract nth text between two patterns (not including)
#'
#' @param string A character vector. Input vector.
#' @param start Pattern to match after.
#' @param end Pattern to match before.
#' @param n Integer.
#' @return A character vector.
#' @export
#'
str_between_n <- function (string, start, end, n) {
  stringr::str_extract_all(string,
                           stringr::str_c(start, "(.*?)", end, collapse = "")) %>%
    purrr::map(~.x %>% str_between(start, end) %>% .[n]) %>%
    unlist()
}

#' Extract text before pattern (not including)
#'
#' @param string A character vector. Input vector.
#' @param pattern Pattern to match before.
#' @return A character vector.
#' @examples
#' string <- "This is a test string where we do the testing."
#'
#' str_before(string, "where")
#' str_before(string, "test")
#' @export
#'
str_before <- function(string, pattern) {
  stringr::str_extract(string, stringr::str_c(".+?(?=", pattern, ")"))
}

#' Extract text after pattern (not including)
#'
#' @param string A character vector. Input vector.
#' @param pattern Pattern to match after.
#' @return A character vector.
#' @examples
#' string <- "This is a test string where we do the testing."
#'
#' str_after(string, "where")
#' str_after(string, "test")
#' @export
#'
str_after <- function(string, pattern) {
  stringr::str_extract(string, stringr::str_c("(?<=", pattern, ").*$"))
}

#' Extract the nth word
#'
#' Fairly rough function. Ideal to work with n-grams.
#'
#' The parameter n must be of length 1 for function to work. For multiple words
#' please run function multiple times.
#'
#' @param string A character vector. Input vector.
#' @param n Integer. Which word to extract.
#' @param sep Pattern to seperate words with.
#' @return A character vector.
#' @examples
#' string <- "This is a test string where we do the testing."
#'
#' str_nth_word(string, 4)
#' str_nth_word(string, 6)
#' @export
#'
str_nth_word <- function(string, n, sep = " ") {
  stringr::str_split(string, pattern = " ") %>%
    purrr::map_chr(~ .x[n])
}

#' Extract after last
#'
#' @param string A character vector. Input vector.
#' @param pattern Pattern to look after.
#' @return A character vector.
#' @examples
#' string <- "This is a test string where we do the testing."
#'
#' str_after_last(string, "where")
#' str_after_last(string, "test")
#' str_after_last(string, " ")
#' @export
#'
str_after_last <- function(string, pattern) {
  stringr::str_extract(string,
                       stringr::str_c("([^", pattern, "]+$)", collapse = ""))
}

#' Before first
#'
#' @param string A character vector. Input vector.
#' @param pattern Pattern to look after.
#' @return A character vector.
#' @examples
#' string <- "This is a test string where we do the testing."
#'
#' str_before_first(string, "where")
#' str_before_first(string, "test")
#' @export
#'
str_before_first <- function(string, pattern) {
  stringr::str_extract(string,
                       stringr::str_c("^(.+?)", pattern, collapse = ""))
}



