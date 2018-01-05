#' Extract first text between two patterns (not including)
#'
#' @param string A number. Input vector.
#' @param start Pattern to match after.
#' @param end Pattern to match before.
#' @return A character vector.
#' @export
#'
str_between <- function(string, start, end) {
  stringr::str_extract(string,
                       stringr::str_c(start, '(.*?)', end, collapse = '')) %>%
    stringr::str_replace(start, "") %>%
    stringr::str_replace(end, "")
}

#' Extract all text between two patterns (not including)
#'
#' @param string A number. Input vector.
#' @param start Pattern to match after.
#' @param end Pattern to match before.
#' @return A character vector.
#' @export
#'
str_between_all <- function(string, start, end) {
  stringr::str_extract_all(string,
                           stringr::str_c(start, '(.*?)', end, collapse = '')) %>%
    purrr::map(~ .x %>% str_between(start, end))
}

#' Extract nth text between two patterns (not including)
#'
#' @param string A number. Input vector.
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
#' @param string A number. Input vector.
#' @param pattern Pattern to match before.
#' @return A character vector.
#' @export
#'
str_before <- function(string, pattern) {
  stringr::str_extract(string, stringr::str_c(".+?(?=", pattern, ")"))
}

#' Extract text after pattern (not including)
#'
#' @param string A number. Input vector.
#' @param pattern Pattern to match after.
#' @return A character vector.
#' @export
#'
str_after <- function(string, pattern) {
  stringr::str_extract(string, stringr::str_c("(?<=", pattern, ").*$"))
}
