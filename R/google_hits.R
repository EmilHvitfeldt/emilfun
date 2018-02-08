#' Number of hits of results from google searches
#'
#' Provided with a (vector of) string(s) will return the number of results
#' approximately given by google.
#'
#' @param string vector of strings to search for.
#' @return A tibble with two columns. search and hits.
#' @examples
#' \dontrun{
#' google_hits(string = letters) %>%
#'   ggplot(aes(search, hits)) +
#'   geom_col()
#' }
#' @export
google_hits <- function(string) {
  base_url <- "https://www.google.com/search?q="
  url <- glue::glue("{base_url}{purrr::map_chr(letters, utils::URLencode)}")

  pb <- dplyr::progress_estimated(length(url))

  hits <- purrr::map_dbl(url, ~ {
    pb$tick()$print()
    xml2::read_html(.x) %>%
      rvest::html_node('div[id="resultStats"]') %>%
      rvest::html_text() %>%
      stringr::str_extract("(?<= )(.*)(?= )") %>%
      readr::parse_number()
  })

  tibble::tibble(search = string,
                 hits = hits)
}
