#' Scrapes color palette from color-hex.com
#'
#' @param url Url of website as string.
#' @return A vector of color values as hex color codes.
#' @examples
#' palette_color_hex("http://www.color-hex.com/color-palette/185")
#' @export
palette_color_hex <- function(url) {
  read_html(url) %>%
    html_nodes('table') %>%
    html_table() %>%
    .[[1]] %>%
    pull(Hex)
}

#' Scrapes color palette from coolors.co
#'
#' @param url Url of website as string.
#' @return A vector of color values as hex color codes.
#' @examples
#' palette_coolors("https://coolors.co/app/05668d-028090-00a896-02c39a-f0f3bd")
#' @export
palette_coolors <- function(url) {
  str_extract(url, "([^/]+$)") %>%
    str_split("-") %>%
    unlist() %>%
    str_c("#", .)
}


