#' Returns mode of a variable
#'
#' This function takes a vector and returns the statistical mode, i.e. the most
#'  common value the vector takes.
#'
#' @param x vector.
#' @return Most common value.
#' @source \url{https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode}
#' @examples
#' stat_mode(ggplot2::mpg$manufacturer)
#' stat_mode(ggplot2::mpg$model)
#'
#' stat_mode(rpois(100, 1))
#' @export
stat_mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
