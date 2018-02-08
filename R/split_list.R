#' Split a vector into a list
#'
#' @param x A vector.
#' @param n a integer. Number of vectors.
#' @param max a integer. Maximum number of element in each vector.
#' @return A named list.
#' @examples
#' split_list(1:100, 2)
#'
#' split_list(1:100, 11)
#'
#' split_list(1:100, max = 11)
#' @export
#'
split_list <- function(x, n = NULL, max = NULL) {
  if(!is.null(max)) {
    split_by <- ceiling(seq_along(x) / max)
  }
  if(!is.null(n)) {
    split_by <- sort(rep(1:n, length.out = NROW(x)))
  }
  split(x, split_by)
}
