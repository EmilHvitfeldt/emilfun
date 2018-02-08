#' Sequence generation based on range of input.
#'
#' Generates regular sequences from the minimum to the maximum of the input.
#'
#' The extend parameter can be either of length 1 or 2. If the length is 1 then
#' the number is subtracted from the minimum, and added to the maximum. Thus
#' extending the range by extend * 2. If the length is 2 then the first number
#' is subtracted from the minimum and the second number is added to the maximum.
#'
#' If procent is set to TRUE then extend is taken as a procentage where 1 is
#' equal to the range, i.e. the distance from the minimum to the maximum.
#'
#' @param x Vector whoes range is used to generate the sequence.
#' @param length.out desired length of the sequence. A non-negative number.
#' @param extend Numeric, number of which to extend the range by.
#' @param procent Indicates if extend is taken as a procentage.
#' @return returns a vector of type "integer" or "double".
#' @examples
#'  seq_range(1:5, 10)
#'
#'  seq_range(1:5, 10, 1)
#'
#'  seq_range(1:5, 10, 1:2)
#'
#'  seq_range(rnorm(100), 10)
#'
#'  seq_range(rnorm(100), 10, 0.1, TRUE)
#'
#'  seq_range(1:5, 10, extend = 1, procent = TRUE)
#' @export
seq_range <- function(x, length.out, extend = 0, procent = FALSE) {

  x_range <- range(x)

  if(procent) {
    extend <- extend * (x_range[2] - x_range[1])
  }

  if(length(extend) == 1) {
    x_range <- x_range + c(-extend, extend)
  }
  if(length(extend) == 2) {
    x_range <- x_range + c(-extend[1], extend[2])
  }

  seq(x_range[1], x_range[2], length.out = length.out)
}
