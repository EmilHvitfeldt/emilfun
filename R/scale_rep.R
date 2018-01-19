#' Create your own discrete scale
#'
#' This allows you to specify you own set of mappings from levels in the
#' data to aesthetic values that will repeat if a insufficient number of levels
#'  is provided.
#'
#'
#' @param ... Other arguments passed on to [discrete_scale()] to control name, limits,
#'   breaks, labels and so forth.
#' @param values a set of aesthetic values to map data values to. If this
#'   is a named vector, then the values will be matched based on the names.
#'   If unnamed, values will be matched in order (usually alphabetical) with
#'   the limits of the scale. Will repeat if number of labels is less.
#'
#' @examples
#' library(ggplot2)
#'
#' dat <- data.frame(x = sample(LETTERS, 200, TRUE))
#'
#' ggplot(dat, aes(x = x)) +
#'   geom_bar(aes(fill = x)) +
#'     scale_fill_rep(values = rainbow(5))
#' @name scale_rep
#' @aliases NULL
NULL

#' @rdname scale_rep
#' @export
scale_fill_rep <- function (..., values) {
  pal <- function(n) {
    rep(values, ceiling(n / length(values)))[1:n]
  }
  ggplot2::discrete_scale(aesthetic = "fill", "manual", pal, ...)
}

#' @rdname scale_rep
#' @export
scale_alpha_rep <- function (..., values) {
  pal <- function(n) {
    rep(values, ceiling(n / length(values)))[1:n]
  }
  ggplot2::discrete_scale(aesthetic = "alpha", "manual", pal, ...)
}

#' @rdname scale_rep
#' @export
scale_color_rep <- function (..., values) {
  pal <- function(n) {
    rep(values, ceiling(n / length(values)))[1:n]
  }
  ggplot2::discrete_scale(aesthetic = "colour", "manual", pal, ...)
}

#' @rdname scale_rep
#' @export
scale_colour_rep <- function (..., values) {
  pal <- function(n) {
    rep(values, ceiling(n / length(values)))[1:n]
  }
  ggplot2::discrete_scale(aesthetic = "colour", "manual", pal, ...)
}

#' @rdname scale_rep
#' @export
scale_linetype_rep <- function (..., values) {
  pal <- function(n) {
    rep(values, ceiling(n / length(values)))[1:n]
  }
  ggplot2::discrete_scale(aesthetic = "linetype", "manual", pal, ...)
}

#' @rdname scale_rep
#' @export
scale_shape_rep <- function (..., values) {
  pal <- function(n) {
    rep(values, ceiling(n / length(values)))[1:n]
  }
  ggplot2::discrete_scale(aesthetic = "shape", "manual", pal, ...)
}

#' @rdname scale_rep
#' @export
scale_size_rep <- function (..., values) {
  pal <- function(n) {
    rep(values, ceiling(n / length(values)))[1:n]
  }
  ggplot2::discrete_scale(aesthetic = "size", "manual", pal, ...)
}
