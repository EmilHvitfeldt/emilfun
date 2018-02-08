#' kmeans function that takes a data.frame and a formula as inputs
#'
#' This function is a simple wrapper that lets you work with \code{kmeans()}
#' while using data.frame, thus allowing you to stay with the same format.
#'
#' @param data A data.frame.
#' @param formula an object of class \code{"\link{formula}"} (or one that
#' can be coerced to that class)
#' @param \dots Additional arguments to be passed to the
#' \code{\link[stats]{kmeans}} function.
#' @return an object of class "kmeans".
#' @examples
#' kmeans_df(cars, ~ ., centers = 2)
#'
#' kmeans_df(iris, ~ Sepal.Length + Sepal.Width, centers = 3)
#' @export
kmeans_df <- function(data, formula, ...) {
  data_mat <- as.matrix(model.frame(formula, data))

  kmeans(x = data_mat, ...)
}
