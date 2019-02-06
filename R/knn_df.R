#' knns function from class that takes a data.frame and a formula as inputs
#' knns function from class that takes a data.frame and a formula as inputs
#'
#' This function is a simple wrapper that lets you work with \code{class::knns()}
#' while using data.frame, thus allowing you to stay with the same format.
#'
#' It allows for the use of the formula expression from base stats, where the
#' lhs is the factor of the true. The split variable must be logical.
#'
#' @param data A data.frame.
#' @param formula an object of class \code{"\link{formula}"} (or one that
#' can be coerced to that class)
#' @param split variable that was used to split data.frame into training and
#' test set.
#' @param \dots Additional arguments to be passed to the
#' \code{\link[class]{knn}} function.
#' @return an object of class "knn".
#' @examples
#' data <- iris
#' data$split <- as.logical(sample(0:1, nrow(data), TRUE))
#'
#' knn_df(data, Species ~ Sepal.Length + Sepal.Width, split, k = 5, prob = TRUE)
#' @export
knn_df <- function(data, formula, split, ...) {
  quo_split <- dplyr::enquo(split)

  formula_obj <- as.character(formula)
  train <- model.frame(formula(paste("~", formula_obj[3])),
                       dplyr::filter(data, !!quo_split))
  test <- model.frame(formula(paste("~", formula_obj[3])),
                      dplyr::filter(data, !(!!quo_split)))
  cl <- data %>%
    dplyr::filter(!!quo_split) %>%
    dplyr::pull(formula_obj[2])

  class::knn(train = train, test = test, cl = cl, ...)
}
