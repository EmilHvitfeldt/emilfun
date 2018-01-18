#' Easy generation of data with dependent and independent variables
#'
#' This function is meant ease testing of other functions by creating a
#' dataframe with a variable number of correlated variables and independent
#' variables.
#'
#' @param n_dependent Integer. Number of variables that are correlated with
#' the response.
#' @param n_random Integer. Number of variables that isn't correlated with
#' the response.
#' @param n Integer. Number of observations.
#' @return data.frame with 1 + n_dependent + n_random number of variables.
#' @examples
#' data_make()
#'
#' data_make(2, 5, 1000)
#' @export
data_make <- function(n_dependent = 1, n_random = 1, n = 100) {
  response <- data.frame(response = runif(n = n, min = 0, 10))

  dependent <- matrix(response[, 1], n, n_dependent) + rnorm(n * n_dependent)

  colnames(dependent) <- paste0("var", seq_len(n_dependent))
  dependent <- as.data.frame(dependent)

  random <- matrix(rnorm(n * n_random), n, n_random)

  colnames(random) <- paste0("ran", seq_len(n_random))
  random <- as.data.frame(random)

  cbind(response, dependent, random)
}
