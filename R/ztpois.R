#' The Zero-truncated Poisson distribution
#'
#' Random generation for the Poisson distribution with parameter lambda.
#'
#' @param n number of random values to return.
#' @param lambda vector of (non-negative) means.
#' @return Generates random deviates of the zero truncated Poisson.
#' @examples
#' rztpois(100)
#' rztpois(1000, 5)
#' @export
rztpois <- function(n, lambda = 1) {
  res <- integer()
  n_sample <- n

  # n = 0
  pois_sample <- stats::rpois(n = n_sample, lambda = lambda)
  res <- c(res, pois_sample[pois_sample != 0])
  n_sample <- (n - length(res)) / mean(pois_sample != 0)

  if (is.infinite(n_sample)) {
    n_sample <- n
  }
  while(length(res) < n) {
    pois_sample <- stats::rpois(n = n_sample, lambda = lambda)
    res <- c(res, pois_sample[pois_sample != 0])
  }
  res[seq_len(n)]
}
