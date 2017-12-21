#' Transform a catagorical variable to one hot representation
#'
#' @param data a data.frame or tibble.
#' @param var name of catagorical variable.
#' @return A data.frame or tibble with the catagorical variable replaced with
#'  an one hot representation.
#' @examples
#' one_hot(iris, Species)
#' @export
one_hot <- function(data, var) {

  var_enquo <- dplyr::enquo(var)
  items <- data %>% dplyr::pull(!!var_enquo)
  items_unique <- items %>% unique()

  out <- matrix(0, NROW(data), length(items_unique))
  colnames(out) <- items_unique

  for (i in items_unique) {
    out[, i] <- items == i
  }

  data %>%
    select(-!!var_enquo) %>%
    bind_cols(tibble::as.tibble(out))
}
