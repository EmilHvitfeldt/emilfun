#' Tidy output of the gvlma function
#'
#' This function is meant takes a lm object, or a data.frame of lm objects and
#' returns a data.frame of lm model assumption tests outlined in “Global
#' validation of linear model assumptions”.
#'
#' @param x lm object or data.frame with lm objects as a list-column.
#' @param model Column name of the
#' @return data.frame with 18 columns.
#' @examples
#' \dontrun{
#' data <- car::Prestige
#'
#' data %>%
#'   nest() %>%
#'   mutate(lm = map(data, ~ lm(education ~ income, .x))) %>%
#'   tidy_masm(lm)
#'
#'   lm(education ~ income, data) %>%
#'     tidy_masm()
#' }
#' @export
tidy_masm <- function(x, model) {
  if("lm" %in% class(x)) {
    x <- data.frame() %>%
      tidyr::nest() %>%
      dplyr::mutate(lm = purrr::map(1, ~ x),
                    assumptions = purrr::map(1, ~ gvlma::gvlma(x)))
  } else if("data.frame" %in% class(x)) {
    enquo_model <- dplyr::enquo(model)
    x <- dplyr::mutate(x, assumptions = purrr::map(!!enquo_model, ~ gvlma::gvlma(.x)))
  } else {
    stop("Input must be a lm object or data.frame with lm objects.")
  }

  gvlma_extract(x, var = assumptions)
}

gvlma_extract <- function(x, var) {
  enquo_var <- dplyr::enquo(var)

  dplyr::mutate(x,
         global_value = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$GlobalStat4$Value),
         global_p_value = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$GlobalStat4$pvalue),
         global_decision = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$GlobalStat4$Decision == 0),
         skewness_value = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat1$Value),
         skewness_p_value = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat1$pvalue),
         skewness_decision = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat1$Decision == 0),
         kurtosis_value = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat2$Value),
         kurtosis_p_value = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat2$pvalue),
         kurtosis_decision = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat2$Decision == 0),
         link_function_value = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat3$Value),
         link_function_p_value = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat3$pvalue),
         link_function_decision = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat3$Decision == 0),
         Heteroscedasticity_value = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat4$Value),
         Heteroscedasticity_p_value = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat4$pvalue),
         Heteroscedasticity_decision = purrr::map_dbl(!!enquo_var, ~ .x$GlobalTest$DirectionalStat4$Decision == 0))
}
