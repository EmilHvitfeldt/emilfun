#' Re-imagination of stats::step() function
#'
#' This functions aims to add the same functionality as stats::step() with the
#' adition to saving intermediate models along with diagnogists.
#'
#' @param object an object representing a model of an appropriate class
#'  (mainly "lm" and "glm"). This is used as the initial model in the
#'  stepwise search.
#' @param steps the maximum number of steps to be considered. The default is
#' 1000 (essentially as many as required). It is typically used to stop the
#' process early.
#' @return to be determined
#' @examples
#' dataset <- dplyr::mutate(iris,
#'          tan1 = rnorm(n()),
#'          tan2 = rnorm(n()),
#'          tan3 = rnorm(n()),
#'          tan4 = rnorm(n()),
#'          tan5 = rnorm(n()))
#'
#'   object <- lm(Sepal.Length ~ ., data = dataset)
#'
#'   X <- step_new(object)
#' @export
step_new <- function(object, steps = 1000) {

  res <- list(starting_model = object)
  models <- list()
  formulas <- character(steps)
  formulas[1] <- as.character(formula(object))[3]

  for(i in 1:steps) {
    AICS <- drop1(object)
    if(!any(AICS$AIC[1] > AICS$AIC)) break

    removeTerm <- rownames(AICS)[which.min(AICS$AIC)]
    object <- update(object, paste("~ . -", removeTerm))
    models[[i]] <- object
    formulas[i + 1] <- as.character(formula(object))[3]
    cat(crayon::blue(removeTerm), " has been ", crayon::red("removed"),
        ", resulting in the model:\n",
        formulas[i + 1], "\n",
        sep = "")

  }

  res[["models"]] <- models
  res[["formulas"]] <- formulas[1:i]

  res
}
