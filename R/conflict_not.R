#' Indicates where a function is loaded from
#'
#' Call this function as an addin over selected text to add packagename::
#' before any function call.
#'
#' @export
conflict_not <- function() {
  context <- rstudioapi::getActiveDocumentContext()

  text <- unlist(strsplit(context$selection[[1]]$text, split = "\n"))

  get_pkg_name <- function(x) {
    out <- purrr::map_chr(x, ~ search()[match(getAnywhere(.x)$where,search())[1]]) %>%
      stringr::str_remove(".GlobalEnv") %>%
      stringr::str_remove("package:")
    out
  }

  fun <- utils::getParseData(parse(text = text)) %>%
    dplyr::filter_(~ token == "SYMBOL_FUNCTION_CALL") %>%
    dplyr::mutate_(package = ~ get_pkg_name(text),
                   text = ~ ifelse(package == "", text, stringr::str_glue("{package}::{text}")))

  for (i in rev(seq_len(nrow(fun)))) {
    stringr::str_sub(text[fun[i, ]$line1], fun[i, ]$col1, fun[i, ]$col2) <- fun[i, ]$text
  }

  rstudioapi::insertText(paste0(text, collapse = "\n"))
}
