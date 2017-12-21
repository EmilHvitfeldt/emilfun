#' Insert latex aligned structure.
#'
#' Call this function as an addin to insert aligned structure at the cursor position.
#'
#' @export
aligned <- function() {
  rstudioapi::insertText("$$\n\\begin{aligned}\n\n\\end{aligned}\n$$")
  row <- as.numeric(
    rstudioapi::getSourceEditorContext()$selection[][[1]]$range$start[1]
  )
  rstudioapi::setCursorPosition(position = c(row - 2, 1))
}
