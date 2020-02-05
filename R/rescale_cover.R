#' Rescales cover images for my blog
#'
#' @return nothing
#' @export
cover_image_rescale <- function() {
  covers <- fs::dir_ls(here::here("static/img/blog/"), recurse = TRUE, regexp = "cover")
  rescale_280 <- function(x) {
    magick::image_read(x) %>%
      magick::image_scale("280") %>%
      magick::image_write(x)
  }
  purrr::walk(covers, rescale_280)
}
