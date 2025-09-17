#' List cached models
#'
#' @return Character vector of cached model names
#' @export
list_cached_models <- function() {
  cache_dir <- tools::R_user_dir("matrixmodel", "cache")
  if (dir.exists(cache_dir)) {
    list.files(cache_dir, pattern = "\\.rds$")
  } else {
    character(0)
  }
}
