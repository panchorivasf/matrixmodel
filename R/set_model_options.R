#' Set package options for model loading
#'
#' @param remote_base_url Base URL for remote model repository
#' @param cache_dir Custom cache directory
#' @export
set_model_options <- function(remote_base_url = NULL, cache_dir = NULL) {
  if (!is.null(remote_base_url)) {
    options(matrixmodel.remote_base_url = remote_base_url)
  }

  if (!is.null(cache_dir)) {
    options(matrixmodel.cache_dir = cache_dir)
  }
}
