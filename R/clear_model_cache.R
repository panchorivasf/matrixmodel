#' Clear model cache
#'
#' @param model_name Specific model to remove from cache, or NULL to clear all
#' @export
clear_model_cache <- function(model_name = NULL) {
  cache_dir <- tools::R_user_dir("matrixmodel", "cache")

  if (is.null(model_name)) {
    unlink(cache_dir, recursive = TRUE)
    message("All cached models removed")
  } else {
    file_path <- file.path(cache_dir, model_name)
    if (file.exists(file_path)) {
      unlink(file_path)
      message("Removed from cache: ", model_name)
    } else {
      message("Model not found in cache: ", model_name)
    }
  }
}

