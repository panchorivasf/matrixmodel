#' Check model cache status
#' @export
check_cache_status <- function() {
  cache_dir <- getOption("matrixmodel.cache_dir")

  cat("Cache directory:", cache_dir, "\n")
  cat("Directory exists:", dir.exists(cache_dir), "\n")

  if (dir.exists(cache_dir)) {
    files <- list.files(cache_dir, pattern = "\\.rds$")
    cat("Cached files:", if (length(files) > 0) paste(files, collapse = ", ") else "None", "\n")

    for (file in files) {
      file_path <- file.path(cache_dir, file)
      cat("  ", file, ": ", round(file.size(file_path)/1024/1024, 2), "MB\n", sep = "")
    }
  }

  cat("Models in memory:", paste(ls(envir = .matrixmodel_models), collapse = ", "), "\n")
}
