#' Load model from PURR repository with caching
#'
#' @param model_name Name of the model file ("mortality", "upgrowth", or "recruitment")
#' @param force_download Whether to force re-download even if cached
#'
#' @return The loaded model
#' @export
#'
#' @examples
#' \dontrun{
#' mortality_model <- load_model("mortality")
#' }
load_model <- function(model_name, force_download = FALSE) {
  # Validate model name
  valid_models <- c("mortality", "upgrowth", "recruitment")
  if (!model_name %in% valid_models) {
    stop("Invalid model name. Choose from: ", paste(valid_models, collapse = ", "))
  }

  # Set up paths
  purr_base_url <- "https://purr.purdue.edu/projects/indianamatrixm/files"
  file_name <- paste0(model_name, ".rds")
  file_url <- paste0(purr_base_url, "/", file_name, "/download")

  # Use package-specific cache directory
  cache_dir <- tools::R_user_dir("matrixmodel", "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  cache_path <- file.path(cache_dir, file_name)

  # Check if model exists in cache and not forcing download
  if (file.exists(cache_path) && !force_download) {
    message("Loading cached model: ", model_name)
    return(readRDS(cache_path))
  }

  # Download from PURR
  message("Downloading model from PURR: ", model_name)

  # Create temporary file
  temp_file <- tempfile(fileext = ".rds")

  # Attempt download
  tryCatch({
    download.file(
      url = file_url,
      destfile = temp_file,
      mode = "wb",
      quiet = FALSE
    )

    # Verify download was successful
    if (file.exists(temp_file) && file.size(temp_file) > 0) {
      # Copy to cache
      file.copy(temp_file, cache_path, overwrite = TRUE)
      message("Model saved to cache: ", cache_path)

      # Load and return the model
      model <- readRDS(temp_file)
      return(model)
    } else {
      stop("Download failed or resulted in empty file")
    }
  }, error = function(e) {
    # If download fails, check if we have a cached version
    if (file.exists(cache_path)) {
      warning("Download failed, using cached version: ", e$message)
      return(readRDS(cache_path))
    } else {
      stop("Download failed and no cached version available: ", e$message)
    }
  })
}
