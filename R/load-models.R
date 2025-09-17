# R/load-models.R

# Global environment for models
.matrixmodel_models <- new.env()

# Direct download URLs (permanent since Zenodo doesn't allow changes)
MODEL_URLS <- list(
  mortality = "https://zenodo.org/records/17138782/files/model_indiana_mortality.rds?download=1",
  upgrowth = "https://zenodo.org/records/17138782/files/model_indiana_upgrowth.rds?download=1",
  recruitment = "https://zenodo.org/records/17138782/files/model_indiana_recruitment.rds?download=1"
)

# File names for caching
MODEL_FILES <- list(
  mortality = "model_indiana_mortality.rds",
  upgrowth = "model_indiana_upgrowth.rds",
  recruitment = "model_indiana_recruitment.rds"
)

# Internal function to load models from Zenodo
.load_models <- function() {
  if (models_loaded()) return(TRUE)

  message("Downloading models from Zenodo...")

  # Create cache directory
  cache_dir <- getOption("matrixmodel.cache_dir")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Download each model file
  for (model_type in names(MODEL_URLS)) {
    file_name <- MODEL_FILES[[model_type]]
    cache_path <- file.path(cache_dir, file_name)

    message("Downloading ", file_name, "...")

    # Use robust download with retries
    success <- download_with_retry(
      url = MODEL_URLS[[model_type]],
      dest_path = cache_path,
      max_retries = 3,
      timeout = 600  # 10 minutes for the 140MB file
    )

    if (success) {
      # Load into memory
      .matrixmodel_models[[model_type]] <- readRDS(cache_path)
      message("  ??? ", model_type, " model loaded successfully")
    } else {
      warning("Failed to download: ", file_name)
    }
  }

  return(models_loaded())
}

# Robust download function with retries and progress
download_with_retry <- function(url, dest_path, max_retries = 3, timeout = 600) {
  for (attempt in 1:max_retries) {
    tryCatch({
      if (attempt > 1) {
        message("  Retry attempt ", attempt, " of ", max_retries)
        # Wait before retry (exponential backoff)
        Sys.sleep(2 ^ (attempt - 1))
      }

      # Use base download.file with increased timeout
      options(timeout = timeout)
      download.file(url, dest_path, mode = "wb", quiet = FALSE)

      if (file.exists(dest_path) && file.size(dest_path) > 0) {
        message("  ??? Download completed (", round(file.size(dest_path)/1024/1024, 1), " MB)")
        return(TRUE)
      }
    }, error = function(e) {
      message("  Download attempt ", attempt, " failed: ", e$message)
      # Clean up failed download
      if (file.exists(dest_path)) unlink(dest_path)
    })
  }

  return(FALSE)
}

# Load models from cache only
.load_models_from_cache <- function() {
  cache_dir <- getOption("matrixmodel.cache_dir")

  for (model_type in names(MODEL_FILES)) {
    file_name <- MODEL_FILES[[model_type]]
    cache_path <- file.path(cache_dir, file_name)

    if (file.exists(cache_path)) {
      tryCatch({
        .matrixmodel_models[[model_type]] <- readRDS(cache_path)
        message("Loaded from cache: ", model_type)
      }, error = function(e) {
        warning("Failed to load from cache: ", model_type, " - ", e$message)
        # Remove corrupt cache file
        unlink(cache_path)
      })
    }
  }

  return(models_loaded())
}

# Try direct download first, then cache
.load_models_with_fallback <- function() {
  # Try direct download first
  success <- .load_models()

  if (!success) {
    message("Direct download failed, trying cache...")
    success <- .load_models_from_cache()
  }

  return(success)
}

# Check if models are loaded
models_loaded <- function() {
  required_models <- names(MODEL_URLS)
  all(required_models %in% ls(envir = .matrixmodel_models))
}

# Get a loaded model
#' @export
get_model <- function(model_name = c("mortality", "upgrowth", "recruitment")) {
  model_name <- match.arg(model_name)

  if (!models_loaded()) {
    stop("Models not loaded. Use load_all_models() first.")
  }

  if (!exists(model_name, envir = .matrixmodel_models)) {
    stop("Model '", model_name, "' not found in loaded models.")
  }

  return(get(model_name, envir = .matrixmodel_models))
}

#' Load all models from Zenodo
#'
#' @param force_download Whether to force re-download even if cached
#' @export
load_all_models <- function(force_download = FALSE) {
  if (force_download) {
    clear_model_cache()
  }

  success <- .load_models_with_fallback()

  if (success) {
    message("??? All models loaded successfully!")
  } else {
    message("??? Failed to load models. Use model_install_instructions() for help.")
  }

  return(success)
}

#' Clear model cache
#' @export
clear_model_cache <- function() {
  cache_dir <- getOption("matrixmodel.cache_dir")
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    message("Model cache cleared.")
  }

  # Clear from memory too
  rm(list = ls(envir = .matrixmodel_models), envir = .matrixmodel_models)
}

#' Manual installation instructions
#' @export
model_install_instructions <- function() {
  cache_dir <- getOption("matrixmodel.cache_dir")

  cat("Manual Model Installation Instructions:\n")
  cat("======================================\n")
  cat("1. Visit: https://zenodo.org/record/17138782\n")
  cat("2. Download these three files:\n")
  for (model_type in names(MODEL_FILES)) {
    cat("   - ", MODEL_FILES[[model_type]], "\n")
  }
  cat("3. Create directory: ", cache_dir, "\n")
  cat("4. Place the downloaded .rds files in that directory\n")
  cat("5. Run: load_all_models()\n")
  cat("\n")
  cat("The package will automatically use these manually installed files.\n")
}
