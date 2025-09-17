# R/load-models.R

# Global environment for models
.matrixmodel_models <- new.env()

# Direct download URLs (permanent since Zenodo doesn't allow changes)
MODEL_URLS <- list(
  mortality = "https://zenodo.org/records/17139020/files/model_indiana_mortality_v2.rds?download=1",
  upgrowth = "https://zenodo.org/records/17139020/files/model_indiana_upgrowth_v2.rds?download=1",
  recruitment = "https://zenodo.org/records/17139020/files/model_indiana_recruitment_v2.rds?download=1"
)

# File names for caching
MODEL_FILES <- list(
  mortality = "model_indiana_mortality_v2.rds",
  upgrowth = "model_indiana_upgrowth_v2.rds",
  recruitment = "model_indiana_recruitment_v2.rds"
)

# Internal function to load models from Zenodo
.load_models <- function() {
  if (models_loaded()) return(TRUE)

  message("Downloading models from Zenodo...")

  # Create cache directory if it doesn't exist
  cache_dir <- getOption("matrixmodel.cache_dir")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    message("Created cache directory: ", cache_dir)
  }

  download_count <- 0
  total_models <- length(MODEL_URLS)

  for (model_type in names(MODEL_URLS)) {
    file_name <- MODEL_FILES[[model_type]]
    cache_path <- file.path(cache_dir, file_name)

    # Skip if already in memory (shouldn't happen, but safe)
    if (exists(model_type, envir = .matrixmodel_models)) {
      message("Already loaded: ", model_type)
      download_count <- download_count + 1
      next
    }

    message("Downloading ", file_name, "...")

    success <- download_with_retry(
      url = MODEL_URLS[[model_type]],
      dest_path = cache_path,
      max_retries = 2,
      timeout = 600
    )

    if (success) {
      tryCatch({
        .matrixmodel_models[[model_type]] <- readRDS(cache_path)
        message("  ??? ", model_type, " model loaded successfully")
        download_count <- download_count + 1
      }, error = function(e) {
        warning("Downloaded file is corrupt: ", model_type, " - ", e$message)
        unlink(cache_path)  # Remove corrupt file
      })
    } else {
      warning("Failed to download: ", file_name)
    }
  }

  return(download_count == total_models)
}
# .load_models <- function() {
#   if (models_loaded()) return(TRUE)
#
#   message("Downloading models from Zenodo...")
#
#   # Create cache directory
#   cache_dir <- getOption("matrixmodel.cache_dir")
#   if (!dir.exists(cache_dir)) {
#     dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
#   }
#
#   # Download each model file
#   for (model_type in names(MODEL_URLS)) {
#     file_name <- MODEL_FILES[[model_type]]
#     cache_path <- file.path(cache_dir, file_name)
#
#     message("Downloading ", file_name, "...")
#
#     # Use robust download with retries
#     success <- download_with_retry(
#       url = MODEL_URLS[[model_type]],
#       dest_path = cache_path,
#       max_retries = 3,
#       timeout = 600  # 10 minutes for the 140MB file
#     )
#
#     if (success) {
#       # Load into memory
#       .matrixmodel_models[[model_type]] <- readRDS(cache_path)
#       message("  ??? ", model_type, " model loaded successfully")
#     } else {
#       warning("Failed to download: ", file_name)
#     }
#   }
#
#   return(models_loaded())
# }

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

  # Check if cache directory exists
  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist: ", cache_dir)
    return(FALSE)
  }

  loaded_count <- 0
  total_models <- length(MODEL_FILES)

  for (model_type in names(MODEL_FILES)) {
    file_name <- MODEL_FILES[[model_type]]
    cache_path <- file.path(cache_dir, file_name)

    if (file.exists(cache_path)) {
      tryCatch({
        .matrixmodel_models[[model_type]] <- readRDS(cache_path)
        message("Loaded from cache: ", model_type)
        loaded_count <- loaded_count + 1
      }, error = function(e) {
        warning("Corrupt cache file, will re-download: ", model_type, " - ", e$message)
        # Remove corrupt cache file
        unlink(cache_path)
      })
    } else {
      message("Cache file not found: ", file_name)
    }
  }

  # Return TRUE only if ALL models are loaded from cache
  return(loaded_count == total_models)
}

# Try cache first, then download if needed
.load_models_with_fallback <- function() {
  # FIRST try cache
  cache_success <- .load_models_from_cache()

  if (cache_success) {
    message("Models loaded from cache.")
    return(TRUE)
  }

  # If cache fails, THEN download from Zenodo
  message("Cache not found or corrupt, downloading from Zenodo...")
  download_success <- .load_models()

  if (!download_success) {
    warning("Both cache and download failed. Models not loaded.")
    return(FALSE)
  }

  return(TRUE)
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
  cat("1. Visit: https://zenodo.org/record/17139020\n")
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
