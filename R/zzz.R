# zzz.R
.onLoad <- function(libname, pkgname) {
  # Set cache directory option and increase timeout
  op <- options()
  op.matrixmodel <- list(
    matrixmodel.cache_dir = tools::R_user_dir("matrixmodel", "cache")
  )
  toset <- !(names(op.matrixmodel) %in% names(op))
  if (any(toset)) options(op.matrixmodel[toset])

  # Increase default timeout for large downloads
  options(timeout = 600)  # 10 minutes
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the Matrix Model package!")

  # Try to load models
  success <- tryCatch({
    .load_models_with_fallback()
  }, error = function(e) {
    packageStartupMessage("Model loading error: ", e$message)
    FALSE
  })

  if (success) {
    packageStartupMessage("All models loaded successfully!")
  } else if (models_loaded()) {
    packageStartupMessage("Models loaded from cache.")
  } else {
    packageStartupMessage(
      "Could not load models automatically.\n",
      "Use load_all_models() to retry download,\n",
      "or model_install_instructions() for manual setup."
    )
  }
}
