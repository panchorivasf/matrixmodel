.onLoad <- function(libname, pkgname) {
  # Set options
  op <- options()
  op.matrixmodel <- list(
    matrixmodel.cache_dir = tools::R_user_dir("matrixmodel", "cache")
  )
  toset <- !(names(op.matrixmodel) %in% names(op))
  if (any(toset)) options(op.matrixmodel[toset])

  # Load models automatically in the background
  # This will download them if not cached
  if (interactive()) {
    message("MatrixModel: Loading models in background...")
    future::plan(future::multisession)  # Use background process
    future::future({
      tryCatch({
        .load_models()
        message("MatrixModel: All models loaded successfully!")
      }, error = function(e) {
        warning("MatrixModel: Failed to load models: ", e$message)
      })
    })
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to the Matrix Model package!\n",
    "Large model files are being loaded in the background from\n",
    "Purdue's PURR repository. This may take a few moments.\n",
    "Use models_loaded() to check if models are ready."
  )

  # For non-interactive sessions, load models immediately
  if (!interactive()) {
    tryCatch({
      .load_models()
    }, error = function(e) {
      packageStartupMessage("Warning: Failed to load models: ", e$message)
    })
  }
}
