# Lazy-load the models when the package loads
#' @export
#' @keywords internal
.matrixmodel_models <- new.env()

# Function to load models
.load_models <- function() {
  # Check if already loaded
  if (models_loaded()) return(TRUE)

  message("Downloading models from PURR repository...")

  tryCatch({
    # Load mortality model
    message("  - Loading mortality model...")
    .matrixmodel_models$mortality <- load_model("mortality")

    # Load upgrowth model
    message("  - Loading upgrowth model...")
    .matrixmodel_models$upgrowth <- load_model("upgrowth")

    # Load recruitment model
    message("  - Loading recruitment model...")
    .matrixmodel_models$recruitment <- load_model("recruitment")

    message("All models loaded successfully!")
    return(TRUE)
  }, error = function(e) {
    warning("Failed to load models: ", e$message)
    return(FALSE)
  })
}

# Check if models are loaded
models_loaded <- function() {
  all(
    exists("mortality", envir = .matrixmodel_models) &&
      !is.null(.matrixmodel_models$mortality),
    exists("upgrowth", envir = .matrixmodel_models) &&
      !is.null(.matrixmodel_models$upgrowth),
    exists("recruitment", envir = .matrixmodel_models) &&
      !is.null(.matrixmodel_models$recruitment)
  )
}

# Get a model
get_model <- function(model_name = c("mortality", "upgrowth", "recruitment")) {
  model_name <- match.arg(model_name)
  if (!exists(model_name, envir = .matrixmodel_models)) {
    stop("Model '", model_name, "' not loaded. Use load_all_models() first.")
  }
  return(get(model_name, envir = .matrixmodel_models))
}
