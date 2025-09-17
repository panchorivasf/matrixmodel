# Lazy-load the models when the package loads
#' @export
#' @keywords internal
.matrixmodel_models <- new.env()

# Function to load models on package startup
.load_models <- function() {
  # Only load if not already loaded
  if (!exists("mortality", envir = .matrixmodel_models)) {
    message("Loading mortality model...")
    .matrixmodel_models$mortality <- load_model("mortality")
  }
  if (!exists("upgrowth", envir = .matrixmodel_models)) {
    message("Loading upgrowth model...")
    .matrixmodel_models$upgrowth <- load_model("upgrowth")
  }
  if (!exists("recruitment", envir = .matrixmodel_models)) {
    message("Loading recruitment model...")
    .matrixmodel_models$recruitment <- load_model("recruitment")
  }
}
