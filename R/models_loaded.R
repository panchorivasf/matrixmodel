# Function to check if models are loaded
#' @export
models_loaded <- function() {
  all(
    exists("mortality", envir = .matrixmodel_models),
    exists("upgrowth", envir = .matrixmodel_models),
    exists("recruitment", envir = .matrixmodel_models)
  )
}
