#' Get model loading status
#'
#' @return List showing which models are loaded
#' @export
model_status <- function() {
  list(
    mortality = exists("mortality", envir = .matrixmodel_models),
    upgrowth = exists("upgrowth", envir = .matrixmodel_models),
    recruitment = exists("recruitment", envir = .matrixmodel_models)
  )
}
