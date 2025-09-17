# Function to access models
#' Get loaded model
#'
#' @param model_name Name of model ("mortality", "upgrowth", or "recruitment")
#' @return The loaded model
#' @export
get_model <- function(model_name = c("mortality", "upgrowth", "recruitment")) {
  model_name <- match.arg(model_name)

  # Load models if not already loaded
  if (!exists(model_name, envir = .matrixmodel_models)) {
    .load_models()
  }

  # Return the model
  return(get(model_name, envir = .matrixmodel_models))
}
