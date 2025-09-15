#' Forecast forest stocks for a stand
#'
#' This function launches a Shiny application that allows users to
#' interactively run a forest stock forecast.
#'
#' @details
#' The app requires a CSV file in the "matrix model" format. To ensure
#' compliance, check the \emph{validate_wide_matrix()} function.
#'
#' @return This function runs a Shiny application
#'
#' @examples
#' \dontrun{
#' app_forecast_biomass()
#' }
#'
#' @export
app_forecast_biomass <- function() {
  app_dir <- system.file("shiny", "app_forecast_biomass",
                         package = "matrixmodel")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing the package.",
         call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
