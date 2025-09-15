#' Select stands from an interactive map
#'
#' This function launches a Shiny application that allows users to interactively
#' select points from a map and manage selections in a table.
#'
#' @details
#' The app requires a CSV file with specific columns: Latitude, Longitude,
#' PrevB, PrevN, Hs, Hd, and PlotID. Users can select points individually or
#' using a rectangle selection tool.
#'
#' @return This function runs a Shiny application
#'
#' @examples
#' \dontrun{
#' app_select_stand()
#' }
#'
#' @export
app_select_stand <- function() {
  app_dir <- system.file("shiny", "app_select_stand", package = "matrixmodel")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing the package.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
