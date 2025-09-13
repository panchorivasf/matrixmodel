#' matrixmodel: Matrix Model for Forest Biomass Projection
#'
#' The matrixmodel package provides functions for projecting forest biomass
#' using matrix models with random forest components, specifically designed
#' for Indiana forests in the US.
#'
#' @section Data:
#' The package works with forest inventory data containing PlotID, SpeciesGroup,
#' Year, and biomass metrics.
#'
#' @docType package
#' @name matrixmodel
#' @aliases matrixmodel-package
#'
#' @importFrom dplyr filter group_by summarise mutate select distinct arrange
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly layout
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom readr read_csv
#' @importFrom htmlwidgets saveWidget
#' @importFrom glue glue
#'
#' @keywords internal
"_PACKAGE"
