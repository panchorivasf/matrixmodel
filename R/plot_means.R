#' Compute the plot-wise means for some variables in a "matrix model" data frame
#'
#' @param df A data frame in "matrix model" format
#'
#' @returns A summary data frame with the means per plot
#' for selected variables
#'
#' @importFrom dplyr group_by summarise
#' @export
#'
#' @examples
#' \dontrun{
#' plot_values <- plot_means(df)
#' }
plot_means <- function(df){

  plotmeans <- df |>
    dplyr::group_by(PlotID, Longitude, Latitude) |>
    dplyr::summarise(BA = mean(PrevB),
                     TPA = mean(PrevN),
                     Hs = mean(Hs),
                     Hd = mean(Hd))

  return(plotmeans)

}
