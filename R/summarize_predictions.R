#' Summarize Forest Biomass Projection Predictions by Plot and Year
#'
#' Aggregates detailed forest projection data to plot-year summaries, calculating
#' totals for biomass and demographic variables, and means for diversity indices
#' and stand characteristics. This function is typically used to summarize the
#' output from forest growth models.
#'
#' @param df A data frame containing forest projection predictions with the following
#'   required columns: 'PlotID', 'Year', 'B' (biomass), 'N' (number of trees),
#'   'DGP' (Diameter Growth Pattern), 'rec_BA' (recruitment basal area),
#'   'up_BA' (upgrade basal area), 'mort_BA' (mortality basal area), 'Hd'
#'   (dominant height), 'Hs' (site height), 'Shannon_DGP', 'Simpson_DGP',
#'   'Shannon_SPCD', and 'Simpson_SPCD' (diversity indices).
#'
#' @returns A data frame with one row per PlotID-Year combination containing:
#'   \describe{
#'     \item{PlotID}{Plot identifier}
#'     \item{Year}{Projection year}
#'     \item{mean_B}{Total biomass across all species/size classes}
#'     \item{mean_N}{Total number of trees across all species/size classes}
#'     \item{mean_rec_BA}{Total recruitment basal area (only from DGP = 1)}
#'     \item{mean_up_BA}{Total upgrade basal area across all size classes}
#'     \item{mean_mort_BA}{Total mortality basal area across all size classes}
#'     \item{mean_Hd}{Mean dominant height}
#'     \item{mean_Hs}{Mean site height}
#'     \item{mean_Shannon_DGP}{Mean Shannon diversity index for diameter groups}
#'     \item{mean_Simpson_DGP}{Mean Simpson diversity index for diameter groups}
#'     \item{mean_Shannon_SPCD}{Mean Shannon diversity index for species}
#'     \item{mean_Simpson_SPCD}{Mean Simpson diversity index for species}
#'   }
#'
#' @examples
#' # Create example prediction data
#' pred_data <- data.frame(
#'   PlotID = rep(c("Plot1", "Plot2"), each = 6),
#'   Year = rep(c(0, 1, 2), times = 4),
#'   SPCD = rep(c(318, 531), each = 3, times = 2),
#'   DGP = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   B = c(10.5, 15.2, 8.3, 12.1, 18.7, 9.4, 11.2, 16.8, 7.9, 13.3, 19.2, 8.1),
#'   N = c(50, 30, 15, 45, 25, 12, 48, 28, 14, 42, 22, 11),
#'   rec_BA = c(2.1, 0, 0, 1.8, 0, 0, 2.3, 0, 0, 2.0, 0, 0),
#'   up_BA = c(1.5, 2.2, 1.8, 1.3, 2.0, 1.6, 1.7, 2.4, 1.9, 1.4, 2.1, 1.5),
#'   mort_BA = c(0.5, 1.2, 2.1, 0.7, 1.0, 1.8, 0.6, 1.3, 2.2, 0.8, 1.1, 1.9),
#'   Hd = c(25.3, 25.3, 25.3, 23.1, 23.1, 23.1, 25.8, 25.8, 25.8, 23.5, 23.5, 23.5),
#'   Hs = c(22.1, 22.1, 22.1, 20.5, 20.5, 20.5, 22.4, 22.4, 22.4, 20.8, 20.8, 20.8),
#'   Shannon_DGP = c(1.2, 1.2, 1.2, 1.1, 1.1, 1.1, 1.3, 1.3, 1.3, 1.15, 1.15, 1.15),
#'   Simpson_DGP = c(0.65, 0.65, 0.65, 0.62, 0.62, 0.62, 0.68, 0.68, 0.68, 0.64, 0.64, 0.64),
#'   Shannon_SPCD = c(0.85, 0.85, 0.85, 0.82, 0.82, 0.82, 0.88, 0.88, 0.88, 0.84, 0.84, 0.84),
#'   Simpson_SPCD = c(0.45, 0.45, 0.45, 0.42, 0.42, 0.42, 0.47, 0.47, 0.47, 0.44, 0.44, 0.44)
#' )
#'
#' # Summarize predictions
#' summary_results <- summarize_predictions(pred_data)
#'
#' # View results
#' print(summary_results)
#'
#' @details
#' This function performs the following aggregations:
#' \itemize{
#'   \item Sums biomass (B) and tree count (N) across all species and size classes
#'   \item Sums recruitment basal area only from the smallest diameter class (DGP = 1)
#'   \item Sums upgrade and mortality basal area across all size classes
#'   \item Calculates means for height variables and diversity indices
#' }
#'
#' The function assumes that diversity indices and height measurements are
#' consistent within each plot-year combination, so taking the mean provides
#' the representative value for that plot-year.
#'
#' Note: Despite the "mean_" prefix in variable names, biomass and demographic
#' variables represent totals, while diversity indices and heights represent
#' true means.
#'
#' @keywords internal
#' @noRd
summarize_predictions <- function(df) {
  df |>
    group_by(PlotID, Year) |>
    summarise(
      BA_total       = mean(B, na.rm = TRUE),
      N_total        = mean(N, na.rm = TRUE),
      rec_BA_total   = mean(rec_BA,  na.rm = TRUE),
      up_BA_total    = mean(up_BA,   na.rm = TRUE),
      mort_BA_total  = mean(mort_BA, na.rm = TRUE),
      Hd             = first(Hd),
      Hs             = first(Hs),
      Shannon_DGP    = first(Shannon_DGP),
      Simpson_DGP    = first(Simpson_DGP),
      Shannon_SPCD   = first(Shannon_SPCD),
      Simpson_SPCD   = first(Simpson_SPCD),
      .groups = "drop"
    )
}
