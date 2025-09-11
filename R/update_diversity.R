#' Update diversity data
#'
#' @param pred_vec Prediction vector
#'
#' @returns An updated diversity data
#' @export
#'
#' @examples
#' \dontrun{
#' update_diversity()
#' }
update_diversity <- function(pred_vec) {

  dgp_div <- pred_vec |>
    group_by(PlotID, DGP) |>
    summarise(TPH_sum = sum(TPH), .groups = "drop") |>
    group_by(PlotID) |>
    mutate(
      p = TPH_sum / sum(TPH_sum),
      Shannon_DGP = -sum(p * log(p), na.rm = TRUE),
      Simpson_DGP = sum(p^2, na.rm = TRUE)
    ) |>
    select(PlotID, Shannon_DGP, Simpson_DGP) |> distinct()

  spcd_div <- pred_vec |>
    group_by(PlotID, SPCD) |>
    summarise(TPH_sum = sum(TPH), .groups = "drop") |>
    group_by(PlotID) |>
    mutate(
      p = TPH_sum / sum(TPH_sum),
      Shannon_SPCD = -sum(p * log(p), na.rm = TRUE),
      Simpson_SPCD = sum(p^2, na.rm = TRUE)
    ) |>
    select(PlotID, Shannon_SPCD, Simpson_SPCD) |> distinct()


}
