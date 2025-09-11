#' Update diversity data
#'
#' @param pred_vec Prediction vector
#'
#' @returns An updated diversity data
#'
#' @examples
#' \dontrun{
#' update_diversity()
#' }
#' @keywords internal
#' @noRd
update_diversity <- function(pred_vec) {
  dgp_div <- pred_vec |>
    group_by(PlotID, DGP) |>
    summarise(TPH_sum = sum(TPH), .groups = "drop") |>
    group_by(PlotID) |>
    mutate(
      total = sum(TPH_sum),
      p = if_else(total > 0, TPH_sum / total, 0),
      Shannon_DGP = if_else(total > 0, -sum(p * log(pmax(p, 1e-10)),
                                            na.rm = TRUE), 0),
      Simpson_DGP = if_else(total > 0, sum(p^2, na.rm = TRUE), 0)
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

  pred_vec |>
    select(-any_of(c("Shannon_DGP","Simpson_DGP",
                     "Shannon_SPCD","Simpson_SPCD"))) |>
    left_join(dgp_div, by = "PlotID") |>
    left_join(spcd_div, by = "PlotID")
}
