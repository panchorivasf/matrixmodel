#' Prepare the prediction vector
#'
#' @param plt_vec The vector
#' @param DBH The DBH whatever
#'
#' @returns A vector
#' @export
#'
#' @examples
#' prepare_pred_vector(plt_vec, DBH)
#'
prepare_pred_vector <- function(plt_vec, DBH) {
  pred_vec <- plt_vec |>
    pivot_longer(cols = matches("^S\\d{2}_D\\d{2}$"),
                 names_to = "S_D", values_to = "TPH") |>
    mutate(
      SPCD    = as.factor(as.numeric(substr(S_D, 2, 3))),
      DGP     = as.numeric(substr(S_D, 6, 7)),
      PrevDGP = DGP,
      PrevDBH = DBH[DGP]
    ) |>
    group_by(PlotID) |>
    mutate(
      BA_unit = pi * (PrevDBH^2) / 40000,
      PrevB   = sum(TPH * BA_unit, na.rm = TRUE),
      PrevN   = sum(TPH, na.rm = TRUE),
      Hs      = n_distinct(SPCD[TPH > 0]),
      Hd      = n_distinct(DGP[TPH > 0])
    ) |>
    ungroup()

  pred_vec <- update_diversity(pred_vec)
  pred_vec <- pred_vec |> mutate(Year = 0L)
  pred_vec
}
