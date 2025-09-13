#' Prepare the prediction vector
#'
#' @param plt_vec The vector
#' @param DBH The DBH whatever
#'
#' @returns A vector
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' prepare_pred_vector(plt_vec, DBH)
#' }
#' @keywords internal
#' @noRd
prepare_pred_vector <- function(plt_vec, DBH) {
  pred_vec <- plt_vec |>
    pivot_longer(cols = matches("^S\\d{2}_D\\d{2}$"),
                 names_to = "S_D", values_to = "TPH") |>
    mutate(
      SPCD    = as.integer(substr(S_D, 2, 3)),
      DGP     = as.integer(substr(S_D, 6, 7)),
      PrevDGP = DGP,
      PrevDBH = DBH[pmin(DGP, length(DBH))]
    ) |>
    group_by(PlotID) |>
    mutate(
      BA_unit = pi * (PrevDBH^2) / 40000,
      Hs      = n_distinct(SPCD[TPH > 0]),
      Hd      = n_distinct(DGP[TPH > 0])
    ) |>
    ungroup()

  pred_vec <- update_diversity(pred_vec)
  pred_vec$Year <- 0L
  pred_vec
}
