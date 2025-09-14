#' Update predictions
#'
#' @param pred_vec Predictions vector
#' @param m Mortality model
#' @param u Upgrowth model
#' @param r Recruitment model
#' @param DBH Diameter at breast height
#'
#' @importFrom stats predict
#'
#' @returns A prediction
#'
#' @examples
#' \dontrun{
#' update_preductions(pred_vec, m, u, r, DBH)
#' }
#' @keywords internal
#' @noRd
update_predictions <- function(pred_vec, m, u, r, DBH) {
  req_m <- m$forest$independent.variable.names
  req_u <- u$forest$independent.variable.names
  req_r <- r$forest$independent.variable.names

  pred_vec <- pred_vec |> mutate(TPH_1 = TPH)

  mort <- as.numeric(stats::predict(m,
                                    pred_vec[, intersect(req_m,
                                                         names(pred_vec)),
                                             drop = FALSE])$predictions)
  up   <- as.numeric(stats::predict(u,
                                    pred_vec[, intersect(req_u,
                                                         names(pred_vec)),
                                             drop = FALSE])$predictions) / 5
  rec  <- as.numeric(stats::predict(r,
                                    pred_vec[, intersect(req_r,
                                                         names(pred_vec)),
                                             drop = FALSE])$predictions)

  up[is.na(up) | up < 0 | pred_vec$DGP >= 15] <- 0
  mort[is.na(mort) | mort < 0] <- 0
  mort[mort > 1] <- 1
  rec[is.na(rec) | pred_vec$DGP != 1] <- 0

  pred_vec <- pred_vec |>
    mutate(
      mort   = mort,
      up     = up,
      rec    = rec,
      stasis = pmax(0, TPH_1 * (1 - mort - up))
    ) |>
    group_by(PlotID, SPCD) |>
    arrange(DGP, .by_group = TRUE) |>
    mutate(
      moved_up = lag(TPH_1 * up, default = 0),
      recruits = if_else(DGP == 1, rec, 0),
      TPH      = stasis + moved_up + recruits
    ) |>
    ungroup()

  pred_vec <- pred_vec |>
    mutate(
      CurrDBH = DBH[pmin(DGP, length(DBH))],
      BA_unit = pi * (CurrDBH^2) / 40000,
      B_row   = TPH * BA_unit,
      N_row   = TPH,
      rec_BA  = recruits * BA_unit,
      up_BA   = pmax(0, moved_up - TPH_1 * up) * BA_unit,
      mort_BA = TPH_1 * mort * BA_unit
    )

  pred_vec <- update_diversity(pred_vec)
  pred_vec
}
