#' Extract and Format Model Outputs for a Single Simulation Year
#'
#' Extracts key variables from forest projection model predictions and formats them
#' for output storage. This function selects relevant columns, renames biomass and
#' tree count variables for consistency, and adds the simulation year identifier.
#'
#' @name extract_outputs
#'
#' @param pred_vec A data frame containing model predictions with the following
#'   required columns: 'PlotID', 'PrevB' (previous/current biomass), 'PrevN'
#'   (previous/current number of trees), 'DGP' (Diameter Growth Pattern), 'SPCD'
#'   (species code), 'TPH' (trees per hectare), 'rec_BA' (recruitment basal area),
#'   'up_BA' (upgrade basal area), 'mort_BA' (mortality basal area), 'Hd'
#'   (dominant height), 'Hs' (site height), 'Shannon_DGP', 'Simpson_DGP',
#'   'Shannon_SPCD', and 'Simpson_SPCD' (diversity indices).
#' @param sim_year Integer or numeric. The simulation year to assign to this
#'   set of predictions.
#'
#' @returns A data frame with the following columns:
#'   \describe{
#'     \item{PlotID}{Plot identifier}
#'     \item{B}{Biomass (renamed from PrevB)}
#'     \item{N}{Number of trees (renamed from PrevN)}
#'     \item{DGP}{Diameter Growth Pattern}
#'     \item{SPCD}{Species code}
#'     \item{TPH}{Trees per hectare}
#'     \item{rec_BA}{Recruitment basal area}
#'     \item{up_BA}{Upgrade basal area}
#'     \item{mort_BA}{Mortality basal area}
#'     \item{Hd}{Dominant height}
#'     \item{Hs}{Site height}
#'     \item{Shannon_DGP}{Shannon diversity index for diameter groups}
#'     \item{Simpson_DGP}{Simpson diversity index for diameter groups}
#'     \item{Shannon_SPCD}{Shannon diversity index for species}
#'     \item{Simpson_SPCD}{Simpson diversity index for species}
#'     \item{Year}{Simulation year}
#'   }
#'
#' @examples
#' # Create example prediction vector
#' pred_data <- data.frame(
#'   PlotID = rep("Plot001", 6),
#'   PrevB = c(12.5, 18.3, 9.7, 15.1, 22.4, 8.9),
#'   PrevN = c(45, 32, 18, 38, 28, 15),
#'   DGP = c(1, 2, 3, 1, 2, 3),
#'   SPCD = c(318, 318, 318, 531, 531, 531),
#'   TPH = c(180, 128, 72, 152, 112, 60),
#'   rec_BA = c(2.1, 0, 0, 1.8, 0, 0),
#'   up_BA = c(1.5, 2.2, 1.8, 1.3, 2.0, 1.6),
#'   mort_BA = c(0.5, 1.2, 2.1, 0.7, 1.0, 1.8),
#'   Hd = rep(25.3, 6),
#'   Hs = rep(22.1, 6),
#'   Shannon_DGP = rep(1.2, 6),
#'   Simpson_DGP = rep(0.65, 6),
#'   Shannon_SPCD = rep(0.85, 6),
#'   Simpson_SPCD = rep(0.45, 6),
#'   # Additional columns that will be dropped
#'   temp_var1 = runif(6),
#'   temp_var2 = rnorm(6)
#' )
#'
#' # Extract outputs for year 5
#' year5_output <- extract_outputs(pred_data, sim_year = 5)
#'
#' # View results
#' print(year5_output)
#'
#' # Extract outputs for year 0 (initial conditions)
#' initial_output <- extract_outputs(pred_data, sim_year = 0)
#'
#' @details
#' This function serves as a standardization step in the forest projection workflow:
#' \itemize{
#'   \item Selects only the variables needed for output storage and analysis
#'   \item Renames 'PrevB' and 'PrevN' to 'B' and 'N' for consistent naming across years
#'   \item Adds the simulation year for tracking temporal progression
#'   \item Discards temporary variables and intermediate calculations
#' }
#'
#' The function is typically called within a loop or iterative process where
#' model predictions are generated for multiple years, and consistent output
#' formatting is needed for downstream analysis and summarization.
#'
#' @keywords internal
#' @noRd
extract_outputs <- function(pred_vec, sim_year) {
  pred_vec |>
    select(PlotID, PrevB, PrevN, DGP, SPCD, TPH,
           rec_BA, up_BA, mort_BA,
           Hd, Hs, Shannon_DGP, Simpson_DGP, Shannon_SPCD, Simpson_SPCD) |>
    rename(B = PrevB, N = PrevN) |>
    mutate(Year = sim_year)
}
