#' Run Forest Biomass Projection Model with Memory-Efficient Processing
#'
#' Executes a forest biomass projection model using mortality, upgrade, and recruitment
#' models to simulate forest growth over multiple years. The function processes plots
#' in parallel and returns comprehensive results including individual predictions,
#' summaries, and aggregated data by species and year.
#'
#' @param t1_all A data frame containing initial forest inventory data for all plots.
#'   Must include a 'PlotID' column and species/diameter matrix columns.
#' @param dffinal A data frame containing diameter at breast height (DBH) reference
#'   data, including 'DGP' (Diameter Growth Pattern) and 'DBH' columns, and 'PlotID'
#'   for distinguishing DRC plots from other plots.
#' @param model_m_path Character string. File path to the mortality model (RDS file).
#' @param model_u_path Character string. File path to the upgrade/growth model (RDS file).
#' @param model_r_path Character string. File path to the recruitment model (RDS file).
#' @param years Integer. Number of years to project forward. Default is 300.
#' @param plot_ids Character vector. Optional subset of PlotIDs to process. If NULL
#'   (default), all plots in t1_all are processed.
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default
#'   is detectCores() - 1.
#'
#' @returns A named list containing five elements:
#'   \describe{
#'     \item{all_preds}{Data frame with detailed predictions for each plot, species,
#'       and year, including biomass (B), number of trees (N), recruitment basal area
#'       (rec_BA), upgrade basal area (up_BA), and mortality basal area (mort_BA)}
#'     \item{all_summary}{Data frame with summary statistics for each plot and year}
#'     \item{summary_by_year}{Data frame with aggregated statistics by year across
#'       all plots, including means, standard deviations, standard errors, and 95%
#'       confidence intervals}
#'     \item{species_year}{Data frame with totals aggregated by species (SPCD) and year}
#'     \item{plot_species_year}{Data frame with totals aggregated by plot, species,
#'       and year}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load required data
#' t1_data <- read.csv("forest_inventory_t1.csv")
#' dbh_reference <- read.csv("dbh_reference.csv")
#'
#' # Run full projection for 100 years
#' results <- run_biomass_projection_memory(
#'   t1_all = t1_data,
#'   dffinal = dbh_reference,
#'   model_m_path = "models/mortality_model.rds",
#'   model_u_path = "models/upgrade_model.rds",
#'   model_r_path = "models/recruitment_model.rds",
#'   years = 100,
#'   cores = 4
#' )
#'
#' # Run projection for subset of plots
#' subset_results <- run_biomass_projection_memory(
#'   t1_all = t1_data,
#'   dffinal = dbh_reference,
#'   model_m_path = "models/mortality_model.rds",
#'   model_u_path = "models/upgrade_model.rds",
#'   model_r_path = "models/recruitment_model.rds",
#'   years = 50,
#'   plot_ids = c("Plot001", "Plot002", "DRC_Plot003"),
#'   cores = 2
#' )
#'
#' # Access results
#' biomass_trends <- results$summary_by_year
#' species_composition <- results$species_year
#' }
#'
#' @details
#' This function implements a forest growth projection model that:
#' \itemize{
#'   \item Uses separate DBH reference values for DRC plots vs. other plots
#'   \item Applies mortality, upgrade, and recruitment models iteratively
#'   \item Processes plots in parallel for computational efficiency
#'   \item Generates comprehensive output summaries at multiple aggregation levels
#' }
#'
#' The function requires the tidyverse, ranger, and doParallel packages, and depends
#' on helper functions: prepare_pred_vector(), update_predictions(),
#' extract_outputs(), and summarize_predictions().
#'
#' @seealso
#' \code{\link{prepare_pred_vector}}, \code{\link{update_predictions}},
#' \code{\link{extract_outputs}}, \code{\link{summarize_predictions}}
run_biomass_projection_memory <- function(
    t1_all,
    dffinal,
    model_m_path,
    model_u_path,
    model_r_path,
    years = 300,
    plot_ids = NULL,
    cores = (detectCores() -1)
) {
  set.seed(1234)

  if (!is.null(plot_ids)) {
    t1_all <- t1_all |> filter(PlotID %in% plot_ids)
  }

  m <- readRDS(model_m_path)
  u <- readRDS(model_u_path)
  r <- readRDS(model_r_path)

  dbh_mean_all <- dffinal |>
    group_by(DGP) |> summarise(DBH = mean(DBH, na.rm = TRUE), .groups = "drop")
  dbh_mean_drc <- dffinal |>
    filter(grepl("DRC", PlotID)) |>
    group_by(DGP) |> summarise(DBH = mean(DBH, na.rm = TRUE), .groups = "drop")

  ref <- tibble(DGP = 1:20) |>
    left_join(dbh_mean_drc |> rename(DBH_drc = DBH), by = "DGP") |>
    left_join(dbh_mean_all |> rename(DBH_all = DBH), by = "DGP") |>
    mutate(DBH = coalesce(DBH_drc, DBH_all))

  all_dbh_vec <- ref$DBH_all
  drc_dbh_vec <- ref$DBH

  registerDoParallel(cores)

  res_list <- foreach(i = 1:nrow(t1_all),
                      .packages = c("tidyverse","ranger")) %dopar% {
    plt_vec <- t1_all[i, ]
    DBH <- if (grepl("DRC", plt_vec$PlotID[1])) drc_dbh_vec else all_dbh_vec

    pred_vec <- prepare_pred_vector(plt_vec, DBH)

    results <- vector("list", length = years + 1L)
    for (yr in 0:years) {
      pred_vec$Year <- yr
      pred_vec <- update_predictions(pred_vec, m, u, r, DBH)
      results[[yr + 1L]] <- extract_outputs(pred_vec, yr)
    }

    pred_df    <- bind_rows(results)
    summary_df <- summarize_predictions(pred_df)

    list(pred = pred_df, summary = summary_df)
  }

  stopImplicitCluster()

  all_preds   <- map(res_list, "pred") |> bind_rows()
  all_summary <- map(res_list, "summary") |> bind_rows()

  summary_by_year <- all_summary |>
    group_by(Year) |>
    summarise(across(starts_with("mean_"), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x,   na.rm = TRUE),
      se   = ~sd(.x, na.rm = TRUE)/sqrt(n()),
      ci   = ~1.96*sd(.x, na.rm = TRUE)/sqrt(n())
    ), .names = "{.col}_{.fn}"), .groups = "drop")

  species_year <- all_preds |>
    group_by(SPCD, Year) |>
    summarise(
      B_total     = sum(B, na.rm = TRUE),
      N_total     = sum(N, na.rm = TRUE),
      rec_BA_sum  = sum(rec_BA,  na.rm = TRUE),
      up_BA_sum   = sum(up_BA,   na.rm = TRUE),
      mort_BA_sum = sum(mort_BA, na.rm = TRUE),
      .groups = "drop"
    )

  plot_species_year <- all_preds |>
    group_by(PlotID, SPCD, Year) |>
    summarise(
      B_total     = sum(B, na.rm = TRUE),
      N_total     = sum(N, na.rm = TRUE),
      rec_BA_sum  = sum(rec_BA,  na.rm = TRUE),
      up_BA_sum   = sum(up_BA,   na.rm = TRUE),
      mort_BA_sum = sum(mort_BA, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    all_preds = all_preds,
    all_summary = all_summary,
    summary_by_year = summary_by_year,
    species_year = species_year,
    plot_species_year = plot_species_year
  )
}
