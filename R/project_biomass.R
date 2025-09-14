#' Project forest stocks over a plot file
#'
#' @param save_to Path to output folder
#' @param data A data frame or CSV file containing the input data in the
#' "matrix model" format.
#' @param plot_id Character. The identifier of the target plot.
#' @param years Numeric. Number of years to simulate.
#' @param m_model Path to the mortality model '.rds' file.
#' @param u_model Path to the upgrowth model '.rds' file.
#' @param r_model Path to the recruitment model '.rds' file.
#' @param output_folder_name Character. Name for the output file.
#'
#' @returns A list with data frames
#' @export
#'
#' @importFrom utils capture.output read.csv write.csv
#'
#' @examples
#' \dontrun{
#' project_biomass(data = df, plot_id = "1234")
#' }
project_biomass <- function(save_to = NULL,
                            data = NULL,
                            plot_id = NULL,
                            years = 50,
                            m_model = "./models/model_mortality.rds",
                            u_model = "./models/model_upgrowth.rds",
                            r_model = "./models/model_recruitment1.rds",
                            output_folder_name = "simulation_output") {

  if(is.null(save_to)){
    save_to <- getwd()
  }

  # Create main output directory
  summary_output <- file.path(save_to, output_folder_name)
  if(!dir.exists(summary_output)) {
    dir.create(summary_output, recursive = TRUE)
  }

  set.seed(1234)

  # ---- Inputs ----
  t1 <- data

  if (!is.null(plot_id)){
    # Input validation
    if (!all(plot_id %in% t1$PlotID)) {
      missing_ids <- setdiff(plot_id, t1$PlotID)
      warning("Some plot_ids not found in data: ",
              paste(missing_ids, collapse = ", "))
    }

    original_count <- nrow(t1)
    t1 <- t1  |>
      dplyr::filter(PlotID %in% plot_id)

    if (nrow(t1) == 0) {
      stop("No plots found with the specified plot_id(s): ",
           paste(plot_id, collapse = ", "))
    }

    if (nrow(t1) > 1) {
      warning("Multiple plots found with specified plot_id(s).
              Using first plot only.")
      t1 <- t1[1, ]
    }

    cat("Processing plot:", t1$PlotID[1], "\n")
  }

  m <- readRDS(m_model)
  u <- readRDS(u_model)
  r <- readRDS(r_model)

  DBH <- (seq(10, 80, 5)) + 2.5
  DBH[15] <- 85

  # ---- Process single plot ----
  cat("Calculating projection for single plot...\n")

  plt_vec <- t1[1, ]
  actual_plot_id <- plt_vec$PlotID[1]
  # DBH <- all_dbh_vec

  pred_vec <- prepare_pred_vector(plt_vec, DBH)

  year_results <- vector("list", length = years + 1)
  for (sim_year in 0:years) {
    pred_vec$Year <- sim_year
    pred_vec <- update_predictions(pred_vec, m, u, r, DBH)
    year_results[[sim_year + 1]] <- extract_outputs(pred_vec, sim_year)
  }

  pred_df <- bind_rows(year_results)

  pred_df <- pred_df |>
    mutate(SpeciesGroup = case_when(
      SPCD == 1 ~ 'Sapindaceae (SD)',
      SPCD == 2 ~ 'Quercus - Quercus (QQ)',
      SPCD == 3 ~ 'Quercus - Lobatae (QL)',
      SPCD == 4 ~ 'Quercus - Velutina (QV)',
      SPCD == 5 ~ 'Juglandaceae (JD)',
      SPCD == 6 ~ 'Gymnosperms (GS)',
      SPCD == 7 ~ 'Tulip (TT)',
      SPCD == 8 ~ 'Fagus (FG)',
      SPCD == 9 ~ 'Other Angiosperms (OA)'
    ))

  pred_df <- pred_df |>
    select(PlotID, SpeciesGroup, SPCD, everything())


  summary_df <- summarize_predictions(pred_df)

  # Save using actual PlotID
  safe_plot_id <- gsub("[^a-zA-Z0-9]", "_", actual_plot_id)
  write.csv(pred_df,
            file.path(summary_output,
                      paste0("plot_", safe_plot_id, "_predictions.csv")),
            row.names = FALSE)
  write.csv(summary_df,
            file.path(summary_output,
                      paste0("plot_", safe_plot_id, "_summary.csv")),
            row.names = FALSE)



  summary_by_year <- summary_df |>
    group_by(PlotID, Year) |>
    summarise(
      BA_total_mean      = mean(BA_total, na.rm = TRUE),
      # BA_total_sd        = sd(BA_total,   na.rm = TRUE),
      # BA_total_se        = BA_total_sd / sqrt(n()),
      # BA_total_ci        = 1.96 * BA_total_se,

      N_total_mean       = mean(N_total, na.rm = TRUE),
      # N_total_sd         = sd(N_total,   na.rm = TRUE),
      # N_total_se         = N_total_sd / sqrt(n()),
      # N_total_ci         = 1.96 * N_total_se,

      # rec_BA_total_mean  = mean(rec_BA_total, na.rm = TRUE),
      # up_BA_total_mean   = mean(up_BA_total,  na.rm = TRUE),
      # mort_BA_total_mean = mean(mort_BA_total, na.rm = TRUE),
      #
      # Hd_mean            = mean(Hd, na.rm = TRUE),
      # Hs_mean            = mean(Hs, na.rm = TRUE),
      # Shannon_DGP_mean   = mean(Shannon_DGP, na.rm = TRUE),
      # Simpson_DGP_mean   = mean(Simpson_DGP, na.rm = TRUE),
      # Shannon_SPCD_mean  = mean(Shannon_SPCD, na.rm = TRUE),
      # Simpson_SPCD_mean  = mean(Simpson_SPCD, na.rm = TRUE),
      .groups = "drop"
    )

  write.csv(summary_by_year,
            file.path(summary_output,
                      paste0("plot_", safe_plot_id, "_year_summary.csv")),
            row.names = FALSE)

  species_year <- pred_df |>
    group_by(PlotID, SPCD, SpeciesGroup, Year) |>
    summarise(
      BA_total     = sum(B, na.rm = TRUE),
      N_total      = sum(N, na.rm = TRUE),
      # rec_BA_total = sum(rec_BA,  na.rm = TRUE),
      # up_BA_total  = sum(up_BA,   na.rm = TRUE),
      # mort_BA_total= sum(mort_BA, na.rm = TRUE),
      .groups = "drop"
    )

  write.csv(species_year,
            file.path(summary_output,
                      paste0("plot_", safe_plot_id,
                             "_sp_year_summary.csv")),
            row.names = FALSE)


  dgp_year <- pred_df |>
    group_by(PlotID, DGP, Year) |>
    summarise(
      BA_total     = sum(B, na.rm = TRUE),
      N_total      = sum(N, na.rm = TRUE),
      # rec_BA_total = sum(rec_BA,  na.rm = TRUE),
      # up_BA_total  = sum(up_BA,   na.rm = TRUE),
      # mort_BA_total= sum(mort_BA, na.rm = TRUE),
      .groups = "drop"
    )

  write.csv(dgp_year,
            file.path(summary_output,
                      paste0("plot_", safe_plot_id,
                             "_dgp_year_summary.csv")),
            row.names = FALSE)

  cat("Finished processing plot:", actual_plot_id, "\n")

  # Return results for this single plot
  results <- list(
    plot_id = actual_plot_id,
    predictions = pred_df,
    summary = summary_df,
    summary_by_year = summary_by_year,
    species_year = species_year,
    dgp_year = dgp_year
  )


  cat("Outputs written to:", summary_output, "\n")
  return(results)
}
