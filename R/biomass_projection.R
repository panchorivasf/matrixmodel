#' Project forest biomass over time using mortality, upgrowth, and recruitment
#' models
#'
#' This function projects forest biomass dynamics over specified years using
#' pre-trained random forest models for mortality, upgrowth, and recruitment.
#' It handles both single plots and multiple plots in parallel, and generates
#' comprehensive output files for analysis.
#'
#' @param output_dir Directory where output files will be saved. Defaults to
#'   current working directory if NULL.
#' @param cores Number of CPU cores to use for parallel processing. Use -1 to
#'   use all available cores minus one.
#' @param plot_data Data frame containing plot data with species and diameter
#'   group information.
#' @param plot_id Optional specific plot ID to process. If NULL, processes all
#' plots.
#' @param dbh_ref Data frame containing DBH reference data for different
#'   diameter groups.
#' @param years Number of years to project forward. Default is 50 years.
#' @param m_model Path to mortality model RDS file.
#' @param u_model Path to upgrowth model RDS file.
#' @param r_model Path to recruitment model RDS file.
#'
#' @return Returns the path to the output directory containing all generated
#'   CSV files with projection results.
#'
#' @details The function creates a "model_output" directory containing:
#' \itemize{
#'   \item Detailed predictions for all plots and years
#'   \item Summary statistics by plot and year
#'   \item Year-level aggregates across plots
#'   \item Species-level summaries
#'   \item Diameter group projections
#'   \item Various combinations of plot, species, diameter group, and year
#'   summaries
#' }
#'
#' @examples
#' \dontrun{
#' # Process a single plot
#' result <- biomass_projection(
#'   plot_data = data,
#'   plot_id = "49628170910",
#'   dbh_ref = db_ref,
#'   years = 5
#' )
#'
#' # Process all plots in parallel
#' result <- biomass_projection(
#'   plot_data = data,
#'   dbh_ref = db_ref,
#'   cores = 4,
#'   years = 10
#' )
#' }
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dfr
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom readr write_csv
#' @importFrom stats sd
#' @export
#'
biomass_projection <- function(output_dir = NULL,
                               cores = -1,
                               plot_data = NULL,
                               plot_id = NULL,
                               dbh_ref = NULL,
                               years = 50,
                               m_model = "./models/model_mortality.rds",
                               u_model = "./models/model_upgrowth.rds",
                               r_model = "./models/model_recruitment1.rds"
) {


  if(is.null(output_dir)){
    output_dir <- getwd()
  }

  # Create main output directory
  summary_output <- file.path(output_dir, "simulation_output")
  if(!dir.exists(summary_output)) {
    dir.create(summary_output, recursive = TRUE)
  }

  # Create temp directories INSIDE simulation_output
  temp_pred_dir <- file.path(summary_output, "temp_predictions")
  temp_summary_dir <- file.path(summary_output, "temp_summaries")
  dir.create(temp_pred_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(temp_summary_dir, showWarnings = FALSE, recursive = TRUE)

  if(cores == -1){
    cores = parallel::detectCores()-1
  }
  set.seed(1234)

  # ---- Inputs ----
  t1 <- plot_data

  if (!is.null(plot_id)){
    # Input validation
    if (!all(plot_id %in% t1$PlotID)) {
      missing_ids <- setdiff(plot_id, t1$PlotID)
      warning("Some plot_ids not found in plot_data: ", paste(missing_ids, collapse = ", "))
    }

    original_count <- nrow(t1)
    t1 <- t1  |>
      dplyr::filter(PlotID %in% plot_id)

    if (nrow(t1) == 0) {
      stop("No plots found with the specified plot_id(s): ", paste(plot_id, collapse = ", "))
    }

    cat("Filtered to", nrow(t1), "plot(s) out of", original_count, "total\n")
  }

  m <- readRDS(m_model)
  u <- readRDS(u_model)
  r <- readRDS(r_model)

  # ---- DBH reference ----
  dbh_mean_all <- dbh_ref |>
    group_by(DGP) |> summarise(DBH = mean(DBH, na.rm = TRUE), .groups = "drop")
  dbh_mean_drc <- dbh_ref |>
    filter(grepl("DRC", PlotID)) |>
    group_by(DGP) |> summarise(DBH = mean(DBH, na.rm = TRUE), .groups = "drop")

  ref <- tibble(DGP = 1:15) |>
    left_join(dbh_mean_drc |> rename(DBH_drc = DBH), by = "DGP") |>
    left_join(dbh_mean_all |> rename(DBH_all = DBH), by = "DGP") |>
    mutate(DBH = coalesce(DBH_drc, DBH_all))

  all_dbh_vec <- ref$DBH_all
  drc_dbh_vec <- ref$DBH

  # ---- Helpers ----
  update_diversity <- function(pred_vec) {
    dgp_div <- pred_vec |>
      group_by(PlotID, DGP) |>
      summarise(TPH_sum = sum(TPH), .groups = "drop") |>
      group_by(PlotID) |>
      mutate(
        total = sum(TPH_sum),
        p = if_else(total > 0, TPH_sum / total, 0),
        Shannon_DGP = if_else(total > 0, -sum(p * log(pmax(p, 1e-10)), na.rm = TRUE), 0),
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
      select(-any_of(c("Shannon_DGP","Simpson_DGP","Shannon_SPCD","Simpson_SPCD"))) |>
      left_join(dgp_div, by = "PlotID") |>
      left_join(spcd_div, by = "PlotID")
  }

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

  update_predictions <- function(pred_vec, m, u, r, DBH) {
    req_m <- m$forest$independent.variable.names
    req_u <- u$forest$independent.variable.names
    req_r <- r$forest$independent.variable.names

    pred_vec <- pred_vec |> mutate(TPH_1 = TPH)

    mort <- as.numeric(predict(m, pred_vec[, intersect(req_m, names(pred_vec)), drop = FALSE])$predictions)
    up   <- as.numeric(predict(u, pred_vec[, intersect(req_u, names(pred_vec)), drop = FALSE])$predictions) / 5
    rec  <- as.numeric(predict(r, pred_vec[, intersect(req_r, names(pred_vec)), drop = FALSE])$predictions)

    up[is.na(up) | up < 0 | pred_vec$DGP >= 20] <- 0
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

  extract_outputs <- function(pred_vec, sim_year) {
    # Debug: check structure
    # cat("Extracting outputs for year", sim_year, "- columns:",
    #     paste(names(pred_vec), collapse = ", "), "\n")

    result <- pred_vec |>
      transmute(
        PlotID, Year = sim_year, DGP, SPCD,
        B = B_row, N = N_row,
        rec_BA, up_BA, mort_BA,
        Hd, Hs
        # Shannon_DGP,
        # Simpson_DGP,
        # Shannon_SPCD,
        # Simpson_SPCD
      )

    # Debug: check result
    # cat("Extracted outputs have columns:", paste(names(result), collapse = ", "), "\n")
    return(result)
  }
  # Replace the summarize_predictions function with a more robust version:
  summarize_predictions <- function(df) {
    # Debug: check what columns are available
    # cat("Columns in summarize_predictions:", paste(names(df), collapse = ", "), "\n")

    # Check if Year column exists
    if (!"Year" %in% names(df)) {
      stop("Year column missing in summarize_predictions. Available columns: ",
           paste(names(df), collapse = ", "))
    }

    df |>
      group_by(PlotID, Year) |>
      summarise(
        BA_total       = sum(B, na.rm = TRUE),
        N_total        = sum(N, na.rm = TRUE),
        rec_BA_total   = sum(rec_BA,  na.rm = TRUE),
        up_BA_total    = sum(up_BA,   na.rm = TRUE),
        mort_BA_total  = sum(mort_BA, na.rm = TRUE),
        Hd             = first(Hd),
        Hs             = first(Hs),
        # Shannon_DGP    = first(Shannon_DGP),
        # Simpson_DGP    = first(Simpson_DGP),
        # Shannon_SPCD   = first(Shannon_SPCD),
        # Simpson_SPCD   = first(Simpson_SPCD),
        .groups = "drop"
      )
  }

  # ---- Process plots ----
  # Use sequential processing for single plot or when cores = 1
  cat("Running sequential processing for", nrow(t1), "plot(s)\n")

  results <- list()
  for (i in 1:nrow(t1)) {
    tryCatch({
      plt_vec <- t1[i, ]
      actual_plot_id <- plt_vec$PlotID[1]  # Get the actual PlotID
      DBH <- if (grepl("DRC", actual_plot_id)) drc_dbh_vec else all_dbh_vec

      pred_vec <- prepare_pred_vector(plt_vec, DBH)

      year_results <- vector("list", length = years + 1)
      for (sim_year in 0:years) {
        pred_vec$Year <- sim_year
        pred_vec <- update_predictions(pred_vec, m, u, r, DBH)
        year_results[[sim_year + 1]] <- extract_outputs(pred_vec, sim_year)
      }

      pred_df <- bind_rows(year_results)
      summary_df <- summarize_predictions(pred_df)

      # Save using actual PlotID instead of sequential number
      safe_plot_id <- gsub("[^a-zA-Z0-9]", "_", actual_plot_id)
      write.csv(pred_df, file.path(temp_pred_dir, paste0("plot_", safe_plot_id, ".csv")), row.names = FALSE)
      write.csv(summary_df, file.path(temp_summary_dir, paste0("plot_", safe_plot_id, ".csv")), row.names = FALSE)

      cat(format(Sys.time(), "%H:%M"), "Finished plot", i, "(PlotID:", plot_id, ")\n")
      results[[i]] <- list(pred = pred_df, summary = summary_df)

    }, error = function(e) {
      cat(format(Sys.time(), "%H:%M"), "Error in plot", i, ":", e$message, "\n")
      results[[i]] <- NULL
    })
  }

  # Convert to same format as parallel results
  results <- Filter(Negate(is.null), results)


  # ---- Combine outputs ----
  # Read from the correct temp directories within simulation_output
  # cat("Reading prediction files from:", temp_pred_dir, "\n")
  # cat("Reading summary files from:", temp_summary_dir, "\n")

  # Check what files exist
  pred_files <- list.files(temp_pred_dir, full.names = TRUE, pattern = "\\.csv$")
  summary_files <- list.files(temp_summary_dir, full.names = TRUE, pattern = "\\.csv$")

  # cat("Found", length(pred_files), "prediction files and", length(summary_files), "summary files\n")

  # Read files with better error handling
  read_with_check <- function(file_path) {
    tryCatch({
      df <- read.csv(file_path)
      # cat("Successfully read:", basename(file_path), "- columns:", paste(names(df), collapse = ", "), "\n")
      if (!"Year" %in% names(df)) {
        warning("Year column missing in: ", basename(file_path))
      }
      df
    }, error = function(e) {
      warning("Failed to read: ", basename(file_path), " - ", e$message)
      NULL
    })
  }

  all_preds <- map_dfr(pred_files, read_with_check)
  all_summary <- map_dfr(summary_files, read_with_check)


  all_preds <- all_preds |>
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

  all_preds <- all_preds |>
    select(Year, SPCD, SpeciesGroup, everything())



  # Check if we have any data
  if (nrow(all_preds) == 0) {
    stop("No prediction data found after processing")
  }
  if (nrow(all_summary) == 0) {
    stop("No summary data found after processing")
  }

  # cat("Combined predictions columns:", paste(names(all_preds), collapse = ", "), "\n")
  # cat("Combined summaries columns:", paste(names(all_summary), collapse = ", "), "\n")
  #
  # Check if Year column exists in summaries
  if (!"Year" %in% names(all_summary)) {
    stop("Year column missing in combined summary data. Available columns: ",
         paste(names(all_summary), collapse = ", "))
  }

  # Save combined files to simulation_output directory
  write.csv(all_preds, file.path(summary_output, "all_predictions.csv"), row.names = FALSE)
  write.csv(all_summary, file.path(summary_output, "all_summaries.csv"), row.names = FALSE)

  # ---- Year-level aggregate (across plots) ----
  # Add additional safety check
  # cat("Starting year-level aggregation...\n")
  # cat("Summary data structure:\n")
  # print(str(all_summary))

  # Check if Year column is numeric/integer
  if (!is.numeric(all_summary$Year)) {
    # cat("Year column is of type:", class(all_summary$Year), "\n")
    # Try to convert if it's character/factor
    if (is.character(all_summary$Year) || is.factor(all_summary$Year)) {
      all_summary$Year <- as.integer(as.character(all_summary$Year))
      # cat("Converted Year column to integer\n")
    }
  }

  summary_by_year <- all_summary |>
    group_by(Year) |>
    summarise(
      BA_total_mean      = mean(BA_total, na.rm = TRUE),
      BA_total_sd        = sd(BA_total,   na.rm = TRUE),
      BA_total_se        = BA_total_sd / sqrt(n()),
      BA_total_ci        = 1.96 * BA_total_se,

      N_total_mean       = mean(N_total, na.rm = TRUE),
      N_total_sd         = sd(N_total,   na.rm = TRUE),
      N_total_se         = N_total_sd / sqrt(n()),
      N_total_ci         = 1.96 * N_total_se,

      rec_BA_total_mean  = mean(rec_BA_total, na.rm = TRUE),
      up_BA_total_mean   = mean(up_BA_total,  na.rm = TRUE),
      mort_BA_total_mean = mean(mort_BA_total, na.rm = TRUE),

      Hd_mean            = mean(Hd, na.rm = TRUE),
      Hs_mean            = mean(Hs, na.rm = TRUE),
      # Shannon_DGP_mean   = mean(Shannon_DGP, na.rm = TRUE),
      # Simpson_DGP_mean   = mean(Simpson_DGP, na.rm = TRUE),
      # Shannon_SPCD_mean  = mean(Shannon_SPCD, na.rm = TRUE),
      # Simpson_SPCD_mean  = mean(Simpson_SPCD, na.rm = TRUE),
      .groups = "drop"
    )

  write.csv(summary_by_year, file.path(summary_output, "year_summary.csv"),
            row.names = FALSE)



  # ---- Additional rollups ----
  species_year <- all_preds |>
    group_by(SPCD, SpeciesGroup, Year) |>
    summarise(
      BA_total     = sum(B, na.rm = TRUE),
      N_total      = sum(N, na.rm = TRUE),
      rec_BA_total = sum(rec_BA,  na.rm = TRUE),
      up_BA_total  = sum(up_BA,   na.rm = TRUE),
      mort_BA_total= sum(mort_BA, na.rm = TRUE),
      .groups = "drop"
    )

  write.csv(species_year,
            file.path(summary_output, "species_year.csv"),
            row.names = FALSE)

  # plot_species_year <- all_preds |>
  #   group_by(PlotID, SPCD, SpeciesGroup, Year) |>
  #   summarise(
  #     BA_total     = sum(B, na.rm = TRUE),
  #     N_total      = sum(N, na.rm = TRUE),
  #     rec_BA_total = sum(rec_BA,  na.rm = TRUE),
  #     up_BA_total  = sum(up_BA,   na.rm = TRUE),
  #     mort_BA_total= sum(mort_BA, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  #
  # write.csv(plot_species_year,
  #           file.path(summary_output, "plot_species_year.csv"),
  #           row.names = FALSE)

  dgp_year <- all_preds |>
    group_by(DGP, Year) |>
    summarise(
      BA_total     = sum(B, na.rm = TRUE),
      N_total      = sum(N, na.rm = TRUE),
      rec_BA_total = sum(rec_BA,  na.rm = TRUE),
      up_BA_total  = sum(up_BA,   na.rm = TRUE),
      mort_BA_total= sum(mort_BA, na.rm = TRUE),
      .groups = "drop"
    )

  write.csv(dgp_year, file.path(summary_output, "dgp_year.csv"),
            row.names = FALSE)

  # plot_dgp_year <- all_preds |>
  #   group_by(PlotID, DGP, SpeciesGroup, Year) |>
  #   summarise(
  #     BA_total     = sum(B, na.rm = TRUE),
  #     N_total      = sum(N, na.rm = TRUE),
  #     rec_BA_total = sum(rec_BA,  na.rm = TRUE),
  #     up_BA_total  = sum(up_BA,   na.rm = TRUE),
  #     mort_BA_total= sum(mort_BA, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  #
  # write.csv(plot_dgp_year,
  #           file.path(summary_output, "plot_dgp_year.csv"),
  #           row.names = FALSE)

  # plot_spcd_dgp_year <- all_preds |>
  #   group_by(PlotID, SPCD, SpeciesGroup, DGP, Year) |>
  #   summarise(
  #     BA_total     = sum(B, na.rm = TRUE),
  #     N_total      = sum(N, na.rm = TRUE),
  #     rec_BA_total = sum(rec_BA,  na.rm = TRUE),
  #     up_BA_total  = sum(up_BA,   na.rm = TRUE),
  #     mort_BA_total= sum(mort_BA, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  #
  # write.csv(plot_spcd_dgp_year,
  #           file.path(summary_output, "plot_spcd_dgp_year.csv"),
  #           row.names = FALSE)

  cat("All outputs written to:", summary_output, "\n")
  # return(summary_output)
}
