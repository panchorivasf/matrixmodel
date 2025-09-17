#' Project forest stocks over a plot file
#'
#' @param save_to Path to output folder
#' @param data A data frame or CSV file containing the input data in the
#' "matrix model" format.
#' @param plot_id Character. The identifier of the target plot.
#' @param years Numeric. Number of years to forecast.
#' @param clear_start Logical. Starts from clear cut. Defaults
#' to FALSE.
#' @param clear_year Numeric. A year to add a clearcut amid simulation.
#' Defaults to NULL (no clearcut).
#' @param planting_init A data frame with SPCD (species group) and TPH
#' (trees per hectare) columns. This is used to simulate growth of a new
#' plantation at the start if clear_start = TRUE. Default is NULL (no
#' plantation).
#' @param planting_post A data frame with SPCD (species group) and TPH
#' (trees per hectare) columns. This is used to simulate growth of a new
#' plantation after a mid-run clearcut (i.e., when clear_year is procided).
#' Default is NULL (no plantation).
#' @param allow_colonization Logical. Whether to allow other species to
#' colonize the stand. Defaults to FALSE (recruits only from the pre-cut
#' species pool.)
#' @param minimal_if_clearcut Logical.if any clear-cut happens, only save
#' summary and year_summary.
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
#' forecast_biomass(data = df, plot_id = "1234")
#' }
forecast_biomass <- function(save_to = NULL,
                             data = NULL,
                             plot_id = NULL,
                             years = 50,
                             clear_start = FALSE,
                             clear_year = NULL,
                             planting_init = NULL,
                             planting_post = NULL,
                             allow_colonization = TRUE,
                             minimal_if_clearcut = TRUE,
                             m_model = NULL,
                             u_model = NULL,
                             r_model = NULL,
                             output_folder_name = "simulation_output") {

  if(is.null(save_to)){
    save_to <- getwd()
  }

  if(is.null(years)){
    stop("Please, provide a target forecast horizon in years.")
  }

  if(is.list(years)){
    stop("Please, provide only one year as forecast horizon.")
  }

  if (is.null(clear_year)) {
    clear_year <- NA_integer_
  }


  # Use package models if not provided
  if (is.null(m_model)) {
    if (!models_loaded()) {
      stop("Mortality model not loaded. Please wait for package initialization or use load_all_models()")
    }
    m_model <- get_model("mortality")
  }

  if (is.null(u_model)) {
    if (!models_loaded()) {
      stop("Upgrowth model not loaded. Please wait for package initialization or use load_all_models()")
    }
    u_model <- get_model("upgrowth")
  }

  if (is.null(r_model)) {
    if (!models_loaded()) {
      stop("Recruitment model not loaded. Please wait for package initialization or use load_all_models()")
    }
    r_model <- get_model("recruitment")
  }

  # Validate that models are actually model objects, not file paths
  if (is.character(m_model)) {
    stop("m_model should be a model object, not a file path. Use get_model('mortality') or load the package first.")
  }
  if (is.character(u_model)) {
    stop("u_model should be a model object, not a file path. Use get_model('upgrowth') or load the package first.")
  }
  if (is.character(r_model)) {
    stop("r_model should be a model object, not a file path. Use get_model('recruitment') or load the package first.")
  }

  # Create main output directory
  summary_output <- file.path(save_to, output_folder_name)
  if(!dir.exists(summary_output)) {
    dir.create(summary_output, recursive = TRUE)
  }

  # Helper functions ----
  sd_cols_in <- function(df) grep('^S[0-9]{2}_D[0-9]{2}$', names(df), value = TRUE)

  safe_diversity_join <- function(pred_vec) {
    dgp_div <- pred_vec %>%
      group_by(PlotID, DGP) %>%
      summarise(TPH_sum = sum(TPH, na.rm = TRUE), .groups = 'drop_last') %>%
      mutate(total = sum(TPH_sum, na.rm = TRUE), p = ifelse(total > 0, TPH_sum / total, 0)) %>%
      summarise(
        Shannon_DGP = ifelse(sum(TPH_sum, na.rm = TRUE) > 0, -sum(p * log(p + 1e-12), na.rm = TRUE), 0),
        Simpson_DGP = ifelse(sum(TPH_sum, na.rm = TRUE) > 0,  sum(p^2, na.rm = TRUE), 0),
        .groups = 'drop'
      )
    spcd_div <- pred_vec %>%
      group_by(PlotID, SPCD) %>%
      summarise(TPH_sum = sum(TPH, na.rm = TRUE), .groups = 'drop_last') %>%
      mutate(total = sum(TPH_sum, na.rm = TRUE), p = ifelse(total > 0, TPH_sum / total, 0)) %>%
      summarise(
        Shannon_SPCD = ifelse(sum(TPH_sum, na.rm = TRUE) > 0, -sum(p * log(p + 1e-12), na.rm = TRUE), 0),
        Simpson_SPCD = ifelse(sum(TPH_sum, na.rm = TRUE) > 0,  sum(p^2, na.rm = TRUE), 0),
        .groups = 'drop'
      )
    pred_vec %>%
      select(-any_of(c('Shannon_DGP','Simpson_DGP','Shannon_SPCD','Simpson_SPCD'))) %>%
      left_join(dgp_div,  by = 'PlotID') %>%
      left_join(spcd_div, by = 'PlotID')
  }

  # Build Prev* & structure features from CURRENT TPH (used at init, after clears)
  sync_prev_from_current <- function(df, DBH) {
    df <- df %>%
      mutate(
        CurrDBH = DBH[pmin(DGP, length(DBH))],
        BA_unit = pi * (CurrDBH^2) / 40000
      ) %>%
      group_by(PlotID) %>%
      mutate(
        PrevB = sum(TPH * BA_unit, na.rm = TRUE),
        PrevN = sum(TPH,           na.rm = TRUE),
        Hs    = n_distinct(SPCD[TPH > 0]),
        Hd    = n_distinct(DGP [TPH > 0])
      ) %>% ungroup() %>%
      mutate(
        PrevDBH = CurrDBH,
        PrevDGP = DGP
      )
    safe_diversity_join(df)
  }

  # After computing NEW TPH this year, set up Prev* for the NEXT year
  prepare_prev_for_next_year <- function(df) {
    df <- df %>%
      mutate(
        PrevDGP = DGP,
        PrevDBH = CurrDBH
      ) %>%
      group_by(PlotID) %>%
      mutate(
        PrevB = sum(B_row, na.rm = TRUE),
        PrevN = sum(N_row, na.rm = TRUE),
        Hs    = n_distinct(SPCD[TPH > 0]),
        Hd    = n_distinct(DGP [TPH > 0])
      ) %>% ungroup()
    safe_diversity_join(df)
  }

  prepare_pred_vector <- function(plt_vec, DBH) {
    pred_vec <- plt_vec %>%
      pivot_longer(cols = matches('^S[0-9]{2}_D[0-9]{2}$'), names_to = 'S_D', values_to = 'TPH') %>%
      mutate(
        SPCD    = as.integer(substr(S_D, 2, 3)),
        DGP     = as.integer(substr(S_D, 6, 7))
      )
    pred_vec$Year <- 0L
    # Initialize Prev*, structure, and diversity from the current TPH state
    sync_prev_from_current(pred_vec, DBH)
  }

  pre_cut_species_pool <- function(plt_vec) {
    sdc <- sd_cols_in(plt_vec)
    if (length(sdc) == 0) return(integer(0))
    vals  <- plt_vec %>% select(all_of(sdc)) %>% as.numeric()
    spcds <- as.integer(substr(sdc, 2, 3))
    unique(spcds[vals > 0])
  }

  update_predictions <- function(pred_vec, m, u, r, DBH,
                                 dgp_widths, allowed_spcds,
                                 allow_colonization) {
    # 1) Validate model variables
    required_m_vars <- m$forest$independent.variable.names
    required_u_vars <- u$forest$independent.variable.names
    required_r_vars <- r$forest$independent.variable.names

    missing_m_vars <- setdiff(required_m_vars, names(pred_vec))
    missing_u_vars <- setdiff(required_u_vars, names(pred_vec))
    missing_r_vars <- setdiff(required_r_vars, names(pred_vec))

    if (length(missing_m_vars) > 0) stop(' Missing variables for mortality model: ',
                                         paste(missing_m_vars, collapse = ', '))
    if (length(missing_u_vars) > 0) stop(' Missing variables for upgrowth model: ',
                                         paste(missing_u_vars, collapse = ', '))
    if (length(missing_r_vars) > 0) stop(' Missing variables for recruitment model: ',
                                         paste(missing_r_vars, collapse = ', '))

    # 2) Predict and update (divide upgrowth by bin width 5)
    pred_vec <- pred_vec %>%
      mutate(
        mort = as.numeric(predict(m, pred_vec[, required_m_vars])$predictions),
        up   = as.numeric(predict(u, pred_vec[, required_u_vars])$predictions) / 5,  # 5 cm bin
        rec  = as.numeric(predict(r, pred_vec[, required_r_vars])$predictions),

        rec  = ifelse(DGP != 1, 0, rec),
        # block colonization if requested
        rec  = ifelse(DGP == 1 & !allow_colonization & !(SPCD %in% allowed_spcds), 0, rec),

        up   = ifelse(DGP >= length(dgp_widths) | up < 0, 0, pmin(up, 1)),
        mort = pmin(pmax(mort, 0), 1),

        TPH_1  = TPH,
        stasis = pmax(0, TPH * (1 - mort - up))
      ) %>%
      group_by(SPCD) %>%
      arrange(DGP, .by_group = TRUE) %>%
      mutate(
        moved_up = lag(up * TPH_1, order_by = DGP, default = 0),
        added    = ifelse(DGP == 1, rec, moved_up)
      ) %>%
      ungroup() %>%
      replace(is.na(.), 0) %>%
      mutate(
        TPH     = stasis + added,
        CurrDBH = DBH[pmin(DGP, length(DBH))],
        BA_unit = pi * (CurrDBH^2) / 40000,
        B_row   = TPH * BA_unit,
        N_row   = TPH,
        rec_BA  = ifelse(DGP == 1, rec * BA_unit, 0),
        up_BA   = moved_up * BA_unit,
        mort_BA = TPH_1 * mort * BA_unit
      )

    # 3) Prepare Prev* for NEXT year (and refresh diversity/structure)
    pred_vec <- prepare_prev_for_next_year(pred_vec)

    pred_vec
  }

  extract_outputs <- function(pred_vec, sim_year) {
    pred_vec %>%
      transmute(
        PlotID, Year = sim_year, DGP, SPCD,
        B = B_row, N = N_row,
        rec_BA, up_BA, mort_BA,
        Hd, Hs
        # Shannon_DGP, Simpson_DGP, Shannon_SPCD, Simpson_SPCD
      )
  }

  apply_planting <- function(pred_vec, planting_df) {
    if (is.null(planting_df) || nrow(planting_df) == 0) return(pred_vec)
    stopifnot(all(c('SPCD','TPH') %in% names(planting_df)))
    pred_vec %>% mutate(
      TPH = if_else(DGP == 1 & SPCD %in% planting_df$SPCD,
                    TPH + dplyr::coalesce(planting_df$TPH[match(SPCD, planting_df$SPCD)], 0),
                    TPH)
    )
  }

  summarize_predictions <- function(df) {
    df %>% group_by(PlotID, Year) %>%
      summarise(BA_total = sum(B, na.rm = TRUE),
                N_total = sum(N, na.rm = TRUE),
                .groups = 'drop')
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
    t1 <- t1  %>%
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
#
#   m <- readRDS(m_model)
#   u <- readRDS(u_model)
#   r <- readRDS(r_model)

  DBH <- (seq(10, 80, 5)) + 2.5
  DBH[15] <- 85

  dgp_widths <- rep(5, length(DBH))
  dgp_widths[length(dgp_widths)] <- Inf

  # ---- Process single plot ----
  # cat("Calculating projection for single plot...\n")

  plt_vec <- t1[1, ]
  actual_plot_id <- plt_vec$PlotID[1]
  # DBH <- all_dbh_vec


  allowed_spcds <- pre_cut_species_pool(plt_vec)
  if (length(allowed_spcds) == 0) {
    allowed_spcds <- unique(as.integer(substr(sd_cols_in(plt_vec), 2, 3)))
  }
  # If we clear at start AND plant, restrict recruitment to planted species only
  if (isTRUE(clear_start) && !is.null(planting_init) && nrow(planting_init) > 0) {
    allowed_spcds <- planting_init$SPCD
  }

  pred_vec <- prepare_pred_vector(plt_vec, DBH)

  year_results <- vector("list", length = years + 1)

  # For Loop ----
  for (sim_year in 0:years) {

    if (sim_year == 0 && isTRUE(clear_start)) {
      pred_vec$TPH <- 0
      pred_vec <- apply_planting(pred_vec, planting_init)
      pred_vec <- sync_prev_from_current(pred_vec, DBH)  # << make models see cleared/planted state
    }
    if (!is.na(clear_year) && sim_year == clear_year) {
      pred_vec$TPH <- 0
      pred_vec <- apply_planting(pred_vec, planting_post)
      pred_vec <- sync_prev_from_current(pred_vec, DBH)  # << same for mid-run clear
      if (!is.null(planting_post) && nrow(planting_post) > 0) {
        allowed_spcds <- planting_post$SPCD  # restrict post-clear recruitment to planted spp
      }
    }

    pred_vec$Year <- sim_year

    # pred_vec <- update_predictions(pred_vec, m, u, r, DBH)
    pred_vec <- update_predictions(
      pred_vec, m, u, r, DBH,
      dgp_widths = dgp_widths,
      allowed_spcds = allowed_spcds,
      allow_colonization = allow_colonization
    )

    year_results[[sim_year + 1]] <- extract_outputs(pred_vec, sim_year)
  }

  # Combine results ----

  pred_df <- bind_rows(year_results)

  pred_df <- pred_df %>%
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

  pred_df <- pred_df %>%
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



  # summary_by_year <- summary_df %>%
  #   group_by(PlotID, Year) %>%
  #   summarise(
  #     BA_total_mean      = mean(BA_total, na.rm = TRUE),
  #     # BA_total_sd        = sd(BA_total,   na.rm = TRUE),
  #     # BA_total_se        = BA_total_sd / sqrt(n()),
  #     # BA_total_ci        = 1.96 * BA_total_se,
  #
  #     N_total_mean       = mean(N_total, na.rm = TRUE),
  #     # N_total_sd         = sd(N_total,   na.rm = TRUE),
  #     # N_total_se         = N_total_sd / sqrt(n()),
  #     # N_total_ci         = 1.96 * N_total_se,
  #
  #     # rec_BA_total_mean  = mean(rec_BA_total, na.rm = TRUE),
  #     # up_BA_total_mean   = mean(up_BA_total,  na.rm = TRUE),
  #     # mort_BA_total_mean = mean(mort_BA_total, na.rm = TRUE),
  #     #
  #     # Hd_mean            = mean(Hd, na.rm = TRUE),
  #     # Hs_mean            = mean(Hs, na.rm = TRUE),
  #     # Shannon_DGP_mean   = mean(Shannon_DGP, na.rm = TRUE),
  #     # Simpson_DGP_mean   = mean(Simpson_DGP, na.rm = TRUE),
  #     # Shannon_SPCD_mean  = mean(Shannon_SPCD, na.rm = TRUE),
  #     # Simpson_SPCD_mean  = mean(Simpson_SPCD, na.rm = TRUE),
  #     .groups = "drop"
  #   )

  # write.csv(summary_by_year,
  #           file.path(summary_output,
  #                     paste0("plot_", safe_plot_id, "_year_summary.csv")),
  #           row.names = FALSE)

  species_year <- pred_df %>%
    group_by(PlotID, SPCD, SpeciesGroup, Year) %>%
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


  dgp_year <- pred_df %>%
    group_by(PlotID, DGP, Year) %>%
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
    # summary_by_year = summary_by_year,
    species_year = species_year,
    dgp_year = dgp_year
  )


  cat("Outputs written to:", summary_output, "\n")
  return(results)
}
