#' Project forest stocks for many plots
#'
#' DEPRECATED. This function is deprecated. Please use `forecast_biomass()`
#' instead.
#'
#' @param save_to Path to output folder
#' @param data_source A data frame, a list of data frames, a CSV file path,
#' or the path to a directory with multiple CSVs (individual plots).
#' @param years Numeric. Number of years to simulate.
#' @param m_model Path to the mortality model '.rds' file.
#' @param u_model Path to the upgrowth model '.rds' file.
#' @param r_model Path to the recruitment model '.rds' file.
#' @param output_folder_name Character. A name for the output folder to be
#' created in the 'save_to' directory.
#' @param n_cores Numeric. Number of cores to use in the parallel process.
#' Default is -1 (all but one).
#'
#' @importFrom utils capture.output read.csv write.csv
#'
#' @returns A list with data frames
#' @export
#'
#' @examples
#' \dontrun{
#' project_biomass_batch(data = df_all)
#' }
project_biomass_batch <- function(save_to = NULL,
                                  data_source = NULL,
                                  years = 50,
                                  m_model = NULL,
                                  u_model = NULL,
                                  r_model = NULL,
                                  output_folder_name = "simulation_results_batch",
                                  n_cores = -1) {

  if (n_cores == -1) {
    n_cores <- parallel::detectCores() - 1
  }

  if (is.null(save_to)) {
    save_to <- getwd()
  }

  # Create main output directory
  summary_output <- file.path(save_to, output_folder_name)
  if (!dir.exists(summary_output)) {
    dir.create(summary_output, recursive = TRUE)
  }

  # ---- Data loading functions ----
  load_data_from_source <- function(data_source) {
    if (is.data.frame(data_source)) {
      # Single data frame with multiple plots
      if (!"PlotID" %in% names(data_source)) {
        stop("Data frame must contain a 'PlotID' column")
      }
      # Split by PlotID into a list
      plot_list <- split(data_source, data_source$PlotID)
      return(plot_list)
    } else if (is.list(data_source) && all(sapply(data_source,
                                                  is.data.frame))) {
      # List of data frames
      return(data_source)
    } else if (is.character(data_source) && file.exists(data_source) &&
               grepl("\\.csv$", data_source, ignore.case = TRUE)) {
      # Single CSV file with multiple plots
      all_data <- read.csv(data_source)
      if (!"PlotID" %in% names(all_data)) {
        stop("CSV file must contain a 'PlotID' column")
      }
      # Split by PlotID
      plot_list <- split(all_data, all_data$PlotID)
      return(plot_list)
    } else if (is.character(data_source) && dir.exists(data_source)) {
      # Directory with multiple CSV files
      csv_files <- list.files(data_source, pattern = "\\.csv$",
                              full.names = TRUE, ignore.case = TRUE)
      if (length(csv_files) == 0) {
        stop("No CSV files found in the specified directory")
      }

      plot_list <- list()
      for (file in csv_files) {
        plot_data <- read.csv(file)
        if ("PlotID" %in% names(plot_data)) {
          # Assume each file contains data for one plot
          plot_id <- unique(plot_data$PlotID)
          if (length(plot_id) > 1) {
            warning("File ", basename(file), " contains multiple PlotIDs.
                    Using first one.")
            plot_id <- plot_id[1]
          }
          plot_list[[as.character(plot_id)]] <- plot_data
        } else {
          warning("File ", basename(file), " does not contain a PlotID
                  column. Skipping.")
        }
      }
      return(plot_list)
    } else {
      stop("Invalid data_source. Provide a data frame, list of data frames,
           a CSV file path, or a directory path.")
    }
  }

  # Load data
  if (is.null(data_source)) {
    stop("data_source must be provided")
  }

  plot_list <- load_data_from_source(data_source)

  if (length(plot_list) == 0) {
    stop("No valid plot data found in the provided source")
  }

  cat("Found", length(plot_list), "plots to process\n")

  # Set up parallel cluster
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)

  # Ensure cluster is stopped on exit
  on.exit({
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
  })

  # Define DBH variable here so it can be exported to workers
  DBH <- (seq(10, 80, 5)) + 2.5
  DBH[15] <- 85

  # Export all required functions, variables, and the DBH vector to
  # parallel workers
  parallel::clusterExport(cl, varlist = c(
    "prepare_pred_vector", "update_predictions", "extract_outputs",
    "summarize_predictions", "update_diversity", "DBH"
  ), envir = environment())

  # Also export the project_biomass function itself
  parallel::clusterExport(cl, "project_biomass", envir = environment())

  # Load required packages on each worker
  parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(tidyr)
    # Add any other packages that biomass_projection depends on
  })

  # Process plots in parallel with detailed error reporting
  results <- foreach::foreach(
    i = 1:length(plot_list),
    .packages = c("dplyr", "tidyr", "ranger", "stats"),
    .errorhandling = "pass"
  ) %dopar% {
    plot_data <- plot_list[[i]]
    plot_id <- unique(plot_data$PlotID)[1]

    tryCatch({
      # Call the original function for this plot
      result <- project_biomass(
        save_to = tempdir(),
        data = plot_data,
        plot_id = plot_id,
        years = years,
        m_model = m_model,
        u_model = u_model,
        r_model = r_model
      )

      # Return results without individual plot files
      list(
        predictions = result$predictions,
        summary = result$summary,
        summary_by_year = result$summary_by_year,
        species_year = result$species_year,
        dgp_year = result$dgp_year,
        success = TRUE,
        plot_id = plot_id
      )

    }, error = function(e) {
      # Return detailed error information
      list(
        success = FALSE,
        plot_id = plot_id,
        error_message = e$message,
        error_traceback = paste(capture.output(traceback()), collapse = "\n")
      )
    })
  }

  # Check which plots succeeded and which failed
  successful_plots <- sapply(results, function(x) isTRUE(x$success))
  failed_plots <- !successful_plots

  if (any(failed_plots)) {
    cat("Failed plots:\n")
    for (i in which(failed_plots)) {
      cat("Plot", results[[i]]$plot_id, "failed with error:", results[[i]]$error_message, "\n")
    }
  }

  if (sum(successful_plots) == 0) {
    stop("No plots were successfully processed. Check the error messages above.")
  }

  # Extract only successful results
  successful_results <- results[successful_plots]

  # Combine results
  combined_results <- list(
    predictions = dplyr::bind_rows(lapply(successful_results, function(x) x$predictions)),
    summary = dplyr::bind_rows(lapply(successful_results, function(x) x$summary)),
    summary_by_year = dplyr::bind_rows(lapply(successful_results, function(x) x$summary_by_year)),
    species_year = dplyr::bind_rows(lapply(successful_results, function(x) x$species_year)),
    dgp_year = dplyr::bind_rows(lapply(successful_results, function(x) x$dgp_year))
  )

  # ---- Save combined outputs ----
  cat("Saving combined outputs...\n")

  # Save combined predictions
  write.csv(combined_results$predictions,
            file.path(summary_output, "all_plots_predictions.csv"),
            row.names = FALSE)

  # Save combined summary
  write.csv(combined_results$summary,
            file.path(summary_output, "all_plots_summary.csv"),
            row.names = FALSE)

  # Save combined summary by year
  write.csv(combined_results$summary_by_year,
            file.path(summary_output, "all_plots_year_summary.csv"),
            row.names = FALSE)

  # Save combined species by year
  write.csv(combined_results$species_year,
            file.path(summary_output, "all_plots_sp_year_summary.csv"),
            row.names = FALSE)

  # Save combined DGP by year
  write.csv(combined_results$dgp_year,
            file.path(summary_output, "all_plots_dgp_year_summary.csv"),
            row.names = FALSE)

  cat("Batch processing completed. Successfully processed", sum(successful_plots),
      "out of", length(plot_list), "plots\n")
  cat("Outputs saved to:", summary_output, "\n")

  # Return combined results
  return(combined_results)
}
# project_biomass_batch <- function(save_to = NULL,
#                                      data_source = NULL,
#                                      years = 50,
#                                      m_model = "./models/model_mortality.rds",
#                                      u_model = "./models/model_upgrowth.rds",
#                                      r_model = "./models/model_recruitment1.rds",
#                                      n_cores = - 1) {
#   if (n_cores == -1) {
#     n_cores = parallel::detectCores() - 1
#   }
#
#
#   if (is.null(save_to)) {
#     save_to <- getwd()
#   }
#
#   # Create main output directory
#   summary_output <- file.path(save_to, "simulation_output_batch")
#   if (!dir.exists(summary_output)) {
#     dir.create(summary_output, recursive = TRUE)
#   }
#
#   # ---- Data loading functions ----
#   load_data_from_source <- function(data_source) {
#     if (is.data.frame(data_source)) {
#       # Single data frame with multiple plots
#       if (!"PlotID" %in% names(data_source)) {
#         stop("Data frame must contain a 'PlotID' column")
#       }
#       # Split by PlotID into a list
#       plot_list <- split(data_source, data_source$PlotID)
#       return(plot_list)
#     } else if (is.list(data_source) && all(sapply(data_source, is.data.frame))) {
#       # List of data frames
#       return(data_source)
#     } else if (is.character(data_source) && file.exists(data_source) &&
#                grepl("\\.csv$", data_source, ignore.case = TRUE)) {
#       # Single CSV file with multiple plots
#       all_data <- read.csv(data_source)
#       if (!"PlotID" %in% names(all_data)) {
#         stop("CSV file must contain a 'PlotID' column")
#       }
#       # Split by PlotID
#       plot_list <- split(all_data, all_data$PlotID)
#       return(plot_list)
#     } else if (is.character(data_source) && dir.exists(data_source)) {
#       # Directory with multiple CSV files
#       csv_files <- list.files(data_source, pattern = "\\.csv$",
#                               full.names = TRUE, ignore.case = TRUE)
#       if (length(csv_files) == 0) {
#         stop("No CSV files found in the specified directory")
#       }
#
#       plot_list <- list()
#       for (file in csv_files) {
#         plot_data <- read.csv(file)
#         if ("PlotID" %in% names(plot_data)) {
#           # Assume each file contains data for one plot
#           plot_id <- unique(plot_data$PlotID)
#           if (length(plot_id) > 1) {
#             warning("File ", basename(file), " contains multiple PlotIDs. Using first one.")
#             plot_id <- plot_id[1]
#           }
#           plot_list[[as.character(plot_id)]] <- plot_data
#         } else {
#           warning("File ", basename(file), " does not contain a PlotID column. Skipping.")
#         }
#       }
#       return(plot_list)
#     } else {
#       stop("Invalid data_source. Provide a data frame, list of data frames, a CSV file path, or a directory path.")
#     }
#   }
#
#   # Load data
#   if (is.null(data_source)) {
#     stop("data_source must be provided")
#   }
#
#   plot_list <- load_data_from_source(data_source)
#
#   if (length(plot_list) == 0) {
#     stop("No valid plot data found in the provided source")
#   }
#
#   cat("Found", length(plot_list), "plots to process\n")
#
#   # Set up parallel cluster
#   cl <- parallel::makeCluster(n_cores)
#   doParallel::registerDoParallel(cl)
#
#   # Ensure cluster is stopped on exit
#   on.exit({
#     parallel::stopCluster(cl)
#     foreach::registerDoSEQ()
#   })
#
#   # Process plots in parallel
#   results <- foreach::foreach(
#     i = 1:length(plot_list),
#     .combine = function(...) {
#       results_list <- list(...)
#       # Combine all results into a single list
#       combined <- list(
#         predictions = bind_rows(lapply(results_list, function(x) x$predictions)),
#         summary = bind_rows(lapply(results_list, function(x) x$summary)),
#         summary_by_year = bind_rows(lapply(results_list, function(x) x$summary_by_year)),
#         species_year = bind_rows(lapply(results_list, function(x) x$species_year)),
#         dgp_year = bind_rows(lapply(results_list, function(x) x$dgp_year))
#       )
#       return(combined)
#     },
#     .packages = c("dplyr"),
#     .errorhandling = "pass"
#   ) %dopar% {
#     plot_data <- plot_list[[i]]
#     plot_id <- unique(plot_data$PlotID)[1]
#
#     tryCatch({
#       # Call the original function for this plot
#       result <- biomass_projection(
#         save_to = tempdir(),  # Save individual plots to temp dir
#         data = plot_data,
#         plot_id = plot_id,
#         years = years,
#         m_model = m_model,
#         u_model = u_model,
#         r_model = r_model
#       )
#
#       # Return results without individual plot files
#       list(
#         predictions = result$predictions,
#         summary = result$summary,
#         summary_by_year = result$summary_by_year,
#         species_year = result$species_year,
#         dgp_year = result$dgp_year
#       )
#
#     }, error = function(e) {
#       warning("Error processing plot ", plot_id, ": ", e$message)
#       return(NULL)
#     })
#   }
#
#   # Remove any NULL results from failed processing
#   valid_results <- !sapply(results, is.null)
#   if (any(!valid_results)) {
#     warning(sum(!valid_results), " plots failed to process")
#     results <- results[valid_results]
#   }
#
#   if (length(results) == 0) {
#     stop("No plots were successfully processed")
#   }
#
#   # ---- Save combined outputs ----
#   cat("Saving combined outputs...\n")
#
#   # Save combined predictions
#   write.csv(results$predictions,
#             file.path(summary_output, "all_plots_predictions.csv"),
#             row.names = FALSE)
#
#   # Save combined summary
#   write.csv(results$summary,
#             file.path(summary_output, "all_plots_summary.csv"),
#             row.names = FALSE)
#
#   # Save combined summary by year
#   write.csv(results$summary_by_year,
#             file.path(summary_output, "all_plots_year_summary.csv"),
#             row.names = FALSE)
#
#   # Save combined species by year
#   write.csv(results$species_year,
#             file.path(summary_output, "all_plots_sp_year_summary.csv"),
#             row.names = FALSE)
#
#   # Save combined DGP by year
#   write.csv(results$dgp_year,
#             file.path(summary_output, "all_plots_dgp_year_summary.csv"),
#             row.names = FALSE)
#
#   cat("Batch processing completed. Outputs saved to:", summary_output, "\n")
#
#   # Return combined results
#   return(results)
# }
