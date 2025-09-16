#' Create static heatmap of species by DGP for single or multiple years using ggplot2
#'
#' Generates static heatmaps showing species-level metrics by DGP
#' for specific year(s) and plot, with appropriate scaling.
#'
#' @param plot_id Character or numeric identifier for the plot to visualize.
#'   If NULL, aggregates data across all plots.
#' @param year Numeric value for the year to visualize. If NULL and all_years = TRUE,
#'   creates plots for all available years.
#' @param data Either a data frame or character string specifying file path/pattern.
#'   If character, uses `read_csv()` to read the file.
#' @param metric Character string specifying the metric to plot: "basal" (basal area)
#'   or "density" (tree density). Defaults to "basal".
#' @param all_years Logical. If TRUE, creates plots for all available years.
#'   Requires year = NULL. Defaults to FALSE.
#' @param dark Logical. Whether to use a dark theme for the graph.
#' @param save_plot Logical indicating whether to save the plot as PNG.
#'   Defaults to FALSE.
#' @param save_path Character string specifying directory path to save the plot.
#' @param save_grid Logical. Whether to save a grid plot with all individual years.
#'   Only used when all_years = TRUE. Defaults to FALSE.
#' @param save_animation Logical. Whether to create and save an animation GIF.
#'   Only used when all_years = TRUE. Defaults to FALSE.
#'
#' @return A ggplot heatmap or a list of plots depending on parameters
#'
#' @examples
#' \dontrun{
#' # Single year plot
#' plot <- view_sp_dbh_heatmap_gg(plot_id = "P1", year = 50, data = spcd_dgp_data)
#'
#' # All years with grid and animation
#' plots <- view_sp_dbh_heatmap_gg(plot_id = "P1", year = NULL, data = spcd_dgp_data,
#'                               all_years = TRUE, save_grid = TRUE, save_animation = TRUE)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c labs theme_minimal theme
#' @importFrom ggplot2 element_text element_rect margin scale_x_discrete scale_y_discrete
#' @importFrom ggplot2 facet_wrap
#' @importFrom patchwork wrap_plots
#' @importFrom readr read_csv
#' @importFrom dplyr filter group_by summarise syms distinct
#' @importFrom glue glue
#' @importFrom grDevices png dev.off
#' @importFrom purrr map
#' @export
view_sp_dbh_heatmap_gg <- function(plot_id = NULL,
                                  year = 50,
                                  data = NULL,
                                  metric = c("basal", "density"),
                                  all_years = FALSE,
                                  dark = TRUE,
                                  save_plot = FALSE,
                                  save_path = NULL,
                                  save_grid = FALSE,
                                  save_animation = FALSE) {

  # Validate input parameters
  if (all_years && !is.null(year)) {
    stop("When all_years = TRUE, year must be NULL")
  }

  if (!all_years && is.null(year)) {
    stop("When all_years = FALSE, year must be specified")
  }

  if ((save_grid || save_animation) && !all_years) {
    warning("save_grid and save_animation are only used when all_years = TRUE. Ignoring these parameters.")
    save_grid <- FALSE
    save_animation <- FALSE
  }

  # Helper functions
  calculate_scale_range <- function(df, metric_col) {
    min_val <- min(df[[metric_col]], na.rm = TRUE)
    max_val <- max(df[[metric_col]], na.rm = TRUE)
    return(c(min_val, max_val))
  }

  load_data <- function(data) {
    if (is.null(data)) {
      stop("'data' must be either a data frame or the name of a csv file")
    } else if (is.data.frame(data)) {
      return(data)
    } else if (is.character(data) && length(data) == 1) {
      return(read_csv(data, show_col_types = FALSE))
    } else {
      stop("'data' must be either a data frame or a character string (file path), not ",
           class(data)[1])
    }
  }

  validate_columns <- function(df, required_cols) {
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
  }

  create_single_heatmap <- function(df_subset, plot_year, metric_col,
                                    metric_display, metric_unit, plot_id,
                                    dark, fill_limits) {

    title_text <- if (!is.null(plot_id)) {
      glue("{metric_display} by Species Group and DGP - Year {plot_year} (Plot {plot_id})")
    } else {
      glue("{metric_display} by Species Group and DGP - Year {plot_year} (All Plots)")
    }

    heatmap <- ggplot(df_subset, aes(x = factor(DGP), y = factor(SpeciesGroup), fill = .data[[metric_col]])) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_viridis_c(
        name = metric_unit,
        option = "viridis",
        limits = fill_limits,
        na.value = "grey50"
      ) +
      labs(
        title = title_text,
        x = "DBH Group (DGP)",
        y = "Species Group"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.y = element_text(hjust = 1),
        panel.grid = element_blank()
      )

    if (dark) {
      heatmap <- heatmap +
        theme(
          plot.background = element_rect(fill = "#2d3e50", color = NA),
          panel.background = element_rect(fill = "#1a2530", color = NA),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          legend.background = element_rect(fill = "#2d3e50"),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white")
        )
    }

    return(heatmap)
  }

  metric <- match.arg(metric)

  # Create metric mappings
  metric_info <- list(
    "basal" = list(col = "BA_total",
                   display = "Basal Area", unit = "m2/ha"),
    "density" = list(col = "N_total",
                     display = "Density", unit = "trees/ha")
  )

  metric_col <- metric_info[[metric]]$col
  metric_display <- metric_info[[metric]]$display
  metric_unit <- metric_info[[metric]]$unit

  # Load and validate data
  df <- load_data(data)

  # Prepare data based on whether plot_id is specified
  if (!is.null(plot_id)) {
    group_vars <- c("PlotID", "SPCD", "SpeciesGroup", "DGP", "Year")
    required_cols <- c("PlotID", "Year", "SpeciesGroup", "DGP", metric_col)
    plot_suffix <- paste0("_plot_", plot_id)
  } else {
    group_vars <- c("SPCD", "SpeciesGroup", "DGP", "Year")
    required_cols <- c("Year", "SpeciesGroup", "DGP", metric_col)
    plot_suffix <- "_all_plots"
  }

  # Aggregate data
  df <- df |>
    group_by(!!!syms(group_vars)) |>
    summarise(
      BA_total = sum(B, na.rm = TRUE),
      N_total = sum(N, na.rm = TRUE),
      .groups = "drop"
    )

  # Validate required columns
  validate_columns(df, required_cols)

  # Filter data by plot if specified
  if (!is.null(plot_id)) {
    df <- df %>% filter(PlotID == plot_id)
    if (nrow(df) == 0) {
      stop("No data found for PlotID: ", plot_id)
    }
  }

  # Get available years
  available_years <- sort(unique(df$Year))

  if (all_years) {
    years_to_plot <- available_years
    # Use whole range for consistent scaling across all years
    fill_limits <- calculate_scale_range(df, metric_col)
  } else {
    years_to_plot <- year
    # Use year-specific range
    df_year <- df %>% filter(Year == year)
    if (nrow(df_year) == 0) {
      stop("No data found for year: ", year)
    }
    fill_limits <- calculate_scale_range(df_year, metric_col)
  }

  # Create plots
  plots_list <- list()

  for (plot_year in years_to_plot) {
    df_year <- df %>% filter(Year == plot_year)

    plot_obj <- create_single_heatmap(
      df_year, plot_year, metric_col,
      metric_display, metric_unit, plot_id,
      dark, fill_limits
    )

    plots_list[[as.character(plot_year)]] <- plot_obj

    # Save individual plots if requested
    if (save_plot) {
      filename <- glue("{metric_display}_speciesDGP_year{plot_year}{plot_suffix}.png")
      file_path <- if (!is.null(save_path)) {
        file.path(save_path, filename)
      } else {
        file.path(getwd(), filename)
      }

      png(file_path, width = 10, height = 8, units = "in", res = 300)
      print(plot_obj)
      dev.off()

      message("Plot saved to: ", file_path)
    }
  }

  # Create and save grid plot if requested
  if (save_grid && all_years) {
    grid_plot <- wrap_plots(plots_list, ncol = 2) +
      plot_annotation(
        title = glue("{metric_display} by Species Group and DGP - All Years"),
        theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
      )

    grid_filename <- glue("{metric_display}_speciesDGP_all_years_grid{plot_suffix}.png")
    grid_file_path <- if (!is.null(save_path)) {
      file.path(save_path, grid_filename)
    } else {
      file.path(getwd(), grid_filename)
    }

    png(grid_file_path, width = 16, height = 12, units = "in", res = 300)
    print(grid_plot)
    dev.off()

    message("Grid plot saved to: ", grid_file_path)
  }

  # Create animation if requested
  if (save_animation && all_years && save_plot) {
    if (is.null(save_path)) {
      animation_path <- getwd()
    } else {
      animation_path <- save_path
    }

    make_gif(
      in_folder = animation_path,
      out_folder = animation_path,
      out_name = glue("{metric_display}_speciesDGP_animation{plot_suffix}"),
      format = "png",
      fps = 1
    )
  }

  # Return appropriate output
  if (all_years) {
    if (length(plots_list) == 1) {
      return(plots_list[[1]])
    } else {
      return(plots_list)
    }
  } else {
    return(plots_list[[1]])
  }
}
