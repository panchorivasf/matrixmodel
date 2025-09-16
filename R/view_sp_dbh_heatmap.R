#' Create interactive heatmap of species by DGP for a single year
#'
#' Generates an interactive heatmap showing species-level metrics by DGP
#' for a specific year and plot, with fixed scale based on all years in dataset.
#'
#' @param plot_id Character or numeric identifier for the plot to visualize.
#'   If NULL, aggregates data across all plots.
#' @param year Numeric value for the year to visualize
#' @param data Either a data frame or character string specifying file path/pattern.
#'   If character, uses `latest_file()` to find the most recent matching file.
#' @param save_to Character string specifying directory path to search for files
#'   (only used if `data` is a character string).
#' @param metric Character string specifying the metric to plot: "basal" (basal area)
#'   or "density" (tree density). Defaults to "basal".
#' @param whole_range Logical. Whether to use the whole range of values
#' for the metric across all years available.
#' @param dark Logical. Whether to use a dark theme for the graph.
#' @param save_html Logical indicating whether to save interactive HTML plot.
#'   Defaults to FALSE.
#'
#' @return A plotly heatmap showing species by DGP for the specified year
#'
#' @examples
#' \dontrun{
#' # From data frame for specific plot
#' plot <- view_sp_dbh_heatmap(plot_id = "P1", year = 50, data = spcd_dgp_data)
#'
#' # From data frame for all plots
#' plot <- view_sp_dbh_heatmap(plot_id = NULL, year = 50, data = spcd_dgp_data)
#'
#' # From file path
#' plot <- view_sp_dbh_heatmap(plot_id = "P1", year = 50, data = "plot_spcd_dgp_year_data.csv")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly layout
#' @importFrom readr read_csv
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr filter group_by summarise
#' @importFrom glue glue
#' @export
view_sp_dbh_heatmap <- function(plot_id = NULL,
                                year = 50,
                                data = NULL,
                                save_to = NULL,
                                metric = c("basal", "density"),
                                whole_range = FALSE,
                                dark = TRUE,
                                save_html = FALSE) {

  # Validate year input
  if (length(year) != 1 || !is.numeric(year)) {
    stop("'year' must be a single numeric value")
  }

  # Helper functions
  calculate_scale_range <- function(df, metric_col) {
    # Get min and max values across ALL years for consistent scaling
    min_val <- min(df[[metric_col]], na.rm = TRUE)
    max_val <- max(df[[metric_col]], na.rm = TRUE)
    return(c(min_val, max_val))
  }

  calculate_year_range <- function(df_year, metric_col) {
    # Get min and max values for the selected year only
    min_val <- min(df_year[[metric_col]], na.rm = TRUE)
    max_val <- max(df_year[[metric_col]], na.rm = TRUE)
    return(c(min_val, max_val))
  }

  load_data <- function(data) {
    if (is.null(data)) {
      stop("'data' must be either a data frame or the name of a csv file
       in the working directory. If the file is not in the working
       directory, provide the full path.")
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

  create_heatmap <- function(df_year, year, metric_col,
                             metric_display, metric_bar,
                             scale_range, plot_id, dark,
                             whole_range) {
    # Create title
    title_text <- if (!is.null(plot_id)) {
      glue("{metric_display} by Species Group and DGP - Year {year} (Plot {plot_id})")
    } else {
      glue("{metric_display} by Species Group and DGP - Year {year} (All Plots)")
    }

    # Determine zmin and zmax based on whole_range parameter
    if (whole_range) {
      zmin <- scale_range[1]
      zmax <- scale_range[2]
    } else {
      year_range <- calculate_year_range(df_year, metric_col)
      zmin <- year_range[1]
      zmax <- year_range[2]
    }

    # Create heatmap with appropriate scaling
    heatmap <- plot_ly(df_year,
                       x = ~factor(DGP),
                       y = ~factor(SpeciesGroup),
                       z = ~.data[[metric_col]],
                       type = "heatmap",
                       colors = "viridis",
                       zmin = zmin,
                       zmax = zmax,
                       colorbar = list(
                         title = list(text = metric_bar, side = "right"),
                         thickness = 20
                       ),
                       hoverinfo = "text",
                       text = ~glue("Species Group: {SpeciesGroup}<br>DGP: {DGP}<br>{metric_display}: {round(.data[[metric_col]], 2)}"))

    # Add layout options
    heatmap <- heatmap %>%
      plotly::layout(
        title = list(text = title_text, font = list(size = 16)),
        xaxis = list(title = "DBH Group (DGP)"),
        yaxis = list(title = "Species Group"),
        margin = list(l = 120, r = 120, t = 80, b = 80)
      )

    # Apply dark theme if requested
    if (dark) {
      heatmap <- heatmap %>%
        plotly::layout(
          plot_bgcolor = "#1a2530",
          paper_bgcolor = "#2d3e50",
          font = list(color = "white"),
          title = list(font = list(color = "white")),
          xaxis = list(
            title = list(text = "DBH Group (DGP)", font = list(color = "white")),
            color = "white",
            gridcolor = "#4a6572",
            zerolinecolor = "#4a6572",
            linecolor = "white",
            tickfont = list(color = "white")
          ),
          yaxis = list(
            title = list(text = "Species Group", font = list(color = "white")),
            color = "white",
            gridcolor = "#4a6572",
            zerolinecolor = "#4a6572",
            linecolor = "white",
            tickfont = list(color = "white")
          )
        )
    }

    return(heatmap)
  }

  save_plot <- function(heatmap, save_to, metric_display,
                        year, plot_suffix) {
    filename <- glue("{metric_display}_speciesDGP_year{year}{plot_suffix}.html")
    save_path <- if (!is.null(save_to)) {
      file.path(save_to, filename)
    } else {
      file.path(getwd(), filename)
    }
    saveWidget(heatmap, save_path)
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
  metric_bar <- metric_info[[metric]]$unit

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

  # Calculate fixed scale range from ALL years in the dataset
  scale_range <- calculate_scale_range(df, metric_col)

  # Filter data for the selected year only
  df_year <- df %>% filter(Year == year)
  if (nrow(df_year) == 0) {
    stop("No data found for year: ", year)
  }

  # Create the heatmap
  heatmap <- create_heatmap(df_year, year, metric_col,
                            metric_display, metric_bar,
                            scale_range, plot_id, dark,
                            whole_range)

  # Save if requested
  if (save_html) {
    save_plot(heatmap, save_to, metric_display, year, plot_suffix)
  }

  return(heatmap)
}


