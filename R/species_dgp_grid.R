#' Create interactive faceted heatmap grid of species by DGP
#'
#' Generates an interactive grid of heatmaps showing species-level metrics by DGP
#' across multiple years for a specific plot.
#'
#' @param plot_id Character or numeric identifier for the plot to visualize
#' @param years Numeric vector of years to include in the faceted grid
#' @param data Either a data frame or character string specifying file path/pattern.
#'   If character, uses `latest_file()` to find the most recent matching file.
#' @param dir Character string specifying directory path to search for files
#'   (only used if `data` is a character string).
#' @param metric Character string specifying the metric to plot: "BA_total" (basal area)
#'   or "N_total" (tree density). Defaults to "BA_total".
#' @param save_html Logical indicating whether to save interactive HTML plot.
#'   Defaults to FALSE.
#'
#' @return A plotly subplot grid showing species by DGP heatmaps across years
#'
#' @examples
#' \dontrun{
#' # From data frame
#' grid <- species_dgp_grid(plot_id = "P1", years = c(0, 50, 100), data = spcd_dgp_data)
#'
#' # From file path
#' grid <- species_dgp_grid(plot_id = "P1", data = "plot_spcd_dgp_year_data.csv")
#'
#' # From directory with pattern matching
#' grid <- species_dgp_grid(plot_id = "P1", dir = "output",
#'                         data = "plot_spcd_dgp_year_.*\\.csv")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly layout subplot
#' @importFrom readr read_csv
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @export
species_dgp_grid <- function(plot_id,
                             years = 50,
                             data = NULL,
                             dir = out_dir,
                             metric = c("BA_total", "N_total"),
                             save_html = FALSE) {

  metric <- match.arg(metric)

  # Handle data input: either data frame or file path
  if (is.data.frame(data)) {
    df <- data
  } else if (is.character(data)) {
    file_pattern <- if (!is.null(data)) data else "plot_spcd_dgp_year_.*\\.csv"
    file_path <- latest_file(dir, file_pattern)
    df <- read_csv(file_path, show_col_types = FALSE)
  } else if (is.null(data)) {
    file_path <- latest_file(dir, "plot_spcd_dgp_year_.*\\.csv")
    df <- read_csv(file_path, show_col_types = FALSE)
  } else {
    stop("The 'data' parameter must be either a data frame or a
         character string")
  }

  # Verify required columns exist
  required_cols <- c("PlotID", "Year", "SPCD", "DGP", metric)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter for the specified plot and years
  df_plot <- df %>% filter(PlotID == plot_id, Year %in% years)

  if (nrow(df_plot) == 0) {
    stop("No data found for PlotID: ", plot_id, " in specified years")
  }

  # Create individual heatmaps for each year
  heatmaps <- list()

  for (year in years) {
    year_data <- df_plot %>% filter(Year == year)

    if (nrow(year_data) > 0) {
      hm <- plot_ly(year_data,
                    x = ~factor(DGP),
                    y = ~factor(SPCD),
                    z = ~.data[[metric]],
                    type = "heatmap",
                    colors = "Viridis",
                    colorbar = list(title = metric),
                    hoverinfo = "text",
                    text = ~glue("Species: {SPCD}<br>DGP: {DGP}<br>Year:
                                 {Year}<br>{metric}: {round(.data[[metric]],
                                 2)}"),
                    showscale = (year == years[1]))  %>%   # Only show colorbar for first plot
        plotly::layout(xaxis = list(title = glue("DGP\nYear {year}")),
               yaxis = list(title = "Species Code"))

      heatmaps[[as.character(year)]] <- hm
    }
  }

  # Create subplot grid
  grid <- subplot(heatmaps, nrows = 1, shareY = TRUE, shareX = TRUE)  %>%
    plotly::layout(title = glue("{metric} by Species x DGP: Plot {plot_id}"),
           showlegend = FALSE)

  if (save_html) {
    if (!is.null(dir)) {
      filename <- glue("interactive_{metric}_speciesDGP_grid_plot_{plot_id}.html")
      saveWidget(grid, file.path(dir, filename))
    } else {
      warning("Cannot save HTML file: no directory specified")
    }
  }

  grid
}
