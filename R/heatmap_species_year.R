#' Create interactive heatmap of species by year
#'
#' Generates an interactive heatmap showing species-level metrics (basal area
#' or tree density) across years for a specific plot. Accepts either a data
#' frame or file path.
#'
#' @param plot_id Character or numeric identifier for the plot to visualize
#' @param data Either a data frame or character string specifying file
#' path/pattern.
#'   If character, uses `latest_file()` to find the most recent matching file.
#' @param dir Character string specifying directory path to search for files
#'   (only used if `data` is a character string).
#' @param metric Character string specifying the metric to plot: "BA_total"
#'  (basal area)
#'   or "N_total" (tree density). Defaults to "BA_total".
#' @param save_html Logical indicating whether to save interactive HTML plot.
#'   Defaults to FALSE.
#'
#' @return A plotly heatmap object showing species metrics across years
#'
#' @examples
#' \dontrun{
#' # From data frame
#' hm <- heatmap_species_year_interactive(plot_id = "P1", data = species_data)
#'
#' # From file path
#' hm <- heatmap_species_year_interactive(plot_id = "P1", data =
#' "plot_species_year_data.csv")
#'
#' # From directory with pattern matching
#' hm <- heatmap_species_year_interactive(plot_id = "P1", dir = "output",
#'                                       data = "plot_species_year_.*\\.csv")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly layout
#' @importFrom readr read_csv
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @export
heatmap_species_year <- function(plot_id,
                                 data = NULL,
                                 dir = out_dir,
                                 metric = c("BA_total", "N_total"),
                                 save_html = FALSE) {

  metric <- match.arg(metric)

  # Handle data input: either data frame or file path
  if (is.data.frame(data)) {
    df <- data
  } else if (is.character(data)) {
    # If data is provided as character, use it as file pattern/path
    file_pattern <- if (!is.null(data)) data else "plot_species_year_.*\\.csv"
    file_path <- latest_file(dir, file_pattern)
    df <- read_csv(file_path, show_col_types = FALSE)
  } else if (is.null(data)) {
    # Default behavior: use latest file in directory
    file_path <- latest_file(dir, "plot_species_year_.*\\.csv")
    df <- read_csv(file_path, show_col_types = FALSE)
  } else {
    stop("The 'data' parameter must be either a data frame or a character
         string")
  }

  # Verify required columns exist
  required_cols <- c("PlotID", "Year", "Species Group", metric)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter for the specified plot
  df_plot <- df %>% filter(PlotID == plot_id)

  if (nrow(df_plot) == 0) {
    stop("No data found for PlotID: ", plot_id)
  }

  # Create interactive heatmap
  hm <- plot_ly(df_plot,
                x = ~Year,
                y = ~factor(SpeciesGroup),
                z = ~.data[[metric]],
                type = "heatmap",
                colors = "Viridis",
                hoverinfo = "text",
                text = ~glue("Species Group: {SpeciesGroup}<br>Year: {Year}<br>{metric}:
                             {round(.data[[metric]], 2)}")) %>%
    plotly::layout(title = glue("Heatmap of {metric}: Species Group x Year: Plot {plot_id}"),
           xaxis = list(title = "Year"),
           yaxis = list(title = "Species Group"),
           colorbar = list(title = metric))

  if (save_html) {
    if (!is.null(dir)) {
      filename <- glue("interactive_heatmap_{metric}_plot_{plot_id}.html")
      saveWidget(hm, file.path(dir, filename))
    } else {
      warning("Cannot save HTML file: no directory specified")
    }
  }

  hm
}
