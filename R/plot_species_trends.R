#' Plot species trends over time using plotly
#'
#' Creates interactive line plots showing species-level trends (basal area or tree density)
#' over time for a specific plot. Accepts either a data frame or file path.
#'
#' @param plot_id Character or numeric identifier for the plot to visualize
#' @param data Either a data frame or character string specifying file path/pattern.
#'   If character, uses `latest_file()` to find the most recent matching file.
#' @param dir Character string specifying directory path to search for files
#'   (only used if `data` is a character string).
#' @param metric Character string specifying the metric to plot: "BA_total" (basal area)
#'   or "N_total" (tree density). Defaults to "BA_total".
#' @param top_n Integer specifying number of top species to display. Defaults to 9.
#' @param save_html Logical indicating whether to save interactive HTML plot.
#'   Defaults to FALSE.
#'
#' @return A plotly object showing species trends over time for the specified plot
#'
#' @examples
#' \dontrun{
#' # From data frame
#' p <- plot_species_trends(plot_id = "P1", data = species_data)
#'
#' # From file path
#' p <- plot_species_trends(plot_id = "P1", data = "plot_species_year_data.csv")
#'
#' # From directory with pattern matching
#' p <- plot_species_trends(plot_id = "P1", dir = "output",
#'                         data = "plot_species_year_.*\\.csv")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly layout
#' @importFrom readr read_csv
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr filter group_by summarise arrange slice_head pull
#' @importFrom glue glue
#' @export
plot_species_trends <- function(plot_id,
                                data = NULL,
                                dir = out_dir,
                                metric = c("BA_total", "N_total"),
                                top_n = 9,
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
    stop("The 'data' parameter must be either a data frame or a character string")
  }

  # Verify required columns exist
  required_cols <- c("PlotID", "Year", "SPCD", metric)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter for the specified plot
  df_plot <- df %>% filter(PlotID == plot_id)

  if (nrow(df_plot) == 0) {
    stop("No data found for PlotID: ", plot_id)
  }

  # Keep top-N species over whole timeline (by total contribution)
  top_sp <- df_plot %>%
    group_by(SPCD) %>%
    summarise(tot = sum(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(tot)) %>%
    slice_head(n = top_n) %>%
    pull(SPCD)

  dplot <- df_plot %>% filter(SPCD %in% top_sp)

  # Create interactive plot
  p <- plot_ly(dplot, x = ~Year, y = ~.data[[metric]], color = ~factor(SPCD),
               type = 'scatter', mode = 'lines',
               line = list(width = 2), hoverinfo = 'text',
               text = ~glue("Species: {SPCD}<br>Year: {Year}<br>{metric}: {round(.data[[metric]], 2)}")) %>%
    layout(title = glue("{metric} by Species across Years: Plot {plot_id}"),
           yaxis = list(title = metric),
           xaxis = list(title = "Year"),
           showlegend = TRUE,
           legend = list(title = list(text = "Species Code")))

  if (save_html) {
    if (!is.null(dir)) {
      filename <- glue("interactive_{metric}_species_plot_{plot_id}.html")
      saveWidget(p, file.path(dir, filename))
    } else {
      warning("Cannot save HTML file: no directory specified")
    }
  }

  p
}
