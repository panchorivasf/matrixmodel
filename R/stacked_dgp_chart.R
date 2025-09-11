#' Create interactive stacked area chart of DGP metrics
#'
#' Generates an interactive stacked area chart showing diameter growth proportion (DGP)
#' metrics (basal area or tree density) over time for a specific plot.
#'
#' @param plot_id Character or numeric identifier for the plot to visualize
#' @param data Either a data frame or character string specifying file path/pattern.
#'   If character, uses `latest_file()` to find the most recent matching file.
#' @param dir Character string specifying directory path to search for files
#'   (only used if `data` is a character string).
#' @param metric Character string specifying the metric to plot: "BA_total" (basal area)
#'   or "N_total" (tree density). Defaults to "BA_total".
#' @param save_html Logical indicating whether to save interactive HTML plot.
#'   Defaults to FALSE.
#'
#' @return A plotly stacked area chart showing DGP metrics over time
#'
#' @examples
#' \dontrun{
#' # From data frame
#' chart <- stacked_dgp_chart(plot_id = "P1", data = dgp_data)
#'
#' # From file path
#' chart <- stacked_dgp_chart(plot_id = "P1", data = "plot_dgp_year_data.csv")
#'
#' # From directory with pattern matching
#' chart <- stacked_dgp_chart(plot_id = "P1", dir = "output",
#'                           data = "plot_dgp_year_.*\\.csv")
#' }
#'
#' @importFrom plotly plot_ly layout
#' @importFrom readr read_csv
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @export
stacked_dgp_chart <- function(plot_id,
                              data = NULL,
                              dir = out_dir,
                              metric = c("BA_total", "N_total"),
                              save_html = FALSE) {

  metric <- match.arg(metric)

  # Handle data input: either data frame or file path
  if (is.data.frame(data)) {
    df <- data
  } else if (is.character(data)) {
    file_pattern <- if (!is.null(data)) data else "plot_dgp_year_.*\\.csv"
    file_path <- latest_file(dir, file_pattern)
    df <- read_csv(file_path, show_col_types = FALSE)
  } else if (is.null(data)) {
    file_path <- latest_file(dir, "plot_dgp_year_.*\\.csv")
    df <- read_csv(file_path, show_col_types = FALSE)
  } else {
    stop("The 'data' parameter must be either a data frame or a character string")
  }

  # Verify required columns exist
  required_cols <- c("PlotID", "Year", "DGP", metric)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter for the specified plot
  df_plot <- df |> filter(PlotID == plot_id)

  if (nrow(df_plot) == 0) {
    stop("No data found for PlotID: ", plot_id)
  }

  # Create interactive stacked area chart
  chart <- plot_ly(df_plot,
                   x = ~Year,
                   y = ~.data[[metric]],
                   color = ~factor(DGP),
                   type = 'scatter',
                   mode = 'none',
                   stackgroup = 'one',
                   fill = 'tonexty',
                   hoverinfo = 'text',
                   text = ~glue("DGP: {DGP}<br>Year: {Year}<br>{metric}: {round(.data[[metric]], 2)}")) %>%
    layout(title = glue("Stacked {metric} by DGP - Plot {plot_id}"),
           yaxis = list(title = metric),
           xaxis = list(title = "Year"),
           showlegend = TRUE,
           legend = list(title = list(text = "DGP")))

  if (save_html) {
    if (!is.null(dir)) {
      filename <- glue("interactive_stacked_{metric}_byDGP_plot_{plot_id}.html")
      saveWidget(chart, file.path(dir, filename))
    } else {
      warning("Cannot save HTML file: no directory specified")
    }
  }

  chart
}
