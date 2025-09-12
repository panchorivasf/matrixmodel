#' Plot trend of forest metrics over time
#'
#' Creates an interactive line plot showing the trend of a selected forest metric
#' across years. Can display data for a specific plot or aggregated across all plots.
#'
#' @param plot_id Character or numeric. Plot identifier to filter data. If NULL,
#'                aggregates data across all plots. Default: NULL
#' @param data Character or data.frame. Either a data frame or file path to CSV data.
#' @param save_to Character. Directory path to save output files. If NULL, uses working directory.
#' @param metric Character. Metric to visualize: "basal_area", "density", "recruitment",
#'               "upgrowth", or "mortality". Default: "basal_area"
#' @param save_html Logical. Whether to save interactive HTML version. Default: FALSE
#'
#' @return A Plotly object displaying the trend over time
#'
#' @importFrom plotly plot_ly layout
#' @importFrom htmlwidgets saveWidget
#' @importFrom readr read_csv
#' @importFrom dplyr filter group_by summarise
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # Plot basal area trend for specific plot
#' plot_trend_by_variable("Plot_1", data = "forest_data.csv")
#'
#' # Plot density trend across all plots
#' plot_trend_by_variable(data = "forest_data.csv", metric = "density")
#' }
#'
#' @export
plot_trend_by_variable <- function(plot_id = NULL,
                       data = NULL,
                       save_to = NULL,
                       metric = c("basal_area",
                                  "density",
                                  "recruitment",
                                  "upgrowth",
                                  "mortality"),
                       save_html = FALSE) {

  metric <- match.arg(metric)

  # Create mapping for both column names and display names
  metric_col <- switch(metric,
                       "basal_area" = "BA_total",
                       "density" = "N_total",
                       "recruitment" = "rec_BA_total",
                       "upgrowth" = "up_BA_total",
                       "mortality" = "mort_BA_total"
  )

  metric_display <- switch(metric,
                           "basal_area" = "Basal Area",
                           "density" = "Density (TPH)",
                           "recruitment" = "Recruitment",
                           "upgrowth" = "Upgrowth",
                           "mortality" = "Mortality"
  )

  # Handle data input: either data frame or file path
  if (is.null(data)) {
    stop("'data' must be either a data frame or the name of a csv file
        in the working directory. If the file is not in the working
        directory, provide the full path.")
  } else if (is.data.frame(data)) {
    df <- data
  } else if (is.character(data)) {
    if (length(data) != 1) {
      stop("File path must be a single character string")
    }
    df <- read_csv(data, show_col_types = FALSE)
  } else {
    stop("'data' must be either a data frame or a character string
          (file path), not ", class(data)[1])
  }

  if (!is.null(plot_id)){
    df <- df |>
      filter(PlotID == plot_id)
  }

  df2 <- df |>
    group_by(Year) |>
    summarise(
      BA_total     = sum(B, na.rm = TRUE),
      N_total      = sum(N, na.rm = TRUE),
      rec_BA_total = sum(rec_BA,  na.rm = TRUE),
      up_BA_total  = sum(up_BA,   na.rm = TRUE),
      mort_BA_total = sum(mort_BA, na.rm = TRUE),
      .groups = "drop"
    )

  # Create interactive plot
  p <- plot_ly(df2, x = ~Year, y = ~.data[[metric_col]],
               type = 'scatter', mode = 'lines',
               line = list(width = 2), hoverinfo = 'text',
               showlegend = FALSE,
               text = ~glue("Year: {Year}<br>{metric_display}: {round(.data[[metric_col]], 2)}")) %>%
    layout(title = glue("{metric_display} across Years"),
           yaxis = list(title = metric_display),
           xaxis = list(title = "Year"),
           showlegend = TRUE)

  if (save_html) {
    if (!is.null(save_to)) {
      filename <- glue("interactive_{metric}_trend_plot_{ifelse(is.null(plot_id), 'all', plot_id)}.html")
      saveWidget(p, file.path(save_to, filename))
    } else {
      filename <- glue("interactive_{metric}_trend_plot_{ifelse(is.null(plot_id), 'all', plot_id)}.html")
      saveWidget(p, file.path(getwd(), filename))
    }
  }

  p
}
