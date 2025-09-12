#' Create interactive stacked area chart of DGP metrics
#'
#' Generates an interactive stacked area chart showing diameter growth proportion (DGP)
#' metrics (basal area or tree density) over time for a specific plot.
#'
#' @param plot_id Character or numeric identifier for the plot to visualize
#' @param data Either a data frame or character string specifying file path/pattern.
#'   If character, uses `latest_file()` to find the most recent matching file.
#' @param save_to Character string specifying directory path to search for files
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
#' chart <- stacked_dgp_chart(plot_id = "P1", save_to = "output",
#'                           data = "plot_dgp_year_.*\\.csv")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly layout
#' @importFrom readr read_csv
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @export
stacked_dgp_chart <- function(plot_id,
                              data = NULL,
                              save_to = NULL,
                              metric = c("BA_total", "N_total"),
                              save_html = FALSE) {

  metric <- match.arg(metric)

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
    stop("'data' must be either a data frame or a character string (file path), not ",
         class(data)[1])
  }

  df <- df |>
    group_by(PlotID, DGP, SpeciesGroup, Year) |>
    summarise(
      BA_total     = sum(B, na.rm = TRUE),
      N_total      = sum(N, na.rm = TRUE),
      rec_BA_total = sum(rec_BA,  na.rm = TRUE),
      up_BA_total  = sum(up_BA,   na.rm = TRUE),
      mort_BA_total= sum(mort_BA, na.rm = TRUE),
      .groups = "drop"
    )


  # Verify required columns exist
  required_cols <- c("PlotID", "Year", "DGP", metric)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter for the specified plot
  df_plot <- df  %>%  filter(PlotID == plot_id)

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
    plotly::layout(title = glue("Stacked {metric} by DGP - Plot {plot_id}"),
           yaxis = list(title = metric),
           xaxis = list(title = "Year"),
           showlegend = TRUE,
           legend = list(title = list(text = "DGP")))

  if (save_html) {
    if (!is.null(save_to)) {
      filename <- glue("interactive_stacked_{metric}_byDGP_plot_{plot_id}.html")
      saveWidget(chart, file.path(save_to, filename))
    } else {
      filename <- glue("interactive_stacked_{metric}_byDGP_plot_{plot_id}.html")
      saveWidget(chart, file.path(getwd(), filename))    }
  }

  chart
}
