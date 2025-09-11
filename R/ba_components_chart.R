#' Create interactive stacked bar chart of BA components
#'
#' Generates an interactive stacked bar chart showing basal area components
#' (recruitment, upgrowth, mortality) over time for a specific plot.
#'
#' @param plot_id Character or numeric identifier for the plot to visualize
#' @param data Either a data frame or character string specifying file
#' path/pattern.
#'   If character, uses `latest_file()` to find the most recent matching file.
#' @param dir Character string specifying directory path to search for files
#'   (only used if `data` is a character string).
#' @param save_html Logical indicating whether to save interactive HTML plot.
#'   Defaults to FALSE.
#'
#' @return A plotly stacked bar chart showing BA components over time
#'
#' @examples
#' \dontrun{
#' # From data frame
#' chart <- ba_components_chart(plot_id = "P1", data = summary_data)
#'
#' # From file path
#' chart <- ba_components_chart(plot_id = "P1", data =
#' "summary_results_data.csv")
#'
#' # From directory with pattern matching
#' chart <- ba_components_chart(plot_id = "P1", dir = "output",
#'                             data = "summary_results__.*\\.csv")
#' }
#'
#' @importFrom plotly plot_ly layout
#' @importFrom readr read_csv
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr filter select mutate case_when
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#' @export
ba_components_chart <- function(plot_id,
                                data = NULL,
                                dir = out_dir,
                                save_html = FALSE) {

  # Handle data input: either data frame or file path
  if (is.data.frame(data)) {
    df <- data
  } else if (is.character(data)) {
    file_pattern <- if (!is.null(data)) data else "summary_results__.*\\.csv"
    file_path <- latest_file(dir, file_pattern)
    df <- read_csv(file_path, show_col_types = FALSE)
  } else if (is.null(data)) {
    file_path <- latest_file(dir, "summary_results__.*\\.csv")
    df <- read_csv(file_path, show_col_types = FALSE)
  } else {
    stop("The 'data' parameter must be either a data frame or a
         character string")
  }

  # Verify required columns exist
  required_cols <- c("PlotID", "Year", "rec_BA_total", "up_BA_total",
                     "mort_BA_total")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter for the specified plot and prepare data
  df_plot <- df |>
    filter(PlotID == plot_id) |>
    select(PlotID, Year, rec_BA_total, up_BA_total, mort_BA_total) |>
    pivot_longer(-c(PlotID, Year), names_to = "component", values_to = "BA")

  if (nrow(df_plot) == 0) {
    stop("No data found for PlotID: ", plot_id)
  }

  # Clean up component names for display
  df_plot <- df_plot |>
    mutate(component = case_when(
      component == "rec_BA_total" ~ "Recruitment",
      component == "up_BA_total" ~ "Upgrowth",
      component == "mort_BA_total" ~ "Mortality",
      TRUE ~ component
    ))

  # Create interactive stacked bar chart
  chart <- plot_ly(df_plot,
                   x = ~Year,
                   y = ~BA,
                   color = ~component,
                   type = 'bar',
                   hoverinfo = 'text',
                   text = ~glue("Component: {component}<br>Year: {Year}<br>BA:
                                {round(BA, 2)} m2/ha")) |>
    layout(title = glue("BA Components over Time: Plot {plot_id}"),
           yaxis = list(title = "Basal Area (m2/ha)"),
           xaxis = list(title = "Year"),
           barmode = 'stack',
           showlegend = TRUE,
           legend = list(title = list(text = "Component")))

  if (save_html) {
    if (!is.null(dir)) {
      filename <- glue("interactive_components_BA_plot_{plot_id}.html")
      saveWidget(chart, file.path(dir, filename))
    } else {
      warning("Cannot save HTML file: no directory specified")
    }
  }

  chart
}
