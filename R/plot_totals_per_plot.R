#' Plot basal area and tree density trends using plotly
#'
#' Creates interactive line plots showing basal area (BA) and tree density (N)
#' trends over time for multiple plots. Accepts either a data frame or path to
#' CSV file.
#'
#' @param data Either a data frame or character string specifying file
#' path/pattern.
#'   If character, uses `latest_file()` to find the most recent matching file.
#' @param dir Character string specifying directory path to search for files
#'   (only used if `data` is a character string).
#' @param save_html Logical indicating whether to save interactive HTML plots.
#'   Defaults to FALSE.
#'
#' @return A list containing two plotly objects:
#' \itemize{
#'   \item \code{ba} - Interactive plot of basal area over time
#'   \item \code{n} - Interactive plot of tree density over time
#' }
#'
#' @examples
#' \dontrun{
#' # From file path
#' plots <- plot_totals_per_plot_interactive("summary_results_2023.csv")
#'
#' # From data frame
#' plots <- plot_totals_per_plot_interactive(my_dataframe)
#'
#' # From directory with pattern matching
#' plots <- plot_totals_per_plot_interactive(dir = "output",
#'                                          data = "summary_results__.*\\.csv")
#' }
#'
#' @importFrom plotly plot_ly layout
#' @importFrom readr read_csv
#' @importFrom htmlwidgets saveWidget
#' @export
plot_totals_per_plot <- function(data = NULL,
                                 dir = out_dir,
                                 save_html = FALSE) {

  # Handle data input: either data frame or file path
  if (is.data.frame(data)) {
    df <- data
  } else if (is.character(data)) {
    # If data is provided as character, use it as file pattern/path
    file <- if (!is.null(data)) data else "summary_results__.*\\.csv"
    file_path <- latest_file(dir, file)
    df <- read_csv(file_path, show_col_types = FALSE)
  } else if (is.null(data)) {
    # Default behavior: use latest file in directory
    file_path <- latest_file(dir, "summary_results__.*\\.csv")
    df <- read_csv(file_path, show_col_types = FALSE)
  } else {
    stop("The 'data' parameter must be either a data frame or a character
         string")
  }

  # Verify required columns exist
  required_cols <- c("Year", "BA_total", "N_total", "PlotID")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Create basal area plot
  p1 <- plot_ly(df, x = ~Year, y = ~BA_total, color = ~PlotID, type = 'scatter',
                mode = 'lines',
                line = list(width = 2, opacity = 0.8), hoverinfo = 'text',
                text = ~paste('Plot:', PlotID, '<br>Year:', Year, '<br>BA:',
                              round(BA_total, 2), 'm2/ha')) |>
    layout(title = "Basal Area (BA) over time",
           yaxis = list(title = "BA (m2/ha)"),
           xaxis = list(title = "Year"),
           showlegend = FALSE)

  # Create tree density plot
  p2 <- plot_ly(df, x = ~Year, y = ~N_total, color = ~PlotID, type = 'scatter',
                mode = 'lines',
                line = list(width = 2, opacity = 0.8), hoverinfo = 'text',
                text = ~paste('Plot:', PlotID, '<br>Year:', Year, '<br>N:',
                              round(N_total, 2), 'TPH')) |>
    layout(title = "Tree Density (N) over time",
           yaxis = list(title = "TPH"),
           xaxis = list(title = "Year"),
           showlegend = FALSE)

  if (save_html) {
    if (!is.null(dir)) {
      saveWidget(p1, file.path(dir, "interactive_BA_total.html"))
      saveWidget(p2, file.path(dir, "interactive_N_total.html"))
    } else {
      warning("Cannot save HTML files: no directory specified")
    }
  }

  list(ba = p1, n = p2)
}
