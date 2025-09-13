#' Create interactive species-DGP heatmap grid
#'
#' Generates an interactive heatmap grid showing species distribution across DGP
#' classes for multiple years using Plotly.
#'
#' @param plot_id Character or numeric. Plot identifier to filter data.
#' @param years Numeric vector. Years to include in the analysis.
#'              Default: c(0, 50, 100, 200, 300)
#' @param save_to Character. Directory path to save output files. If NULL,
#'                uses working directory.
#' @param data Character or data.frame. Either a data frame or file path to
#'             CSV data. Required.
#' @param metric Character. Metric to visualize: "BA_total" or "N_total".
#'               Default: "BA_total"
#' @param save_png Logical. Whether to save PNG version. Default: TRUE
#' @param save_html Logical. Whether to save interactive HTML version.
#'                  Default: TRUE
#'
#' @return A Plotly object displaying the heatmap grid
#'
#' @importFrom plotly plot_ly add_heatmap layout subplot plotly_empty save_image
#' @importFrom htmlwidgets saveWidget
#' @importFrom readr read_csv
#' @importFrom dplyr filter
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' species_dgp("Plot_1", data = "species_data.csv")
#' }
#' @keywords internal
#' @noRd
species_dgp <- function(plot_id,
                        years = c(0, 50, 100, 200, 300),
                        save_to = NULL,
                        data = NULL,
                        metric = c("BA_total","N_total"),
                        save_png = TRUE,
                        save_html = TRUE) {
  metric <- match.arg(metric)

  # Handle data input
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
    group_by(PlotID, SPCD, SpeciesGroup, DGP, Year) |>
    summarise(
      BA_total     = sum(B, na.rm = TRUE),
      N_total      = sum(N, na.rm = TRUE),
      rec_BA_total = sum(rec_BA,  na.rm = TRUE),
      up_BA_total  = sum(up_BA,   na.rm = TRUE),
      mort_BA_total= sum(mort_BA, na.rm = TRUE),
      .groups = "drop"
    )


  # Filter data
  df <- df |>
    filter(PlotID == plot_id, Year %in% years)

  # Create Plotly heatmap
  p <- plot_ly() |>
    add_heatmap(
      data = df,
      x = ~factor(DGP),
      y = ~factor(SPCD),
      z = ~.data[[metric]],
      colorbar = list(title = metric),
      hovertemplate = paste(
        "DGP: %{x}<br>",
        "SPCD: %{y}<br>",
        metric, ": %{z}<br>",
        "Year: %{customdata}<extra></extra>"
      ),
      customdata = ~Year
    ) |>
    layout(
      title = list(
        text = glue("{metric} by Species Group x DGP: Plot {plot_id}"),
        x = 0.5
      ),
      xaxis = list(title = "DGP"),
      yaxis = list(title = "SPCD")
    ) |>
    subplot(
      plotly_empty(type = "scatter", mode = "markers"),
      nrows = 1,
      widths = c(0.1, 0.9)
    ) |>
    layout(
      annotations = list(
        list(
          x = 0.5,
          y = 1.05,
          text = "",
          showarrow = FALSE,
          xref = "paper",
          yref = "paper"
        )
      )
    )

  # Save files
  if (save_html) {
    output_dir <- if (!is.null(save_to)) save_to else getwd()
    base_name <- glue("quick_{metric}_speciesDGP_grid_plot_{plot_id}")

    if (save_html) {
      html_file <- file.path(output_dir, paste0(base_name, ".html"))
      saveWidget(p, html_file, selfcontained = TRUE)
    }


  }

  return(p)
}
