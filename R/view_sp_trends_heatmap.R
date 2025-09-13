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
#' @param save_to Character string specifying directory path to search for files
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
#' hm <- view_sp_trends_heatmap_interactive(plot_id = "P1", data = species_data)
#'
#' # From file path
#' hm <- view_sp_trends_heatmap_interactive(plot_id = "P1", data =
#' "plot_species_year_data.csv")
#'
#' # From directory with pattern matching
#' hm <- view_sp_trends_heatmap_interactive(plot_id = "P1", save_to = "output",
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
view_sp_trends_heatmap <- function(plot_id,
                                 data = NULL,
                                 save_to = NULL,
                                 metric = c("basal", "density"),
                                 save_html = FALSE) {

  metric <- match.arg(metric)

  # Create mapping for both column names and display names
  metric_col <- switch(metric,
                       "basal" = "BA_total",
                       "density" = "N_total"
                       # "recruitment" = "rec_BA_total",
                       # "upgrowth" = "up_BA_total",
                       # "mortality" = "mort_BA_total"
  )

  metric_display <- switch(metric,
                           "basal" = "Basal Area",
                           "density" = "Density"
  )

  metric_bar <- switch(metric,
                           "basal" = "m2/ha",
                           "density" = "TPH"
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
    stop("'data' must be either a data frame or a character string (file path), not ",
         class(data)[1])
  }


  if (!is.null(plot_id)){

    # Filter for the specified plot
    df_plot <- df %>% filter(PlotID == plot_id)

    df_plot <- df_plot |>
      group_by(#PlotID,
               SPCD, SpeciesGroup, Year) |>
      summarise(
        BA_total     = sum(B, na.rm = TRUE),
        N_total      = sum(N, na.rm = TRUE),
        # rec_BA_total = sum(rec_BA,  na.rm = TRUE),
        # up_BA_total  = sum(up_BA,   na.rm = TRUE),
        # mort_BA_total= sum(mort_BA, na.rm = TRUE),
        .groups = "drop"
      )

    # Verify required columns exist
    required_cols <- c("PlotID", "Year", "SpeciesGroup", metric_col)
    missing_cols <- setdiff(required_cols, names(df_plot))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    if (nrow(df_plot) == 0) {
      stop("No data found for PlotID: ", plot_id)
    }

    # Create interactive heatmap
    hm <- plot_ly(df_plot,
                  x = ~Year,
                  y = ~factor(SpeciesGroup),
                  z = ~.data[[metric_col]],
                  type = "heatmap",
                  colors = "viridis",
                  colorbar = list(title = metric_bar),
                  hoverinfo = "text",
                  text = ~glue("Species Group: {SpeciesGroup}<br>Year: {Year}<br>{metric_display}:
                             {round(.data[[metric]], 2)}")) %>%
      plotly::layout(title = list(
                       text = glue("{metric_display}<br><sub>PLOT: {plot_id}</sub>"),
                       x = 0,  # Left align
                       xref = "paper"
                     ),
                     xaxis = list(title = "Year"),
                     yaxis = list(title = "Species Group"))
                     # colorbar = list(title = list(text = "")))
                     # colorbar = list(title = metric_display)
                     # )

    if (save_html) {
      if (!is.null(save_to)) {
        filename <- glue("interactive_heatmap_{metric}_plot_{plot_id}.html")
        saveWidget(hm, file.path(save_to, filename))
      } else {
        filename <- glue("interactive_heatmap_{metric}_plot_{plot_id}.html")
        saveWidget(hm, file.path(getwd(), filename))
      }
    }


  } else {



    df_plot <- df |>
      group_by(#PlotID,
        SPCD, SpeciesGroup, Year) |>
      summarise(
        BA_total     = sum(B, na.rm = TRUE),
        N_total      = sum(N, na.rm = TRUE),
        # rec_BA_total = sum(rec_BA,  na.rm = TRUE),
        # up_BA_total  = sum(up_BA,   na.rm = TRUE),
        # mort_BA_total= sum(mort_BA, na.rm = TRUE),
        .groups = "drop"
      )

    # Verify required columns exist
    required_cols <- c("Year", "SpeciesGroup", metric_col)
    missing_cols <- setdiff(required_cols, names(df_plot))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # if (nrow(df_plot) == 0) {
    #   stop("No data found for PlotID: ", plot_id)
    # }

    # Create interactive heatmap
    hm <- plot_ly(df_plot,
                  x = ~Year,
                  y = ~factor(SpeciesGroup),
                  z = ~.data[[metric_col]],
                  type = "heatmap",
                  colors = "viridis",
                  colorbar = list(title = metric_bar),
                  hoverinfo = "text",
                  text = ~glue("Species Group: {SpeciesGroup}<br>Year: {Year}<br>{metric_display}:
                             {round(.data[[metric_col]], 2)}")) %>%
      plotly::layout(title = glue("{metric_display}"),
                     xaxis = list(title = "Year"),
                     yaxis = list(title = "Species Group"))
                     # colorbar = list(title = list(text = metric_display)))
                     # legend = list(title = list(text = glue("{metric_display}")))
      # )
# layout(legend = list(title = list(text = paste('<b>', title, '</b>'))))


    if (save_html) {
      if (!is.null(save_to)) {
        filename <- glue("interactive_heatmap_{metric}.html")
        saveWidget(hm, file.path(save_to, filename))
      } else {
        filename <- glue("interactive_heatmap_{metric}.html")
        saveWidget(hm, file.path(getwd(), filename))
      }
    }

  }

    hm

}
