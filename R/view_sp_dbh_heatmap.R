#' Create interactive faceted heatmap grid of species by DGP
#'
#' Generates an interactive grid of heatmaps showing species-level metrics by DGP
#' across multiple year for a specific plot.
#'
#' @param plot_id Character or numeric identifier for the plot to visualize
#' @param  Numeric vector of year to include in the faceted grid
#' @param data Either a data frame or character string specifying file path/pattern.
#'   If character, uses `latest_file()` to find the most recent matching file.
#' @param save_to Character string specifying directory path to search for files
#'   (only used if `data` is a character string).
#' @param metric Character string specifying the metric to plot: "BA_total" (basal area)
#'   or "N_total" (tree density). Defaults to "BA_total".
#' @param save_html Logical indicating whether to save interactive HTML plot.
#'   Defaults to FALSE.
#'
#' @return A plotly subplot grid showing species by DGP heatmaps for a year
#'
#' @examples
#' \dontrun{
#' # From data frame
#' grid <- view_sp_dbh_heatmap(plot_id = "P1", year = c(0, 50, 100), data = spcd_dgp_data)
#'
#' # From file path
#' grid <- view_sp_dbh_heatmap(plot_id = "P1", data = "plot_spcd_dgp_year_data.csv")
#'
#' # From directory with pattern matching
#' grid <- view_sp_dbh_heatmap(plot_id = "P1", save_to = "output",
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
view_sp_dbh_heatmap <- function(plot_id,
                                year = 50,
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
                           "density" = "Density (TPH)"
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

    df <- df |>
      group_by(PlotID, SPCD, SpeciesGroup, DGP, Year) |>
      summarise(
        BA_total     = sum(B, na.rm = TRUE),
        N_total      = sum(N, na.rm = TRUE),
        # rec_BA_total = sum(rec_BA,  na.rm = TRUE),
        # up_BA_total  = sum(up_BA,   na.rm = TRUE),
        # mort_BA_total= sum(mort_BA, na.rm = TRUE),
        .groups = "drop"
      )

    # Verify required columns exist
    required_cols <- c("PlotID", "Year", "SpeciesGroup", "DGP", metric_col)
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Filter for the specified plot and year
    df_plot <- df %>% filter(PlotID == plot_id, Year %in% year)

    if (nrow(df_plot) == 0) {
      stop("No data found for PlotID: ", plot_id, " in specified year")
    }

    # Create individual heatmaps for each year
    heatmaps <- list()

    for (year in year) {
      year_data <- df_plot %>% filter(Year == year)

      if (nrow(year_data) > 0) {
        hm <- plot_ly(year_data,
                      x = ~factor(DGP),
                      y = ~factor(SpeciesGroup),
                      z = ~.data[[metric_col]],
                      type = "heatmap",
                      colors = "viridis",
                      colorbar = list(title = metric_bar),
                      hoverinfo = "text",
                      text = ~glue("Species Group: {SpeciesGroup}<br>DGP: {DGP}<br>{metric_display}: {round(.data[[metric_col]],
                                 2)}"),
                      showscale = (year == year[1]))  %>%   # Only show colorbar for first plot
          plotly::layout(xaxis = list(title = glue("DBH Group")),
                         yaxis = list(title = "Species Group"),
                         )

        heatmaps[[as.character(year)]] <- hm
      }
    }

    # Create subplot grid
    grid <- subplot(heatmaps, nrows = 1, shareY = TRUE, shareX = TRUE)  %>%
      plotly::layout(title = glue("{metric_display} by year {year} / Plot {plot_id}"),
                     showlegend = FALSE,
                     margin = list(l = 80, r = 80, t = 80, b = 80))

    if (save_html) {
      if (!is.null(save_to)) {
        filename <- glue("{metric_display}_speciesDGP_grid_plot_{plot_id}.html")
        saveWidget(grid, file.path(save_to, filename))
      } else {
        filename <- glue("{metric_display}_speciesDGP_grid_plot_{plot_id}.html")
        saveWidget(grid, file.path(getwd(), filename))    }
    }

  } else {

    df <- df |>
      group_by(SPCD, SpeciesGroup, DGP, Year) |>
      summarise(
        BA_total     = sum(B, na.rm = TRUE),
        N_total      = sum(N, na.rm = TRUE),
        # rec_BA_total = sum(rec_BA,  na.rm = TRUE),
        # up_BA_total  = sum(up_BA,   na.rm = TRUE),
        # mort_BA_total= sum(mort_BA, na.rm = TRUE),
        .groups = "drop"
      )

    # Verify required columns exist
    required_cols <- c("Year", "SpeciesGroup", "DGP", metric_col)
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Create individual heatmaps for each year
    heatmaps <- list()

    for (year in year) {
      year_data <- df %>% filter(Year == year)

      if (nrow(year_data) > 0) {
        hm <- plot_ly(year_data,
                      x = ~factor(DGP),
                      y = ~factor(SpeciesGroup),
                      z = ~.data[[metric_col]],
                      type = "heatmap",
                      colors = "viridis",
                      colorbar = list(title = metric_bar),
                      hoverinfo = "text",
                      text = ~glue("Species Group: {SpeciesGroup}<br>DGP: {DGP}<br>{metric_display}: {round(.data[[metric_col]],
                                 2)}"),
                      showscale = (year == year[1]))  %>%   # Only show colorbar for first plot
          plotly::layout(xaxis = list(title = glue("DBH Group")),
                         yaxis = list(title = "Species Group"))

        heatmaps[[as.character(year)]] <- hm
      }
    }

    # Create subplot grid
    grid <- subplot(heatmaps, nrows = 1, shareY = TRUE, shareX = TRUE)  %>%
      plotly::layout(title = glue("{metric_display} by year {year} (All plots)"),
                     showlegend = FALSE,
                     margin = list(l = 80, r = 80, t = 80, b = 80))

    if (save_html) {
      if (!is.null(save_to)) {
        filename <- glue("{metric_display}_speciesDGP_grid_all_plots.html")
        saveWidget(grid, file.path(save_to, filename))
      } else {
        filename <- glue("{metric_display}_speciesDGP_grid_all_plots.html")
        saveWidget(grid, file.path(getwd(), filename))    }
    }


  }



  grid
}
