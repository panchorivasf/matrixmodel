plot_totals <- function(dir = out_dir,
                                 file = latest_file(dir, "summary_results__.*\\.csv"),
                                 save_png = TRUE) {
  df <- read_csv(file, show_col_types = FALSE)

  p1 <- ggplot(df, aes(Year, BA_total, color = PlotID, group = PlotID)) +
    geom_line(linewidth = 0.6, alpha = 0.8) +
    labs(title = "Basal Area (BA) over time", y = "BA (m2/ha)", x = "Year") +
    theme_minimal(base_family = "Times New Roman") +
    theme(legend.position = "none")

  p2 <- ggplot(df, aes(Year, N_total, color = PlotID, group = PlotID)) +
    geom_line(linewidth = 0.6, alpha = 0.8) +
    labs(title = "Tree Density (N) over time", y = "TPH", x = "Year") +
    theme_minimal(base_family = "Times New Roman") +
    theme(legend.position = "none")

  if (save_png) {
    ggsave(file.path(dir, "quick_BA_total.png"), p1, width = 8, height = 4.5, dpi = 220)
    ggsave(file.path(dir, "quick_N_total.png"),  p2, width = 8, height = 4.5, dpi = 220)
  }
  list(ba = p1, n = p2)
}
