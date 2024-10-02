# read results
df <- readRDS("simulation/combi_model/simulation_results.rds")

# plot colors for different number of beeps
cols <- c("#D81B60", "#1E88E5", "#FFC107")

# only autoregressive and cross(-lagged) parameters
df_plot <- subset(df, par %in% c("ar_mf", "ar_m", "ar_night_m",
                                 "cr_mf_s", "cr_m_s",
                                 "ar_s", "cr_s_mf", "cr_s_m"))

# make long format data frame for plotting with factors and nice labels
df_plot$beeps <- as.factor(df_plot$beeps)
df_plot$days <- as.factor(df_plot$days)

df_plot_long <- tidyr::pivot_longer(df_plot, cols = 2:4,
                                    names_to = "diagnostic",
                                    values_to = "value")
df_plot_long$diagnostic <- factor(df_plot_long$diagnostic,
                                  levels = c("bias", "mae", "coverage"),
                                  labels = c("Bias", "MAE", "Coverage"))

df_plot_long$par <- factor(df_plot_long$par,
                           levels = c("ar_mf", "ar_m", "ar_night_m",
                                      "cr_mf_s", "cr_m_s",
                                      "ar_s", "cr_s_mf", "cr_s_m"),
                           labels = c(expression(phi["MF"]),
                                      expression(phi["M"]),
                                      expression(gamma["M"]),
                                      expression(beta["MF"]),
                                      expression(delta["M"]),
                                      expression(phi[S]),
                                      expression(beta["S"]),
                                      expression(delta["S"])))

# axis limits for each facet
df_axis <- data.frame(diagnostic = unique(df_plot_long$diagnostic),
                      ymin = c(-0.3, 0, .85), ymax = c(0.3, 1.2, 1),
                      hline = c(0, 0, .95))

# axis ticks for each facet
yBreaks <- function(x) {
  if (mean(x) < 0.1) {
    seq(-0.3, 0.3, 0.15)
  } else if (mean(x) > 0.9) {
    seq(.85, 1, 0.05)
  } else {
    seq(0, 1.2, 0.3)
  }
}

# plot bias, MAE, and coverage
p <- ggplot2::ggplot(df_plot_long) +
  ggplot2::geom_hline(data = df_axis, ggplot2::aes(yintercept = hline), linewidth = 0.5, alpha = 0.2) +
  ggplot2::geom_line(ggplot2::aes(x = days, y = value, colour = beeps, group = interaction(beeps, software), linetype = software), linewidth = 1) +
  ggplot2::geom_point(ggplot2::aes(x = days, y = value, colour = beeps, fill = software, shape = software), size = 2, stroke = 1) +
  ggplot2::scale_shape_manual(values = c(19, 21)) +
  ggplot2::scale_fill_manual(values = c("transparent", "white")) +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::facet_grid(cols = ggplot2::vars(par), rows = ggplot2::vars(diagnostic), scales = "free_y", labeller = ggplot2::label_parsed) +
  ggplot2::labs(x = "Days", colour = "Beeps", fill = "Software", shape = "Software", linetype = "Software") +
  ggplot2::theme_void() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "sans", size = 12),
    axis.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(size = 12),
    axis.text = ggplot2::element_text(margin = ggplot2::margin(5, 5, 5, 5)),
    axis.text.y = ggplot2::element_text(hjust = 1),
    axis.ticks = ggplot2::element_line(lineend = "butt", linewidth = 0.5),
    axis.ticks.length = ggplot2::unit(2.5, "pt"),
    strip.text = ggplot2::element_text(margin = ggplot2::margin(0, 5, 0, 5), size = 12),
    strip.text.y = ggplot2::element_text(angle = 270),
    legend.position = "bottom",
    panel.spacing = ggplot2::unit(7.5, units = "pt")
  ) +
  ggplot2::guides(linetype = ggplot2::guide_legend(override.aes = list(size = 1.5, linewidth = 0.5))) +
  ggplot2::scale_y_continuous(breaks = yBreaks) +
  ggplot2::geom_segment(data = df_axis,
                        ggplot2::aes(x = -Inf, xend = -Inf, y = ymin, yend = ymax),
                        linewidth = 0.3, lineend = "square") +
  ggplot2::geom_segment(x = 1, xend = 4, y = -Inf, yend = -Inf,
                        linewidth = 0.3, lineend = "square")

# save plot
ggplot2::ggsave("simulation/combi_model/parameter_recovery.pdf", p,
                width = 10, height = 4)
