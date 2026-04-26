# 02_event_study_plots.R
# Event study visualizations for report.Rmd
# Requires: 00_load_data.R sourced first

library(tidyverse)
library(scales)

# ---- 1. Average post-storm ZHVI trajectory by resilience quartile ----
# Shows the average indexed deviation from regional baseline at each month T-6 to T+12
# for high resilience (Q4) vs low resilience (Q1) counties

fig_event_study <- monthly_dev |>
  filter(resl_quartile %in% c("Low (Q1)", "High (Q4)")) |>
  group_by(resl_quartile, month_t) |>
  summarise(
    mean_dev  = mean(deviation, na.rm = TRUE),
    se_dev    = sd(deviation, na.rm = TRUE) / sqrt(n()),
    .groups   = "drop"
  ) |>
  ggplot(aes(x = month_t, y = mean_dev, color = resl_quartile,
             fill = resl_quartile)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.5) +
  geom_ribbon(aes(ymin = mean_dev - 1.96 * se_dev,
                  ymax = mean_dev + 1.96 * se_dev),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = c(-6:-1, 0, 1:12),
    labels = c(paste0("T", -6:-1), "T0", paste0("T+", 1:12))
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_manual(values = c("Low (Q1)" = "#d62728", "High (Q4)" = "#1f77b4")) +
  scale_fill_manual(values  = c("Low (Q1)" = "#d62728", "High (Q4)" = "#1f77b4")) +
  annotate("text", x = -0.3, y = Inf, label = "Storm",
           hjust = 1, vjust = 1.5, size = 3, color = "grey40", fontface = "italic") +
  labs(
    title    = "Post-Storm ZHVI Trajectory by Resilience Quartile",
    subtitle = "Mean monthly indexed deviation from regional baseline. Shaded band = 95% CI.",
    x        = "Months Post-Storm",
    y        = "Mean Indexed Deviation (index points)",
    color    = "Resilience",
    fill     = "Resilience"
  ) +
  theme_minimal() +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 7, angle = 45, hjust = 1)
  )

# ---- 2. CIR distribution by resilience quartile (box plot) ----
fig_auc_by_quartile <- df |>
  filter(resl_quartile %in% c("Low (Q1)", "High (Q4)")) |>
  ggplot(aes(x = resl_quartile, y = auc, fill = resl_quartile)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  coord_cartesian(ylim = quantile(df$auc, c(0.05, 0.95), na.rm = TRUE)) +
  scale_fill_manual(values = c("Low (Q1)" = "#d62728", "High (Q4)" = "#1f77b4")) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title    = "Post-Storm CIR by Resilience Quartile",
    subtitle = "Outliers trimmed to 5th-95th percentile for display. Dashed line = zero deviation.",
    x        = "Resilience Quartile",
    y        = "CIR (index points)",
    fill     = "Resilience"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 3. Cumulative CIR trajectory by resilience quartile ----
# Shows how the gap between high and low resilience counties accumulates over time

fig_cumulative_auc <- monthly_dev |>
  filter(resl_quartile %in% c("Low (Q1)", "High (Q4)"),
         month_t >= 1) |>
  group_by(resl_quartile, month_t) |>
  summarise(mean_dev = mean(deviation, na.rm = TRUE), .groups = "drop") |>
  group_by(resl_quartile) |>
  mutate(cumulative_dev = cumsum(mean_dev)) |>
  ggplot(aes(x = month_t, y = cumulative_dev, color = resl_quartile)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = paste0("T+", 1:12)) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  scale_color_manual(values = c("Low (Q1)" = "#d62728", "High (Q4)" = "#1f77b4")) +
  labs(
    title    = "Cumulative CIR Post-Storm by Resilience Quartile",
    subtitle = "Cumulative sum of mean monthly indexed deviations from regional baseline.",
    x        = "Months Post-Storm",
    y        = "Cumulative Indexed Deviation (index points)",
    color    = "Resilience"
  ) +
  theme_minimal() +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

# ---- Save figures ----
dir.create("./output/figures", recursive = TRUE, showWarnings = FALSE)

ggsave("./output/figures/fig_event_study.pdf",     fig_event_study,
       width = 8, height = 5)
ggsave("./output/figures/fig_auc_by_quartile.pdf", fig_auc_by_quartile,
       width = 6, height = 5)
ggsave("./output/figures/fig_cumulative_auc.pdf",  fig_cumulative_auc,
       width = 8, height = 5)

cat("Event study figures saved to ./output/figures/\n")
