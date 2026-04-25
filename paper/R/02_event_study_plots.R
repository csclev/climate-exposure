# 02_event_study_plots.R
# Event study visualizations for report.Rmd
# Requires: 00_load_data.R sourced first

library(tidyverse)
library(scales)


# ---- 1. Average post-storm ZHVI trajectory by resilience quartile ----
# Shows the average indexed deviation from regional baseline at each month T-6 to T+12
# for high resilience (Q4) vs low resilience (Q1) counties

fig_event_study <- monthly_dev |>
  filter(resl_quartile %in% c("Low", "High")) |>
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
    breaks = c(-PRE_EVENT_MONTHS:-1, 0, 1:POST_EVENT_MONTHS),
    labels = c(paste0("T", -PRE_EVENT_MONTHS:-1), "T0", paste0("T+", 1:POST_EVENT_MONTHS))
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_manual(values = c("Low" = "#d62728", "High" = "#1f77b4")) +
  scale_fill_manual(values  = c("Low" = "#d62728", "High" = "#1f77b4")) +
  annotate("text", x = -0.3, y = Inf, label = "Storm",
           hjust = 1, vjust = 1.5, size = 3, color = "grey40", fontface = "italic") +
  labs(
    title    = "Post-Storm ZHVI Trajectory by Resilience Quartile",
    subtitle = "Mean monthly indexed deviation from neighbor baseline. Shaded band = 95% CI.",
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


# ---- 2. Average post-storm ZHVI trajectory by Event Type ----
fig_event_study_by_type <- monthly_dev |>
  pivot_longer(
    cols      = c(evt_wind, evt_tornado, evt_hail, evt_flood, evt_other),
    names_to  = "event_category",
    values_to = "flag"
  ) |>
  filter(flag == 1) |>
  mutate(event_category = recode(event_category,
                                 "evt_wind"    = "Wind",
                                 "evt_tornado" = "Tornado",
                                 "evt_hail"    = "Hail",
                                 "evt_flood"   = "Flood",
                                 "evt_other"   = "Other"
  )) |>
  group_by(event_category, month_t) |>
  summarise(
    mean_dev = mean(deviation, na.rm = TRUE),
    se_dev   = sd(deviation, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) |>
  ggplot(aes(x = month_t, y = mean_dev, color = event_category,
             fill = event_category)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.5) +
  geom_ribbon(aes(ymin = mean_dev - 1.96 * se_dev,
                  ymax = mean_dev + 1.96 * se_dev),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = c(-PRE_EVENT_MONTHS:-1, 0, 1:POST_EVENT_MONTHS),
    labels = c(paste0("T", -PRE_EVENT_MONTHS:-1), "T0", paste0("T+", 1:POST_EVENT_MONTHS))
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ event_category, ncol = 3) +
  labs(
    title    = "Post-Storm ZHVI Trajectory by Event Type",
    subtitle = "Mean monthly indexed deviation from neighbor baseline. Shaded band = 95% CI.",
    x        = "Months Post-Storm",
    y        = "Mean Indexed Deviation (index points)"
  ) +
  theme_minimal() +
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 7, angle = 45, hjust = 1)
  )
fig_event_study_by_type


# ---- Save figures ----
dir.create("./paper/output/figures", recursive = TRUE, showWarnings = FALSE)

ggsave("./paper/output/figures/fig_event_study.pdf",     fig_event_study,
       width = 8, height = 5)
ggsave("./paper/output/figures/fig_event_study_by_type.pdf", fig_event_study_by_type,
       width = 10, height = 7)

cat("Event study figures saved to ./paper/output/figures/\n")
