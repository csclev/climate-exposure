# 02_event_study_plots.R
library(tidyverse)
library(scales)
PRE_EVENT_MONTHS = 3
POST_EVENT_MONTHS = 6
# ---- 1. Post-storm ZHVI trajectory by market tier ----
fig_event_study <- monthly_dev |>
  group_by(tier, month_t) |>
  summarise(
    mean_dev = mean(deviation, na.rm = TRUE),
    se_dev   = sd(deviation, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) |>
  ggplot(aes(x = month_t, y = mean_dev, color = tier, fill = tier)) +
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
  scale_color_manual(values = c("mid" = "steelblue", "bottom" = "#d7191c", "top" = "#2c7bb6")) +
  scale_fill_manual(values  = c("mid" = "steelblue", "bottom" = "#d7191c", "top" = "#2c7bb6")) +
  annotate("text", x = -0.3, y = Inf, label = "Storm",
           hjust = 1, vjust = 1.5, size = 3, color = "grey40", fontface = "italic") +
  labs(
    title    = "Post-Storm ZHVI Trajectory by Market Tier",
    subtitle = "Mean monthly indexed deviation from neighbor baseline. Shaded band = 95% CI.",
    x        = "Months Post-Storm",
    y        = "Mean Indexed Deviation (index points)",
    color    = "Market Tier",
    fill     = "Market Tier"
  ) +
  theme_minimal() +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 7, angle = 45, hjust = 1)
  )

# ---- 2. Post-storm trajectory by event type - faceted, mid tier only ----
fig_event_study_by_type <- monthly_dev |>
  filter(tier == "mid") |>
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
  ggplot(aes(x = month_t, y = mean_dev, color = event_category, fill = event_category)) +
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
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ event_category, ncol = 3) +
  labs(
    title    = "Post-Storm ZHVI Trajectory by Event Type (Mid Tier)",
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

# ---- Save ----
ggsave(here::here("paper", "output", "figures", "fig_event_study.pdf"),
       fig_event_study, width = 8, height = 5)
ggsave(here::here("paper", "output", "figures", "fig_event_study_by_type.pdf"),
       fig_event_study_by_type, width = 10, height = 7)

cat("Event study figures saved\n")