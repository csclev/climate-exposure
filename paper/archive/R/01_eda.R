# 01_eda.R
library(tidyverse)
library(tigris)
library(sf)
library(scales)

options(tigris_use_cache = TRUE)

# Use mid tier as base for event counts to avoid triple-counting
df_mid <- df |> filter(tier == "mid")

# ---- 1. Storm Events by Year and Type ----
fig_events_by_type <- df_mid |>
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
  count(year, event_category) |>
  ggplot(aes(x = year, y = n, fill = event_category)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = comma) +
  labs(title = "Storm Events by Year and Event Type", x = NULL,
       y = "County-Month Storm Observations", fill = "Event Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# ---- 2. AUC Distribution by Tier ----
fig_auc_by_tier <- df |>
  ggplot(aes(x = auc, fill = tier)) +
  geom_histogram(bins = 60, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~ tier, ncol = 1, labeller = labeller(tier = c(
    "mid"    = "Mid Tier (33rd-67th percentile)",
    "bottom" = "Bottom Tier (0-33rd percentile)",
    "top"    = "Top Tier (67th-100th percentile)"
  ))) +
  scale_fill_manual(values = c("mid" = "steelblue", "bottom" = "#d7191c", "top" = "#2c7bb6")) +
  scale_x_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title    = "Distribution of Post-Storm CIR by Market Tier",
    subtitle = "Cumulative 9-month ZHVI deviation from neighbor baseline. Dashed line = zero.",
    x        = "CIR (index points)",
    y        = "Event Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

# ---- 3. Median AUC by Tier ----
fig_auc_tier_box <- df |>
  ggplot(aes(x = tier, y = auc, fill = tier)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  coord_cartesian(ylim = quantile(df$auc, c(0.05, 0.95), na.rm = TRUE)) +
  scale_fill_manual(values = c("mid" = "steelblue", "bottom" = "#d7191c", "top" = "#2c7bb6")) +
  scale_x_discrete(labels = c("mid" = "Mid", "bottom" = "Bottom", "top" = "Top")) +
  labs(
    title    = "Post-Storm CIR by Market Tier",
    subtitle = "Outliers trimmed to 5th-95th percentile. Dashed line = zero.",
    x        = "Market Tier",
    y        = "CIR (index points)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 4. County-level storm heatmap ----
county_events <- df_mid |>
  group_by(stcofips) |>
  summarise(total_events = sum(event_count), .groups = "drop") |>
  mutate(
    state_fips  = str_sub(stcofips, 1, 2),
    county_fips = str_sub(stcofips, 3, 5)
  )

counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2021) |>
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) |>
  left_join(county_events, by = c("STATEFP" = "state_fips", "COUNTYFP" = "county_fips"))

fig_storm_heatmap <- ggplot(counties_sf) +
  geom_sf(aes(fill = total_events), color = NA) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, na.value = "grey90",
    name = "Storm Events", labels = comma, trans = "log1p",
    breaks = c(1, 5, 20, 100, 500)
  ) +
  labs(
    title    = "County-Level Storm Event Exposure (2020-2025)",
    subtitle = "Total county-month storm observations. Counties in grey had no recorded events.",
    caption  = "Source: NOAA Storm Events Database"
  ) +
  theme_void() +
  theme(
    legend.position  = "bottom",
    legend.key.width = unit(2, "cm"),
    plot.title       = element_text(size = 13, face = "bold"),
    plot.subtitle    = element_text(size = 9, color = "grey40")
  )

# ---- Save ----
ggsave(here::here("paper", "output", "figures", "fig_events_by_type.pdf"),
       fig_events_by_type, width = 8, height = 5)
ggsave(here::here("paper", "output", "figures", "fig_auc_by_tier.pdf"),
       fig_auc_by_tier, width = 7, height = 7)
ggsave(here::here("paper", "output", "figures", "fig_auc_tier_box.pdf"),
       fig_auc_tier_box, width = 6, height = 5)
ggsave(here::here("paper", "output", "figures", "fig_storm_heatmap.pdf"),
       fig_storm_heatmap, width = 10, height = 6)

cat("EDA figures saved\n")