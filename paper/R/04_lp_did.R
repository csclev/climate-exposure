# 04_lp_did.R
# Local Projections: Dynamic effect of storm damage moderated by resilience
# Requires: 00_load_data.R sourced first

library(tidyverse)
library(fixest)
library(broom)

# ---- Prep Data ----
md_lp <- monthly_dev |>
  left_join(
    df |> select(stcofips, year, month, 
                 log_damage, event_count, pre_trend_annual, month_year),
    by = c("stcofips", "year", "month")
  ) |>
  filter(month_t != 0)

# ---- LP Loop ----
horizons <- c(-PRE_EVENT_MONTHS:-1, 1:POST_EVENT_MONTHS)
results  <- list()

for (h in horizons) {
  data_h <- md_lp |> filter(month_t == h)
  cat(sprintf("horizon %d: %d rows\n", h, nrow(data_h)))
  
  model <- tryCatch({
    feols(
      deviation ~ log_damage + log_damage:resl_score + resl_score +
        event_count + evt_wind + evt_tornado + evt_hail + evt_flood +
        pre_trend_annual | month_year + stcofips,
      cluster = ~stcofips + month_year,
      data    = data_h
    )
  }, error = function(e) NULL)
  
  if (!is.null(model)) {
    results[[as.character(h)]] <- broom::tidy(model, conf.int = TRUE) |>
      mutate(horizon = h, n_obs = nobs(model))
  }
}

lp_results <- bind_rows(results)

# ---- Extract coefficients of interest ----
lp_damage <- lp_results |>
  filter(term == "log_damage") |>
  bind_rows(tibble(term = "log_damage", estimate = 0, conf.low = 0, 
                   conf.high = 0, horizon = 0))

lp_interaction <- lp_results |>
  filter(term == "log_damage:resl_score") |>
  bind_rows(tibble(term = "log_damage:resl_score", estimate = 0, conf.low = 0,
                   conf.high = 0, horizon = 0))

# ---- Plot 1: log_damage dynamic effect ----
fig_lp_damage <- ggplot(lp_damage, 
                        aes(x = horizon, y = estimate)) +
  annotate("rect", xmin = -PRE_EVENT_MONTHS - 0.5, xmax = -0.5,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "firebrick") +
  annotate("rect", xmin = 0.5, xmax = POST_EVENT_MONTHS + 0.5,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, fill = "steelblue") +
  geom_line(linewidth = 0.9, color = "steelblue") +
  geom_point(size = 2.5, color = "steelblue") +
  scale_x_continuous(
    breaks = c(-PRE_EVENT_MONTHS:-1, 0, 1:POST_EVENT_MONTHS),
    labels = c(paste0("T", -PRE_EVENT_MONTHS:-1), "T0", 
               paste0("T+", 1:POST_EVENT_MONTHS))
  ) +
  labs(
    title    = "Dynamic Effect of Log Property Damage on ZHVI Deviation",
    subtitle = "Red shading = placebo region. Blue shading = post-storm window. 95% CI.",
    x        = "Months Relative to Storm",
    y        = "Coefficient on Log Property Damage"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# ---- Plot 2: log_damage x resl_score interaction dynamic effect ----
fig_lp_interaction <- ggplot(lp_interaction,
                             aes(x = horizon, y = estimate)) +
  annotate("rect", xmin = -PRE_EVENT_MONTHS - 0.5, xmax = -0.5,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "firebrick") +
  annotate("rect", xmin = 0.5, xmax = POST_EVENT_MONTHS + 0.5,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, fill = "coral") +
  geom_line(linewidth = 0.9, color = "coral") +
  geom_point(size = 2.5, color = "coral") +
  scale_x_continuous(
    breaks = c(-PRE_EVENT_MONTHS:-1, 0, 1:POST_EVENT_MONTHS),
    labels = c(paste0("T", -PRE_EVENT_MONTHS:-1), "T0",
               paste0("T+", 1:POST_EVENT_MONTHS))
  ) +
  labs(
    title    = "Dynamic Effect of Damage × Resilience Interaction on ZHVI Deviation",
    subtitle = "Negative = resilience attenuates damage-driven appreciation. 95% CI.",
    x        = "Months Relative to Storm",
    y        = "Coefficient on Log Damage × Resilience Score"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# ---- LP: M5 discrete interaction version ----
m5_results <- list()

for (h in horizons) {
  data_h <- md_lp |> filter(month_t == h)
  
  model <- tryCatch({
    feols(
      deviation ~ log_damage + resl_quartile + log_damage:resl_quartile +
        event_count + pre_trend_annual | month_year + stcofips,
      cluster = ~stcofips + month_year,
      data    = data_h
    )
  }, error = function(e) NULL)
  
  if (!is.null(model)) {
    m5_results[[as.character(h)]] <- broom::tidy(model, conf.int = TRUE) |>
      mutate(horizon = h, n_obs = nobs(model))
  }
}

m5_lp <- bind_rows(m5_results)

# ---- Extract terms ----
ref_row <- function(term_name) {
  tibble(term = term_name, estimate = 0, conf.low = 0, conf.high = 0,
         horizon = 0, std.error = NA, statistic = NA, p.value = NA)
}

lp_m5_damage <- m5_lp |>
  filter(term == "log_damage") |>
  bind_rows(ref_row("log_damage"))

lp_m5_interactions <- m5_lp |>
  filter(term %in% c("log_damage:resl_quartileLow", 
                     "log_damage:resl_quartileHigh")) |>
  mutate(quartile = case_when(
    str_detect(term, "Low")  ~ "Low",
    str_detect(term, "High") ~ "High"
  )) |>
  bind_rows(
    ref_row("log_damage:resl_quartileLow")  |> mutate(quartile = "Low"),
    ref_row("log_damage:resl_quartileHigh") |> mutate(quartile = "High")
  )

# ---- Panel 1: log_damage baseline ----
fig_lp_m5_damage <- ggplot(lp_m5_damage,
                           aes(x = horizon, y = estimate)) +
  annotate("rect", xmin = -PRE_EVENT_MONTHS - 0.5, xmax = -0.5,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "firebrick") +
  annotate("rect", xmin = 0.5, xmax = POST_EVENT_MONTHS + 0.5,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, fill = "steelblue") +
  geom_line(linewidth = 0.9, color = "steelblue") +
  geom_point(size = 2.5, color = "steelblue") +
  scale_x_continuous(
    breaks = c(-PRE_EVENT_MONTHS:-1, 0, 1:POST_EVENT_MONTHS),
    labels = c(paste0("T", -PRE_EVENT_MONTHS:-1), "T0",
               paste0("T+", 1:POST_EVENT_MONTHS))
  ) +
  labs(
    title    = "Dynamic Effect of Log Damage (Middle Quartile Reference)",
    subtitle = "Red = placebo region. Blue = post-storm window. 95% CI.",
    x        = "Months Relative to Storm",
    y        = "Coefficient on Log Property Damage"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# ---- Panel 2: Interactions overlaid ----
fig_lp_m5_interactions <- ggplot(lp_m5_interactions,
                                 aes(x = horizon, y = estimate,
                                     color = quartile, fill = quartile)) +
  annotate("rect", xmin = -PRE_EVENT_MONTHS - 0.5, xmax = -0.5,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "firebrick") +
  annotate("rect", xmin = 0.5, xmax = POST_EVENT_MONTHS + 0.5,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = c(-PRE_EVENT_MONTHS:-1, 0, 1:POST_EVENT_MONTHS),
    labels = c(paste0("T", -PRE_EVENT_MONTHS:-1), "T0",
               paste0("T+", 1:POST_EVENT_MONTHS))
  ) +
  scale_color_manual(values = c("Low" = "#d7191c", "High" = "#2c7bb6")) +
  scale_fill_manual(values  = c("Low" = "#d7191c", "High" = "#2c7bb6")) +
  labs(
    title    = "Dynamic Interaction: Log Damage × Resilience Quartile",
    subtitle = "Relative to Middle quartile reference. Red = placebo. 95% CI.",
    x        = "Months Relative to Storm",
    y        = "Interaction Coefficient",
    color    = "Quartile",
    fill     = "Quartile"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# ---- Save ----
ggsave(here("paper", "output", "figures", "fig_lp_m5_damage.pdf"),
       fig_lp_m5_damage, width = 8, height = 5)
ggsave(here("paper", "output", "figures", "fig_lp_m5_interactions.pdf"),
       fig_lp_m5_interactions, width = 8, height = 5)

cat("LP M5 figures saved\n")


ggsave(here("paper", "output", "figures", "fig_lp_damage.pdf"),
       fig_lp_damage, width = 8, height = 5)
ggsave(here("paper", "output", "figures", "fig_lp_interaction.pdf"),
       fig_lp_interaction, width = 8, height = 5)

cat("LP figures saved\n")
