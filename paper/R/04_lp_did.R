# 04_lp_did.R
# Local Projections: Within-event tier DiD
# Requires: 00_load_data.R sourced first

library(tidyverse)
library(fixest)
library(broom)
POST_EVENT_MONTHS = 6
# ---- Prep Data ----
md_lp <- monthly_dev |>
  filter(month_t %in% 0:POST_EVENT_MONTHS) |>
  left_join(
    df |> select(stcofips, year, month, tier, pre_trend_annual, month_year),
    by = c("stcofips", "year", "month", "tier")
  ) |>
  filter(!is.na(pre_trend_annual)) |>
  mutate(tier = relevel(factor(tier), ref = "mid"))

cat(sprintf("LP dataset: %s rows, %s events\n",
            format(nrow(md_lp), big.mark = ","),
            format(n_distinct(interaction(md_lp$stcofips, md_lp$year, md_lp$month)), big.mark = ",")))

# ---- LP Loop ----
horizons <- 1:POST_EVENT_MONTHS
results  <- list()

for (h in horizons) {
  data_h <- md_lp |> filter(month_t == h)
  
  model <- tryCatch({
    feols(
      deviation ~ pre_trend_annual + tier | month_year,
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

# ---- Extract tier coefficients ----
ref_row <- function(term_name) {
  tibble(term = term_name, estimate = 0, conf.low = 0, conf.high = 0,
         horizon = 0, std.error = NA, statistic = NA, p.value = NA)
}

lp_tiers <- lp_results |>
  filter(term %in% c("tierbottom", "tiertop")) |>
  mutate(tier = case_when(
    term == "tierbottom" ~ "Bottom",
    term == "tiertop"    ~ "Top"
  )) |>
  bind_rows(
    ref_row("tierbottom") |> mutate(tier = "Bottom"),
    ref_row("tiertop")    |> mutate(tier = "Top")
  ) |>
  arrange(tier, horizon)

# ---- Plot ----
fig_lp_tier <- ggplot(lp_tiers,
                      aes(x = horizon, y = estimate,
                          color = tier, fill = tier)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 0:POST_EVENT_MONTHS,
    labels = c("T0", paste0("T+", 1:POST_EVENT_MONTHS))
  ) +
  scale_color_manual(values = c("Bottom" = "#d7191c", "Top" = "#2c7bb6")) +
  scale_fill_manual(values  = c("Bottom" = "#d7191c", "Top" = "#2c7bb6")) +
  labs(
    title    = "Dynamic Tier Response to Storm Events",
    subtitle = "Deviation from mid tier reference. T=0 anchored at 0. 95% CI.",
    x        = "Months Post-Storm",
    y        = "Coefficient Relative to Mid Tier",
    color    = "Market Tier",
    fill     = "Market Tier"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    axis.text.x      = element_text(size = 8)
  )

ggsave(here::here("paper", "output", "figures", "fig_lp_tier.pdf"),
       fig_lp_tier, width = 8, height = 5)

cat("LP tier figure saved\n")