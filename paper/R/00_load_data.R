df <- read_csv(here::here("data/processed/analysis_dataset.csv"), 
               col_types = cols(stcofips = col_character())) |>
  mutate(
    month_year    = factor(paste0(month, "_", year)),
    tier          = factor(tier, levels = c("bottom", "mid", "top")),
    log_eal_valt  = log1p(eal_valt),
    log_risk_value = log1p(risk_value),
    resl_quartile = cut(
      resl_value,
      breaks   = quantile(resl_value, probs = c(0, 0.25, 0.75, 1), na.rm = TRUE),
      labels   = c("Low", "Middle", "High"),
      include.lowest = TRUE
    )
  ) |>
  mutate(resl_quartile = relevel(factor(resl_quartile), ref = "Middle"))

df <- df |>
  mutate(
    evt_wind    = as.integer(str_detect(event_type, "Thunderstorm Wind|Funnel Cloud")),
    evt_tornado = as.integer(str_detect(event_type, "Tornado")),
    evt_hail    = as.integer(str_detect(event_type, "Hail")),
    evt_flood   = as.integer(str_detect(event_type, "Flash Flood|Flood|Heavy Rain")),
    evt_other   = as.integer(str_detect(event_type, "Lightning|Dust Devil|Debris Flow"))
  )

monthly_dev <- read_csv(here::here("data/processed/monthly_deviations.csv")) |>
  left_join(
    df |> select(stcofips, year, month, tier, resl_score, resl_value, resl_quartile,
                 event_type, episode_count, evt_wind, evt_tornado,
                 evt_hail, evt_flood, evt_other),
    by = c("stcofips", "year", "month", "tier")
  )

# Summary constants — base on mid tier only for event counts
df_mid <- df |> filter(tier == "mid")
n_events   <- n_distinct(interaction(df_mid$stcofips, df_mid$year, df_mid$month))
n_counties <- n_distinct(df_mid$stcofips)
n_years    <- n_distinct(df_mid$year)
year_range <- paste(min(df_mid$year), max(df_mid$year), sep = "-")
median_resl <- median(df_mid$resl_score, na.rm = TRUE)
median_auc  <- median(df_mid$auc, na.rm = TRUE)

cat("Data loaded successfully\n")
cat(sprintf("  Analysis dataset: %s events x 3 tiers = %s rows, %s counties\n",
            format(n_events, big.mark = ","),
            format(nrow(df), big.mark = ","),
            format(n_counties, big.mark = ",")))
cat(sprintf("  Monthly deviations: %s rows\n",
            format(nrow(monthly_dev), big.mark = ",")))
