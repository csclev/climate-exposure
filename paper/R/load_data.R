# load_data.R
# Function to load and prepare a single baseline dataset
# Returns a named list: df, monthly_dev, event_neighbors

library(tidyverse)
library(fixest)
library(here)

load_baseline <- function(path, baseline_name) {
  
  path <- here::here(path)
  
  # ---- Analysis dataset ----
  df <- read_csv(file.path(path, "analysis_dataset.csv"),
                 col_types = cols(stcofips = col_character())) |>
    mutate(
      baseline      = baseline_name,
      month_year    = factor(paste0(month, "_", year)),
      tier          = factor(tier, levels = c("mid", "bottom", "top")),
      log_eal_valt  = log1p(eal_valt),
      log_risk_value = log1p(risk_value),
      resl_quartile = cut(
        resl_value,
        breaks = quantile(resl_value, probs = c(0, 0.25, 0.75, 1), na.rm = TRUE),
        labels = c("Low", "Middle", "High"),
        include.lowest = TRUE
      )
    ) |>
    mutate(
      resl_quartile = relevel(factor(resl_quartile), ref = "Middle"),
      evt_wind      = as.integer(str_detect(event_type, "Thunderstorm Wind|Funnel Cloud")),
      evt_tornado   = as.integer(str_detect(event_type, "Tornado")),
      evt_hail      = as.integer(str_detect(event_type, "Hail")),
      evt_flood     = as.integer(str_detect(event_type, "Flash Flood|Flood|Heavy Rain")),
      evt_other     = as.integer(str_detect(event_type, "Lightning|Dust Devil|Debris Flow"))
    )
  
  # ---- Monthly deviations ----
  monthly_dev <- read_csv(file.path(path, "monthly_deviations.csv")) |>
    left_join(
      df |> select(stcofips, year, month, tier, resl_score, resl_value,
                   resl_quartile, event_type, evt_wind, evt_tornado,
                   evt_hail, evt_flood, evt_other),
      by = c("stcofips", "year", "month", "tier")
    )
  
  # ---- Event neighbors (for balance table) ----
  event_neighbors_mid    <- read_csv(file.path(path, "event_neighbors_mid.csv"),
                                     col_types = cols(target_fips = col_character()))
  event_neighbors_top    <- read_csv(file.path(path, "event_neighbors_top.csv"),
                                     col_types = cols(target_fips = col_character()))
  event_neighbors_bottom <- read_csv(file.path(path, "event_neighbors_bottom.csv"),
                                     col_types = cols(target_fips = col_character()))
  
  # ---- Window parameters ----
  PRE_EVENT_MONTHS  <- 3
  POST_EVENT_MONTHS <- 9
  
  # ---- Summary constants ----
  df_mid     <- df |> filter(tier == "mid")
  n_events   <- n_distinct(interaction(df_mid$stcofips, df_mid$year, df_mid$month))
  n_counties <- n_distinct(df_mid$stcofips)
  year_range <- paste(min(df_mid$year), max(df_mid$year), sep = "-")
  median_resl <- median(df_mid$resl_score, na.rm = TRUE)
  median_auc  <- median(df_mid$auc, na.rm = TRUE)
  
  cat(sprintf("Loaded baseline: %s\n", baseline_name))
  cat(sprintf("  Events (mid tier): %s, Counties: %s\n",
              format(n_events, big.mark = ","),
              format(n_counties, big.mark = ",")))
  cat(sprintf("  Monthly deviations: %s rows\n",
              format(nrow(monthly_dev), big.mark = ",")))
  
  list(
    df                     = df,
    monthly_dev            = monthly_dev,
    event_neighbors_mid    = event_neighbors_mid,
    event_neighbors_top    = event_neighbors_top,
    event_neighbors_bottom = event_neighbors_bottom,
    n_events               = n_events,
    n_counties             = n_counties,
    year_range             = year_range,
    median_resl            = median_resl,
    median_auc             = median_auc,
    PRE_EVENT_MONTHS       = PRE_EVENT_MONTHS,
    POST_EVENT_MONTHS      = POST_EVENT_MONTHS,
    baseline_name          = baseline_name
  )
}