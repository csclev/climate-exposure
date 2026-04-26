# 00_load_data.R
# Single source of truth for data loading and preparation.
# All other R files source() this file.

# ---- Output directories ----
dir.create(here::here("paper", "output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("paper", "output", "tables"),  recursive = TRUE, showWarnings = FALSE)

# ---- Window parameters (must match 04_zillow_pipeline.ipynb) ----
PRE_EVENT_MONTHS  <- 3
POST_EVENT_MONTHS <- 9
LATEST_ZHVI <- as.Date("2026-02-28")
CUTOFF      <- seq(LATEST_ZHVI, length = 2, by = paste0("-", POST_EVENT_MONTHS, " months"))[2]

# ---- Lib ----
library(tidyverse)
library(fixest)
library(here)

# ---- Analysis dataset (one row per county-month storm event) ----

# Note: 'auc' in code corresponds to 'Cumulative Impulse Response (CIR)' in the paper

df <- read_csv(here::here("data/processed/analysis_dataset.csv"), 
               col_types = cols(stcofips = col_character())) |>
  mutate(
    month_year     = factor(paste0(month, "_", year)),
    log_eal_valt   = log1p(eal_valt),
    log_risk_value = log1p(risk_value),
    resl_quartile  = cut(
      resl_score,
      breaks   = quantile(resl_score, probs = c(0, 0.25, 0.75, 1), na.rm = TRUE),
      labels   = c("Low", "Middle", "High"),
      include.lowest = TRUE
    )
  ) |>
  mutate(resl_quartile = relevel(factor(resl_quartile), ref = "Middle"))


# ---- Dummy variables for all Event Types  ----

df <- df |>
  mutate(
    evt_wind    = as.integer(str_detect(event_type, "Thunderstorm Wind|Funnel Cloud")),
    evt_tornado = as.integer(str_detect(event_type, "Tornado")),
    evt_hail    = as.integer(str_detect(event_type, "Hail")),
    evt_flood   = as.integer(str_detect(event_type, "Flash Flood|Flood|Heavy Rain")),
    evt_other   = as.integer(str_detect(event_type, "Lightning|Dust Devil|Debris Flow"))
  )


# ---- Monthly deviations (one row per event per post-storm month) ----
monthly_dev <- read_csv(here::here("data/processed/monthly_deviations.csv")) |>
  # Join resilience score and climate region from main dataset
  left_join(
    df |> select(stcofips, year, month, resl_score, resl_quartile, 
                 event_type, episode_id, evt_wind, evt_tornado, 
                 evt_hail, evt_flood, evt_other),
    by = c("stcofips", "year", "month")
  )

# ---- Summary constants (used in inline R blocks in report.Rmd) ----
n_events        <- nrow(df)
n_counties      <- n_distinct(df$stcofips)
n_years         <- n_distinct(df$year)
year_range      <- paste(min(df$year), max(df$year), sep = "-")
median_resl     <- median(df$resl_score, na.rm = TRUE)
median_auc      <- median(df$auc, na.rm = TRUE)

cat("Data loaded successfully\n")
cat(sprintf("  Analysis dataset: %s events, %s counties\n",
            format(n_events, big.mark = ","),
            format(n_counties, big.mark = ",")))
cat(sprintf("  Monthly deviations: %s rows\n",
            format(nrow(monthly_dev), big.mark = ",")))

