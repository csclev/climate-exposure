# 00_load_data.R
# Single source of truth for data loading and preparation.
# All other R files source() this file.

library(tidyverse)
library(fixest)
library(here)

# ---- Analysis dataset (one row per county-month storm event) ----


df <- read_csv(here("data/processed/analysis_dataset.csv"), 
               col_types = cols(stcofips = col_character())) |>
  mutate(
    month_year     = factor(paste0(month, "_", year)),
    log_eal_valt   = log(eal_valt),
    log_risk_value = log(risk_value),
    resl_quartile  = cut(
      resl_score,
      breaks   = quantile(resl_score, probs = c(0, 0.25, 0.75, 1), na.rm = TRUE),
      labels   = c("Low (Q1)", "Middle", "High (Q4)"),
      include.lowest = TRUE
    )
  )

# ---- Monthly deviations (one row per event per post-storm month T+1 to T+12) ----
monthly_dev <- read_csv(here("data/processed/monthly_deviations.csv")) |>
  # Join resilience score and climate region from main dataset
  left_join(
    df |> select(stcofips, year, month, resl_score, resl_quartile, climate_region),
    by = c("stcofips", "year", "month")
  )

# ---- Summary constants (used in inline R blocks in report.Rmd) ----
n_events        <- nrow(df)
n_counties      <- n_distinct(df$stcofips)
n_years         <- n_distinct(df$year)
year_range      <- paste(min(df$year), max(df$year), sep = "-")
n_climate_regions <- n_distinct(df$climate_region)
median_resl     <- median(df$resl_score, na.rm = TRUE)
median_auc      <- median(df$auc, na.rm = TRUE)

cat("Data loaded successfully\n")
cat(sprintf("  Analysis dataset: %s events, %s counties\n",
            format(n_events, big.mark = ","),
            format(n_counties, big.mark = ",")))
cat(sprintf("  Monthly deviations: %s rows\n",
            format(nrow(monthly_dev), big.mark = ",")))

