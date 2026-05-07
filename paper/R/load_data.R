# load_data.R
# Loads a single baseline dataset and prepares it for analysis

library(tidyverse)
library(fixest)
library(here)

load_baseline <- function(path, baseline_name) {
  
  path <- here::here(path)
  
  df <- read_csv(file.path(path, "analysis_dataset.csv"),
                 col_types = cols(stcofips = col_character()),
                 show_col_types = FALSE) |>
    mutate(
      baseline       = baseline_name,
      tier           = factor(tier, levels = c("mid", "bottom", "top")),
      month_year     = factor(paste0(month, "_", year)),
      log_eal_valt   = log1p(eal_valt),
      log_risk_value = log1p(risk_value)
    )
  
  hed <- read_csv(here::here("data", "processed", "hedonic_dataset.csv"),
                  col_types = cols(stcofips = col_character()),
                  show_col_types = FALSE) |>
    distinct(stcofips, coastal)
  
  df <- df |> left_join(hed, by = "stcofips")
  
  monthly_dev <- read_csv(file.path(path, "monthly_deviations.csv"),
                          show_col_types = FALSE) |>
    mutate(stcofips = as.character(stcofips))
  
  cat(sprintf("Loaded %s: %s rows, %s counties\n",
              baseline_name,
              format(nrow(df), big.mark = ","),
              format(n_distinct(df$stcofips), big.mark = ",")))
  
  list(
    df          = df,
    monthly_dev = monthly_dev,
    baseline    = baseline_name
  )
}