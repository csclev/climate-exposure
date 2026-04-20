# 04_lp_did.R
# Local Projection Models


library(tidyverse)
library(fixest)

# ---- Prep Data ----

md <- read_csv("./data/processed/monthly_deviations.csv")
md <- md |>
  mutate(stcofips = str_pad(as.character(stcofips), width=5, side="left", pad="0")) |>
  left_join(df,by = c("stcofips","year", "month"))|> 
  arrange(stcofips, year, month, month_t)


md_ols <- md |> 
  group_by(stcofips, year, month) |>
  mutate(deviation_prior = lag(deviation, n=1, order_by = month_t)) |>
  ungroup() 

md_ols <- md_ols |>
  mutate(resl_zone = factor(resl_zone, levels = c("Middle", "Low", "High")))

md_ols$resl_zone


results_list <- list()
for (i in -5:12) {
  
  if (i == 0) next
  
  data_i <- md_ols |>
    filter(month_t == i)
  
  if (i > 0) {
    form <- deviation ~ deviation_prior + resl_zone + sovi_score + log_eal_valt + event_count * resl_zone + log_damage  | month_year + stcofips
  } else if (i < 0) {
    form <- deviation ~ deviation_prior + resl_zone + sovi_score + log_eal_valt   | month_year + stcofips
  }
  
  model <- feols(form,
                 cluster = ~stcofips,
                 data=data_i)

  results_list[[as.character(i)]] <- broom::tidy(model, conf.int = TRUE) |> 
    mutate(horizon = i,
           n_obs = nobs(model)
           )
}

final_results <- bind_rows(results_list)

# Filter for main variable of interest
variable = "resl_zoneHigh:event_count"
plot_data <- final_results |> 
  filter(term == variable)

ggplot(plot_data, aes(x = horizon, y = estimate, group = 1)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") +
  geom_text(aes(label = paste0("n=", n_obs)), vjust = -1.5, size = 3, check_overlap = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  expand_limits(y = max(plot_data$conf.high) * 1.2) +
  labs(
    title = paste0("Impact of ",variable),
    subtitle = "Local Projection with Horizon-Specific Sample Sizes",
    x = "Months Relative to Event",
    y = "Coefficient Estimate"
  ) +
  theme_minimal()