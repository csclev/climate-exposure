# 04_lp_did.R
# Local Projection Models


library(tidyverse)
library(fixest)

# ---- Prep Data ----

md <- read_csv("./data/processed/monthly_deviations.csv")
md_ols <- md |>
  mutate(stcofips = str_pad(as.character(stcofips), width=5, side="left", pad="0")) |>
  left_join(df, by = c("stcofips","year", "month")) |> 
  mutate(resl_zone = str_trim(resl_zone)) |> 
  group_by(stcofips, year, month) |>
  mutate(deviation_prior = lag(deviation, n=1, order_by = month_t)) |>
  ungroup()

md_ols <- md_ols |>
  arrange(stcofips, year, month)
print(md_ols,n=26)
# results_list <- list()
# 
# for (i in -5:12) {
#   
#   if (i == 0) next
#   
#   data_i <- md_ols |>
#     filter(month_t == i)
#   
#   if (i > 0) {
#     form <- deviation ~ deviation_prior + resl_score + sovi_score + log_eal_valt + event_count + log_damage  | month_year + stcofips
#   } else if (i < 0) {
#     form <- deviation ~ deviation_prior + resl_score + sovi_score + log_eal_valt   | month_year + stcofips
#   }
#   
#   model <- feols(form,
#                  cluster = ~stcofips,
#                  data=data_i)
# 
#   results_list[[as.character(i)]] <- broom::tidy(model, conf.int = TRUE) |> 
#     mutate(horizon = i,
#            n_obs = nobs(model)
#            )
# }
# 
# final_results <- bind_rows(results_list)
# plot_data <- final_results |> 
#   filter(term == variable)
# 
# ggplot(plot_data, aes(x = horizon, y = estimate, group = 1)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "steelblue") +
#   geom_line(color = "steelblue", size = 1) +
#   geom_point(color = "steelblue") +
#   geom_text(aes(label = paste0("n=", n_obs)), vjust = -1.5, size = 3, check_overlap = TRUE) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#   expand_limits(y = max(plot_data$conf.high) * 1.2) +
#   labs(
#     title = paste0("Impact of ",variable),
#     subtitle = "Local Projection with Horizon-Specific Sample Sizes",
#     x = "Months Relative to Event",
#     y = "Coefficient Estimate"
#   ) +
#   theme_minimal()

# Filter for main variable of interest
target_var <- "deviation_prior" 

combined_results <- list()

# 1. Change the variable of interest to your "News" variable
target_var <- "event_count" 

for (zone in c("High", "Low")) {
  for (i in -2:9) {
    if (i == 0) next
    
    data_i <- md_ols |> filter(month_t == i, resl_zone == zone)
    
    # 2. Updated Formula: 
    # Use event_count as the main driver. Keep log_damage as a control.
    # Keep deviation_prior only if i > 1 (to avoid the t=0 zero-variance issue)
    
    if (i == 1) {
      form <- deviation ~ event_count + log_damage | month_year + stcofips
    } else {
      form <- deviation ~ event_count + log_damage + deviation_prior | month_year + stcofips
    }
    
    model <- tryCatch({
      feols(form, cluster = ~stcofips, data = data_i)
    }, error = function(e) return(NULL))
    
    if (!is.null(model)) {
      res <- broom::tidy(model, conf.int = TRUE) |> filter(term == target_var)
      if(nrow(res) > 0) {
        combined_results[[paste0(zone, i)]] <- res |> 
          mutate(horizon = i, zone = zone, n_obs = nobs(model))
      }
    }
  }
}

final_plot_data <- bind_rows(combined_results)

# --- SAFETY CHECK ---
if(nrow(final_plot_data) == 0) {
  stop(paste0("Error: No data found for term '", target_var, "'. Check the printed list of terms above."))
}

# --- DYNAMIC TITLE HELPER ---
# This converts technical names to readable titles
clean_title <- case_when(
  target_var == "log_damage" ~ "Direct Impact of Storm Damage",
  target_var == "sovi_score" ~ "Direct Effect of Social Vulnerability",
  target_var == "log_damage:sovi_score" ~ "Interaction: Storm Damage x Social Vulnerability",
  target_var == "log_damage:resl_score" ~ "Interaction: Storm Damage x Resilience Score",
  TRUE ~ target_var
)
combined_results
# --- PLOT ---
ggplot(final_plot_data, aes(x = horizon, y = estimate, color = zone, fill = zone)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  geom_line(size = 1) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  # NEW: Dynamic Labels
  labs(
    title = paste0("Housing Value Response: ", clean_title),
    subtitle = "Comparing High vs. Low Resilience Zones (County & Month-Year FE)",
    x = "Months Relative to Event (t=0)",
    y = "Coefficient Estimate (Impact on ZHVI)",
    color = "Resilience Group",
    fill = "Resilience Group",
    caption = "Data source: Zillow (ZHVI) and FEMA/CDC. Estimates via Local Projections (fixest)."
  ) +
  
  theme_minimal() +
  scale_color_manual(values = c("High" = "#2c7bb6", "Low" = "#d7191c")) +
  scale_fill_manual(values = c("High" = "#2c7bb6", "Low" = "#d7191c")) +
  
  # Better spacing for titles
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )