# 04_lp_did.R
# Local Projection Models


library(tidyverse)
library(fixest)
library(broom)
library(tidyr)
library(dplyr)
library(ggplot2)

# ---- Prep Data ----

md <- read_csv("./data/processed/monthly_deviations.csv")
md_ols <- md |>
  mutate(stcofips = str_pad(as.character(stcofips), width=5, side="left", pad="0")) |>
  left_join(df, by = c("stcofips","year", "month")) |> 
  mutate(resl_zone = str_trim(resl_zone)) |> 
  group_by(stcofips, year, month) |>
  mutate(deviation_prior = lag(deviation, n=1, order_by = month_t))|>
  mutate(
    # Combine Low and Middle into one group
    resl_combined = fct_collapse(resl_zone,
                                 "Low/Mid" = c("Low", "Middle"),
                                 "High" = "High"
    ),
    # Set High as the reference category
    resl_combined = fct_relevel(resl_combined, "High")
  ) |>
  ungroup()
md_ols$resl_zone <- factor(md_ols$resl_zone, levels = c("Low", "High", "Middle"))

md_ols <- md_ols |>
  arrange(stcofips, year, month)

model_pooled <- feols(
  deviation ~deviation_prior + i(month_t, resl_zone, ref = 0)  | stcofips + month_year,
  cluster = ~stcofips,
  data = md_ols
)
summary(model_pooled)

model_combined <- feols(
  deviation ~ deviation_prior + i(month_t, resl_combined,ref = 0) + 
    log_damage + event_count| stcofips + month_year,
  cluster = ~stcofips,
  data = md_ols
)

######
# 1. Tidy the model and filter for the interaction terms
plot_data <- tidy(model_pooled, conf.int = TRUE) %>%
  filter(grepl("month_t::", term)) %>%
  # 2. Use regex to split the complex fixest term names
  extract(term, 
          into = c("horizon", "zone"), 
          regex = "month_t::(-?\\d+):resl_zone::(\\w+)", 
          convert = TRUE) %>%
  # 3. Add the reference month (t=0) which was omitted by the model
  bind_rows(
    expand.grid(horizon = 0, zone = c("High", "Middle", "Low")) %>%
      mutate(estimate = 0, conf.low = 0, conf.high = 0)
  ) %>%
  # 4. Clean up factor levels for plotting
  mutate(zone = factor(zone, levels = c("High", "Middle", "Low")))

ggplot(plot_data, aes(x = horizon, y = estimate, color = zone, fill = zone)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  # Confidence intervals
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  # Main lines and points
  geom_line(size = 1.1) +
  geom_point(size = 2.5) +
  # Aesthetic tweaks
  scale_color_manual(values = c("High" = "#2c7bb6", "Middle" = "#fdae61", "Low" = "#d7191c")) +
  scale_fill_manual(values = c("High" = "#2c7bb6", "Middle" = "#fdae61", "Low" = "#d7191c")) +
  scale_x_continuous(breaks = -2:9) +
  labs(
    title = "Housing Value Response by Resilience Zone",
    subtitle = "Estimates relative to month of event (t=0)",
    x = "Months Relative to Event",
    y = "Coefficient Estimate (ZHVI Deviation)",
    color = "Resilience Zone", fill = "Resilience Zone"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
#####

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
  for (i in -2:6) {
    if (i == 0) next
    
    data_i <- md_ols |> filter(month_t == i, resl_zone == zone)
    
    # 2. Updated Formula: 
    # Use event_count as the main driver. Keep log_damage as a control.
    # Keep deviation_prior only if i > 1 (to avoid the t=0 zero-variance issue)
    
    if (i == 1) {
      form <- deviation ~ event_count + log_damage | month_year + stcofips
    } else {
      form <- deviation ~ event_count + log_damage + deviation_prior + sovi_score| month_year + stcofips
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
# Create the reference row for t=0
ref_row <- data.frame(
  term = "event_count",
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 1,
  conf.low = 0,
  conf.high = 0,
  horizon = 0,
  n_obs = NA
)

# Expand this for both zones
ref_data <- bind_rows(
  ref_row %>% mutate(zone = "High"),
  ref_row %>% mutate(zone = "Low")
)

# Combine with your actual results
final_plot_data <- bind_rows(combined_results) %>%
  bind_rows(ref_data) %>%
  arrange(zone, horizon)


# --- SAFETY CHECK ---
if(nrow(final_plot_data) == 0) {
  stop(paste0("Error: No data found for term '", target_var, "'. Check the printed list of terms above."))
}

# --- DYNAMIC TITLE HELPER ---
# This converts technical names to readable titles
clean_title <- case_when(
  target_var == "event_count" ~ "Direct Impact of Storms",
  target_var == "sovi_score" ~ "Direct Effect of Social Vulnerability",
  target_var == "event_count:sovi_score" ~ "Interaction: Storms x Social Vulnerability",
  target_var == "event_count:resl_score" ~ "Interaction: Storms x Resilience Score",
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