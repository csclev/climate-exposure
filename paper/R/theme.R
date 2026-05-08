# theme.R
# Shared visual constants for all charts

library(ggplot2)

# ---- Color palettes ----
TIER_COLORS <- c(
  "Bottom" = "#e41a1c",
  "Mid"    = "#4daf4a",
  "Top"    = "#984ea3"
)

TIER_COLORS_LOWER <- c(
  "bottom" = "#e41a1c",
  "mid"    = "#4daf4a",
  "top"    = "#984ea3"
)

TIER_LABELS <- c(
  "bottom" = "Bottom",
  "mid"    = "Mid",
  "top"    = "Top"
)

BASELINE_COLORS <- c(
  "Regional"         = "#fc8d62",
  "Second Ring (L2)" = "#8da0cb",
  "Adjacent (L1)"    = "#66c2a5"
)

# ---- Shared theme ----
theme_paper <- theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    axis.text.x      = element_text(size = 7, angle = 45, hjust = 1)
  )