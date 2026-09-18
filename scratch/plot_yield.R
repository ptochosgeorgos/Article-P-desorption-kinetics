library(tidyverse)

# Load data
d_brms <- readRDS("data/D_Yield_clean.rds")

# Recreate GRUD Supply Class logic natively (to align crops)
crop_refs <- data.frame(
  crop = c("Wheat", "KA", "KM", "RA", "SJ", "WG", "ZR", "FR"),
  Y_ref = c(60, 450, 100, 30, 30, 60, 900, 175)
)

d_brms <- d_brms |>
    mutate(crop = case_when(
      crop %in% c("WW", "SW", "WS") ~ "Wheat",
      TRUE ~ as.character(crop)
    )) |>
    filter(crop %in% crop_refs$crop)

# Create the plot
p <- ggplot(d_brms, aes(x = soil_0_20_P_CO2, y = annual_yield_mp_DM)) +
  geom_point(alpha = 0.5, color = "#2c3e50") +
  facet_wrap(~ crop, scales = "free_y") +
  labs(
    title = "Raw Agronomic Yield vs P-CO2 by Crop",
    subtitle = "Without mixed-effects regularization, the noise completely dominates the signal",
    x = "Extractable Soil P (P-CO2) [mg/kg]",
    y = "Annual Yield (mp DM)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

# Save to artifacts directory so it can be embedded in chat
ggsave("/home/marc/.gemini/antigravity-ide/brain/97d9c769-81d9-4856-9d32-3ce497c24462/yield_by_crop.png", plot = p, width = 10, height = 7, dpi = 150)
cat("Plot saved to artifacts.")
