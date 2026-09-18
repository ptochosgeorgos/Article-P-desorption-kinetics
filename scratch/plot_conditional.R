library(tidyverse)

res <- readRDS("data/cluster_results.rds")

# The conditional effects dataframe
df_mech <- res$yield$plot_data$mech
df_null <- res$yield$plot_data$null

# We need the raw data for the background scatter (but only for the reference crop that conditional_effects used, which is FR)
d_brms <- readRDS("data/D_Yield_clean.rds") |> filter(crop == "FR")

p <- ggplot() +
  geom_point(data = d_brms, aes(x = soil_0_20_P_CO2, y = annual_yield_mp_DM), alpha = 0.3, color = "gray50") +
  
  # Null Model (Red)
  geom_ribbon(data = df_null, aes(x = effect1__, ymin = lower__, ymax = upper__), fill = "red", alpha = 0.15) +
  geom_line(data = df_null, aes(x = effect1__, y = estimate__), color = "red", linewidth = 1, linetype = "dashed") +
  
  # Mechanistic Model (Blue)
  geom_ribbon(data = df_mech, aes(x = effect1__, ymin = lower__, ymax = upper__), fill = "blue", alpha = 0.25) +
  geom_line(data = df_mech, aes(x = effect1__, y = estimate__), color = "blue", linewidth = 1.2) +
  
  labs(
    title = "Marginal Yield Prediction (Mechanistic vs Null)",
    subtitle = "Blue: Mechanistic (w/ Buffer & Weather) | Red: Null (Kinetics only). Plotting reference crop (FR).",
    x = "Extractable Soil P (P-CO2) [mg/kg]",
    y = "Predicted Yield (mp DM)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("/home/marc/.gemini/antigravity-ide/brain/97d9c769-81d9-4856-9d32-3ce497c24462/marginal_yield.png", plot = p, width = 8, height = 6, dpi = 150)
cat("Plot saved.\n")
