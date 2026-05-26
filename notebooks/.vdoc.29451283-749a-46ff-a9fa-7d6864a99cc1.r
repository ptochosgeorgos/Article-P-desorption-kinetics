# Helper to plot boxplots of residuals per site
plot_residuals_boxplot <- function(model, data, title) {
  plot_data <- data |> mutate(Residuals = residuals(model))
  
  ggplot(plot_data, aes(x = site, y = Residuals, fill = site)) +
    geom_boxplot(alpha = 0.7, outlier.size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = title,
      x = "Monitoring Site",
      y = "Conditional Residuals"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

(plot_residuals_boxplot(mod_raw_co2_geo, D_Long_Geo, "Geo: Raw P_CO2") |
 plot_residuals_boxplot(mod_thm_co2_geo, D_Long_Geo, "Geo: Thermo a_CO2") |
 plot_residuals_boxplot(mod_raw_aae_geo, D_Long_Geo, "Geo: Legacy P_AAE10")) /
(plot_residuals_boxplot(mod_raw_co2_agro, D_Long_Agro, "Agro: Raw P_CO2") |
 plot_residuals_boxplot(mod_thm_co2_agro, D_Long_Agro, "Agro: Thermo a_CO2") |
 plot_residuals_boxplot(mod_raw_aae_agro, D_Long_Agro, "Agro: Legacy P_AAE10"))
