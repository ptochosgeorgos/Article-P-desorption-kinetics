library(ggplot2)
library(dplyr)

final_obj <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_obj$data$D_Long_Agro

target_crops <- c("WW", "KA", "ZR", "RA", "KM")

D_plot <- D_Long_Agro |>
    filter(crop %in% target_crops) |>
    mutate(
        C_P = annual_P_uptake / annual_yield_mp_DM,
        Yield = annual_yield_mp_DM
    ) |>
    filter(is.finite(C_P), C_P > 0, Yield > 0, a_CO2_total_mg_L > 0) |>
    group_by(crop) |>
    mutate(
        Yield_Quantile = ntile(Yield, 5),
        Yield_Quantile = factor(Yield_Quantile, levels = 1:5, labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4", "Q5 (Highest)"))
    ) |>
    ungroup()

p <- ggplot(D_plot, aes(x = a_CO2_total_mg_L, y = C_P, color = Yield_Quantile)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), aes(group = 1), color = "black", size = 1.2, se = FALSE) +
    scale_color_viridis_d(option = "plasma", direction = -1) +
    facet_wrap(~crop, scales = "free") +
    theme_minimal() +
    labs(
        title = "Tissue Concentration vs Intensity",
        subtitle = "Points colored by Yield Quantile within each crop",
        x = "Soil P Intensity (aCO2 mg/L)",
        y = "System Tissue Concentration C_P (mg/g)",
        color = "Yield Quantile"
    ) +
    theme(
        legend.position = "bottom",
        strip.text = element_text(size = 12, face = "bold")
    )

ggsave("scratch/cp_vs_intensity.png", p, width = 12, height = 8, dpi = 300)
cat("Saved to scratch/cp_vs_intensity.png\n")
