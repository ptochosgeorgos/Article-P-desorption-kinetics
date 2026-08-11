library(ggplot2)
library(dplyr)

final_obj <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_obj$data$D_Long_Agro

# Filter to major crops
target_crops <- c("WW", "KA", "ZR", "RA", "KM")

D_plot <- D_Long_Agro |>
    filter(crop %in% target_crops) |>
    mutate(
        C_P = annual_P_uptake / annual_yield_mp_DM,
        Yield = annual_yield_mp_DM
    ) |>
    filter(is.finite(C_P), C_P > 0, Yield > 0)

# The classic Steenbjerg plot is Tissue Concentration (C_P) on the Y-axis 
# and Yield (or Dry Matter) on the X-axis.
# If the effect exists, C_P should start high at very low yields, 
# drop to a minimum as yield increases (dilution), and then rise (luxury).

p <- ggplot(D_plot, aes(x = Yield, y = C_P)) +
    geom_point(alpha = 0.3, color = "darkgray") +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "red", size = 1.5, se = TRUE) +
    facet_wrap(~crop, scales = "free") +
    theme_minimal() +
    labs(
        title = "Empirical Evidence of the Steenbjerg Effect",
        subtitle = "Tissue Concentration (C_P) vs. Yield across major crops",
        x = "Yield (DM t/ha)",
        y = "Tissue Concentration C_P (mg/g)"
    )

ggsave("scratch/steenbjerg_evidence.png", p, width = 12, height = 8, dpi = 300)
cat("Saved to scratch/steenbjerg_evidence.png\n")
