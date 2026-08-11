library(ggplot2)
library(dplyr)
library(tidyr)

final_obj <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_obj$data$D_Long_Agro

target_crops <- c("WW", "KA", "ZR", "RA", "KM")

D_plot <- D_Long_Agro |>
    filter(crop %in% target_crops) |>
    mutate(
        CP_System = annual_P_uptake / annual_yield_mp_DM,
        CP_MP = P_harv1_mp / annual_yield_mp_DM,
        CP_BP = P_harv1_bp1 / annual_yield_bp_DM
    ) |>
    filter(annual_yield_mp_DM > 0) |>
    select(crop, annual_yield_mp_DM, annual_yield_bp_DM, CP_System, CP_MP, CP_BP)

# Reshape for plotting
D_long <- D_plot |>
    pivot_longer(cols = c("CP_System", "CP_MP", "CP_BP"), names_to = "Component", values_to = "Concentration") |>
    filter(is.finite(Concentration), Concentration > 0) |>
    mutate(
        Yield_X = ifelse(Component == "CP_BP", annual_yield_bp_DM, annual_yield_mp_DM)
    ) |>
    filter(is.finite(Yield_X), Yield_X > 0)

# Rename for clear labels
D_long$Component <- factor(D_long$Component, levels = c("CP_System", "CP_MP", "CP_BP"), 
                           labels = c("System (Total P / MP Yield)", "Main Product (Grain/Tuber)", "Byproduct (Straw/Leaves)"))

p <- ggplot(D_long, aes(x = Yield_X, y = Concentration, color = Component)) +
    geom_point(alpha = 0.15) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 1.2, se = FALSE) +
    facet_wrap(~crop, scales = "free") +
    theme_minimal() +
    labs(
        title = "Where does Luxury Consumption happen?",
        subtitle = "Comparing Tissue Concentrations of Main Product vs Byproduct",
        x = "Yield (DM t/ha)",
        y = "Tissue Concentration (mg/g)",
        color = "Plant Component"
    )

ggsave("scratch/cp_components.png", p, width = 12, height = 8, dpi = 300)
cat("Saved to scratch/cp_components.png\n")
