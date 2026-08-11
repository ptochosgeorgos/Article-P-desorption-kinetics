library(nlme)
library(ggplot2)
library(dplyr)

options(warn=-1)

final_obj <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_obj$data$D_Long_Agro

D_CP <- D_Long_Agro |>
    mutate(
        C_P = annual_P_uptake / annual_yield_mp_DM,
        crop_factor = as.factor(crop)
    ) |>
    filter(is.finite(C_P), C_P > 0, a_CO2_total_mg_L > 0) |>
    filter(!is.na(z_pH), !is.na(z_ln_FineTexture), !is.na(z_inv_b_agro), !is.na(z_v0))

n_crops <- length(levels(D_CP$crop_factor))
start_vec <- c(rep(1.5, n_crops), rep(0.05, n_crops), 0, 0, 2.0, 8.0, 0, 0)

# Fit Unified Model
mod_unified <- try(nlme(
    C_P ~ C_base + beta_pH * z_pH + beta_tex * z_ln_FineTexture + 
          A_dil * exp(-k_dil * a_CO2_total_mg_L) + 
          (S_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) * a_CO2_total_mg_L,
    data = D_CP,
    fixed = list(
        C_base ~ crop_factor - 1,
        S_base ~ crop_factor - 1,
        beta_pH + beta_tex + A_dil + k_dil + beta_invb + beta_v0 ~ 1
    ),
    random = C_base ~ 1 | site,
    start = start_vec,
    control = nlmeControl(maxIter = 1500, pnlsMaxIter = 200, msMaxIter = 400, returnObject = TRUE, opt="nlminb")
), silent=TRUE)

if(inherits(mod_unified, "try-error")) {
    stop("NLME fit failed.")
}

target_crops <- c("WW", "KA", "ZR", "RA", "KM")

# Generate smooth prediction lines for the target crops.
# We set covariates to 0 (mean) to show the population average hook.
pred_df <- expand.grid(
    crop_factor = factor(target_crops, levels = levels(D_CP$crop_factor)),
    a_CO2_total_mg_L = seq(0, max(D_CP$a_CO2_total_mg_L), length.out = 200),
    z_pH = 0,
    z_ln_FineTexture = 0,
    z_inv_b_agro = 0,
    z_v0 = 0
)

pred_df$C_P_pred <- predict(mod_unified, newdata = pred_df, level = 0)

# Prepare empirical data for plotting
D_plot <- D_CP |>
    filter(crop %in% target_crops) |>
    group_by(crop) |>
    mutate(
        Yield_Quantile = ntile(annual_yield_mp_DM, 5),
        Yield_Quantile = factor(Yield_Quantile, levels = 1:5, labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4", "Q5 (Highest)"))
    ) |>
    ungroup()

p <- ggplot(D_plot, aes(x = a_CO2_total_mg_L)) +
    geom_point(aes(y = C_P, color = Yield_Quantile), alpha = 0.5, size = 1.5) +
    geom_line(data = pred_df, aes(y = C_P_pred), color = "black", linewidth = 1.2) +
    scale_color_viridis_d(option = "plasma", direction = -1) +
    # free_y allows distinct tissue concentration scales, but x is fixed so we see the full intensity range
    facet_wrap(~crop_factor, scales = "free_y") +
    theme_minimal() +
    labs(
        title = "Tissue Concentration vs Intensity (with Unified NLME Prediction)",
        subtitle = "Points colored by Yield Quantile. Black line is the population-level NLME prediction.",
        x = "Soil P Intensity (aCO2 mg/L)",
        y = "System Tissue Concentration C_P (mg/g)",
        color = "Yield Quantile"
    ) +
    theme(
        legend.position = "bottom",
        strip.text = element_text(size = 12, face = "bold")
    )

ggsave("scratch/cp_vs_intensity_nlme.png", p, width = 12, height = 8, dpi = 300)
cat("Saved to scratch/cp_vs_intensity_nlme.png\n")
