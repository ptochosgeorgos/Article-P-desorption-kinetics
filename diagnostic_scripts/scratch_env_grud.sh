cat << 'EOF' > scratch_env_grud.R
source("notebooks/qi_modelling1.R")
library(ggplot2)
library(dplyr)
library(patchwork)

# 1. We want to predict Q_safe for a grid of FineTexture and Corg
# To isolate their effects, we will hold all other covariates at their mean (0 in scaled space)
# except FineTexture and C_org, which we will vary across their empirical ranges.

# Set the Danger Intensity limit
P_CO2_env <- 1.0
ln_P_CO2_env <- log(P_CO2_env)

# Get the empirical ranges of the unscaled variables
min_ft <- min(D_ready$rollMean_soil_0_20_clay + D_ready$rollMean_soil_0_20_silt, na.rm=TRUE)
max_ft <- max(D_ready$rollMean_soil_0_20_clay + D_ready$rollMean_soil_0_20_silt, na.rm=TRUE)

min_corg <- min(D_ready$rollMean_soil_0_20_Corg, na.rm=TRUE)
max_corg <- max(D_ready$rollMean_soil_0_20_Corg, na.rm=TRUE)

# We need the scaling center and scale for FineTexture and Corg to unscale the axes later, or we can just use the scaled variables
scale_center_ft <- attr(scale(log(D_ready$rollMean_soil_0_20_clay + D_ready$rollMean_soil_0_20_silt)), "scaled:center")
scale_scale_ft <- attr(scale(log(D_ready$rollMean_soil_0_20_clay + D_ready$rollMean_soil_0_20_silt)), "scaled:scale")

scale_center_corg <- attr(scale(log(D_ready$rollMean_soil_0_20_Corg)), "scaled:center")
scale_scale_corg <- attr(scale(log(D_ready$rollMean_soil_0_20_Corg)), "scaled:scale")

# Create a grid
grid <- expand.grid(
    FineTexture_unscaled = seq(min_ft, max_ft, length.out=50),
    Corg_unscaled = seq(min_corg, max_corg, length.out=5)
)

grid$z_ln_FineTexture <- (log(grid$FineTexture_unscaled) - scale_center_ft) / scale_scale_ft
grid$z_ln_Corg <- (log(grid$Corg_unscaled) - scale_center_corg) / scale_scale_corg

# Set other covariates to mean (0)
grid$ln_P_CO2 <- ln_P_CO2_env
grid$z_pH <- 0
grid$z_ln_Ca <- 0
grid$z_ln_Mg <- 0
grid$z_ln_K <- 0
grid$z_Temp_Anom <- 0
grid$z_Prec_Anom <- 0
grid$z_Temp_Mean <- 0

# Predict Q_safe (P_AAE10)
# using ptf_practical_raw
grid$pred_ln_Q <- predict(ptf_practical_raw, newdata = grid, re.form = NA)
grid$Q_safe <- exp(grid$pred_ln_Q)

# Plot
p <- ggplot(grid, aes(x = FineTexture_unscaled, y = Q_safe, color = as.factor(round(Corg_unscaled, 2)))) +
    geom_line(linewidth=1.2) +
    labs(
        title = "PTF-Derived Environmental Limits (Q_safe) vs GRUD Parameters",
        x = "Fine Texture (% Clay + Silt)",
        y = "Maximum Safe P_AAE10 (mg/kg)",
        color = "C_org (%)"
    ) +
    theme_minimal()

ggsave("scratch_env_grud.png", p, width=8, height=6)
cat("Plot generated: scratch_env_grud.png\n")

# Let's also check the actual coefficients in the PTF to explain the mechanism
cf <- fixef(ptf_practical_raw)
cat("\nPTF Coefficients for FineTexture and Corg:\n")
cat("Intercept Shift (Quantity Multiplier):\n")
cat("z_ln_FineTexture:", cf["z_ln_FineTexture"], "\n")
cat("z_ln_Corg:", cf["z_ln_Corg"], "\n")
cat("\nInteraction with ln_P_CO2 (Slope Modulator):\n")
cat("ln_P_CO2:z_ln_FineTexture:", cf["ln_P_CO2:z_ln_FineTexture"], "\n")
cat("ln_P_CO2:z_ln_Corg:", cf["ln_P_CO2:z_ln_Corg"], "\n")

EOF
Rscript scratch_env_grud.R
