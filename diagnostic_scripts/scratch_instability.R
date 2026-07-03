source("notebooks/qi_modelling1.R")

# Reconstruct def_data
def_data <- quad_co2 |>
    dplyr::filter(Quadrant == "True Negative (Correct Warning)")

def_data$ln_P_CO2_actual <- def_data$ln_P_CO2
def_data$ln_P_CO2 <- log(def_data$P_crit_loo)
def_data$pred_ln_Q <- predict(ptf_practical_raw, newdata = def_data, re.form = NA)
def_data$ln_P_CO2 <- def_data$ln_P_CO2_actual
def_data <- def_data |>
    dplyr::mutate(
        Q_crit = exp(pred_ln_Q),
        Delta_Q = Q_crit - soil_0_20_P_AAE10
    )

# Identify extreme outliers
outliers <- def_data |> dplyr::filter(Delta_Q > 500) |> dplyr::arrange(desc(Delta_Q))

cat("Number of extreme outliers (Delta Q > 500):", nrow(outliers), "\n")
if(nrow(outliers) > 0) {
    print(outliers |> dplyr::select(site, Delta_Q, P_crit_loo, Relative_Yield, rollMean_soil_0_20_pH_H2O, z_pH, z_inv_b, z_Temp_Mean, z_Prec_Anom, z_ln_FineTexture) |> head(10))
}

# Correlation of covariates with Delta_Q
corr_data <- def_data |> dplyr::select(Delta_Q, P_crit_loo, rollMean_soil_0_20_pH_H2O, z_pH, z_inv_b, z_Temp_Mean, z_Prec_Anom, z_Temp_Anom, z_ln_FineTexture, z_ln_Corg)
cat("\nCorrelation with Delta_Q (Spearman):\n")
print(cor(corr_data, use="pairwise.complete.obs", method="spearman")[,"Delta_Q"])

