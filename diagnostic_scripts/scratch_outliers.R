source("notebooks/qi_modelling1.R")
library(dplyr)

# quad_co2 is already calculated in the global environment if we source properly?
# Actually run_loocv_quadrants is defined but we must run it
quad_co2 <- run_loocv_quadrants(D_Yield, "soil_0_20_P_CO2", "z_inv_b")

def_data <- quad_co2 |>
    dplyr::filter(Quadrant == "True Negative (Correct Warning)")

def_data$ln_P_CO2_actual <- def_data$ln_P_CO2
def_data$ln_P_CO2 <- log(def_data$P_crit_loo)
def_data$pred_ln_Q <- predict(ptf_practical_raw, newdata = def_data, re.form = NA)
def_data$ln_P_CO2 <- def_data$ln_P_CO2_actual

def_data <- def_data |>
    dplyr::mutate(
        Q_crit = exp(pred_ln_Q),
        Delta_Q = Q_crit - soil_0_20_P_AAE10,
        Yield_Gap = 0.95 - Relative_Yield
    )

def_data_acidic <- def_data |> dplyr::filter(rollMean_soil_0_20_pH_H2O < 7.20)

cat("Summary of Delta_Q in Acidic:\n")
print(summary(def_data_acidic$Delta_Q))

outliers <- def_data_acidic |> filter(Delta_Q > 150)
cat("\nNumber of outliers (Delta_Q > 150 mg/kg):", nrow(outliers), "\n")

if(nrow(outliers) > 0) {
    cat("\nBreakdown of Outliers by Site:\n")
    print(table(outliers$site))
    
    cat("\nBreakdown of Outliers by Crop:\n")
    print(table(outliers$crop))
    
    cat("\nSummary of pedoclimatic covariates for Outliers:\n")
    print(summary(outliers |> select(site, crop, rollMean_soil_0_20_pH_H2O, rollMean_soil_0_20_clay, z_ln_FineTexture, inv_b, soil_0_20_P_CO2, P_crit_loo, Delta_Q)))
    
    # Let's see the worst offenders
    worst <- outliers |> arrange(desc(Delta_Q)) |> head(10)
    cat("\nTop 10 Worst Offenders:\n")
    print(worst |> select(site, crop, rollMean_soil_0_20_pH_H2O, z_ln_FineTexture, P_crit_loo, soil_0_20_P_AAE10, Q_crit, Delta_Q))
}

