source("notebooks/qi_modelling1.R")

cat("Trying m_yield_raw_aae with scaled starts...\n")
start_co2 <- fixef(m_yield_raw_co2)

# Parameters 1 to 9 are c_base.(Intercept) and c_base.crop*
# Parameter 17 is E_base
start_aae <- start_co2
start_aae[1:9] <- start_co2[1:9] / 25  # P_AAE10 is ~25x larger
start_aae[17] <- start_co2[17] * 25    # E_base must scale up by 25x

m_yield_raw_aae <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_invb * z_inv_b +
        beta_pH * z_pH +
        beta_K * z_ln_K +
        beta_Mg * z_ln_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean +
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_AAE10 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_K ~ 1, beta_Mg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = unname(start_aae),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

print(round(summary(m_yield_raw_aae)$tTable, 4))
cat("\nPseudo-R2:", round(cor(D_Yield$Relative_Yield, predict(m_yield_raw_aae, level = 2))^2, 3), "\n")
cat("RMSE (Conditional):", round(sqrt(mean(residuals(m_yield_raw_aae, level = 2)^2)), 3), "\n")
cat("RMSE (Marginal):", round(sqrt(mean(residuals(m_yield_raw_aae, level = 0)^2)), 3), "\n")
