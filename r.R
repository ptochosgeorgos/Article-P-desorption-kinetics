#   Y = 1 − exp(−c_eff * P_CO2)
#
# A negative β implies that the environmental driver LOWERS the P-foraging efficiency,
# meaning the crop yield rises more slowly per unit of P_CO2 in the soil.

m_yield_nlme <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_invb * z_inv_b + 
        beta_pH * z_pH + 
        beta_K * z_ln_K +
        beta_Mg * z_ln_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_CO2 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_K ~ 1, beta_Mg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1), 0, 0, 0, 0, 0, 0, 0, 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

cat("### Yield ~ P_CO2 (Mitscherlich NLME with 1/b and Pedoclimatic drivers) ###\n")
print(round(summary(m_yield_nlme)$tTable, 4))
cat("\nPseudo-R2:", round(cor(D_Yield$Relative_Yield, predict(m_yield_nlme))^2, 3), "\n")
cat("RMSE:", round(sqrt(mean(residuals(m_yield_nlme)^2)), 3), "\n")
