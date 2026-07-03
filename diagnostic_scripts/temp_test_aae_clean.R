source("notebooks/qi_modelling1.R")

cat("Trying m_yield_raw_aae with simple starts...\n")

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
    start = c(0.04, rep(0, 8), rep(0, 7), 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)
cat("Pseudo-R2:", round(cor(D_Yield$Relative_Yield, predict(m_yield_raw_aae, level = 2))^2, 3), "\n")
