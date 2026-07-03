source("notebooks/qi_modelling1.R")
cat("m_yield_raw_co2 DONE\n")

cat("Trying m_yield_thm_co2...\n")
m_yield_thm_co2 <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_invb * z_inv_b +
        beta_pH * z_pH +
        beta_K * z_ln_K +
        beta_Mg * z_ln_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean +
        beta_Prec * z_Prec_Anom
    )) * (a_CO2_total_mg_L + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_K ~ 1, beta_Mg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(levels(D_Yield$crop)) - 1), 0, 0, 0, 0, 0, 0, 0, 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)
cat("m_yield_thm_co2 DONE\n")

cat("Trying m_yield_raw_aae...\n")
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
    start = c(0.1, rep(0, length(levels(D_Yield$crop)) - 1), 0, 0, 0, 0, 0, 0, 0, 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)
cat("m_yield_raw_aae DONE\n")
