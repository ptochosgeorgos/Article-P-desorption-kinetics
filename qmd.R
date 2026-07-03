# and an annual random effect to capture unmeasured temporal variation:
#
#   c_eff = (c_base + u_year) * exp(β_invb * z_inv_b + β_pH * z_pH + ...)
#   Y = 1 − exp(−c_eff * P_CO2)
#
# A negative β implies that the environmental driver LOWERS the P-foraging efficiency,
# meaning the crop yield rises more slowly per unit of P_CO2 in the soil.

m_yield_raw_co2 <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_invb * z_inv_b + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_CO2 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_invb = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0, E_base = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_thm_co2 <- nlme(
