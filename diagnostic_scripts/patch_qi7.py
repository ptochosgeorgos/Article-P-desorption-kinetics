import re

with open("notebooks/qi_modelling1.qmd", "r") as f:
    content = f.read()

# 1. Update D_Yield
yield_data_orig = """    filter(is.finite(inv_b), is.finite(Relative_Yield), Relative_Yield > 0) |>
    mutate(
        z_inv_b = as.numeric(scale(inv_b)),
        z_n = as.numeric(scale(n_pred_agro)),
        z_fert_N = as.numeric(scale(fert_N_tot)),"""

yield_data_new = """    filter(is.finite(inv_b), is.finite(Relative_Yield), Relative_Yield > 0) |>
    mutate(
        z_inv_b = as.numeric(scale(inv_b)),
        z_n = as.numeric(scale(n_pred_agro)),
        z_b = as.numeric(scale(b_power)),
        z_fert_N = as.numeric(scale(fert_N_tot)),"""

content = content.replace(yield_data_orig, yield_data_new)


# 2. Add Yield Models
yield_models_orig = """# ---------------------------------------------------------
# NULL MODELS (NO 1/b PENALTY) FOR YIELD COMPARISON
# ---------------------------------------------------------"""

yield_models_new = """# ---------------------------------------------------------
# b-PARAMETER MODELS (Buffer b instead of 1/b) FOR YIELD COMPARISON
# ---------------------------------------------------------
m_yield_raw_co2_b <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_b * z_b + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * soil_0_20_P_CO2),
    data = D_Yield,
    fixed = c_base + beta_b + beta_pH + beta_fertK + beta_fertMg + beta_N + beta_Temp + beta_Prec ~ 1,
    random = c_base ~ 1 | site,
    start = c(c_base = 1.2, beta_b = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_thm_co2_b <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_b * z_b + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * a_CO2_total_mg_L),
    data = D_Yield,
    fixed = c_base + beta_b + beta_pH + beta_fertK + beta_fertMg + beta_N + beta_Temp + beta_Prec ~ 1,
    random = c_base ~ 1 | site,
    start = c(c_base = 1.2, beta_b = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_raw_aae_b <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_b * z_b + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * soil_0_20_P_AAE10),
    data = D_Yield,
    fixed = c_base + beta_b + beta_pH + beta_fertK + beta_fertMg + beta_N + beta_Temp + beta_Prec ~ 1,
    random = c_base ~ 1 | site,
    start = c(c_base = 1.2, beta_b = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

# ---------------------------------------------------------
# NULL MODELS (NO 1/b PENALTY) FOR YIELD COMPARISON
# ---------------------------------------------------------"""

content = content.replace(yield_models_orig, yield_models_new)


# 3. Update extract_yield function
ext_yield_orig = """    p_invb <- if("beta_invb" %in% rownames(tt)) round(tt["beta_invb", "p-value"], 4) else NA
    p_n    <- if("beta_n" %in% rownames(tt)) round(tt["beta_n", "p-value"], 4) else NA
    data.frame(Model = name, Marginal_R2 = r2_m, AIC = aic,
               p_val_Physical_1b = p_invb,
               p_val_Freundlich_n = p_n,
               p_val_fertK = round(tt["beta_fertK", "p-value"], 4),"""

ext_yield_new = """    p_invb <- if("beta_invb" %in% rownames(tt)) round(tt["beta_invb", "p-value"], 4) else NA
    p_n    <- if("beta_n" %in% rownames(tt)) round(tt["beta_n", "p-value"], 4) else NA
    p_b    <- if("beta_b" %in% rownames(tt)) round(tt["beta_b", "p-value"], 4) else NA
    data.frame(Model = name, Marginal_R2 = r2_m, AIC = aic,
               p_val_Physical_1b = p_invb,
               p_val_Freundlich_n = p_n,
               p_val_Buffer_b = p_b,
               p_val_fertK = round(tt["beta_fertK", "p-value"], 4),"""

content = content.replace(ext_yield_orig, ext_yield_new)


# 4. Update yield_table
yield_tab_orig = """    extract_yield(m_yield_raw_aae_n, "3. Freundlich n - Legacy P_AAE10", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_co2_null, "1. Null Model - Raw P_CO2", D_Yield$Relative_Yield),"""

yield_tab_new = """    extract_yield(m_yield_raw_aae_n, "3. Freundlich n - Legacy P_AAE10", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_co2_b, "1. Buffer b - Raw P_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_thm_co2_b, "2. Buffer b - Thermo a_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_aae_b, "3. Buffer b - Legacy P_AAE10", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_co2_null, "1. Null Model - Raw P_CO2", D_Yield$Relative_Yield),"""

content = content.replace(yield_tab_orig, yield_tab_new)


# 5. Update D_Cum
dcum_orig = """        mean_P_AAE10 = mean(soil_0_20_P_AAE10, na.rm = TRUE),
        z_inv_b_agro = mean(inv_b_agro, na.rm = TRUE),
        mean_n_agro = mean(n_pred_agro, na.rm = TRUE),"""

dcum_new = """        mean_P_AAE10 = mean(soil_0_20_P_AAE10, na.rm = TRUE),
        z_inv_b_agro = mean(inv_b_agro, na.rm = TRUE),
        mean_n_agro = mean(n_pred_agro, na.rm = TRUE),
        mean_b_agro = mean(b_power_agro, na.rm = TRUE),"""

content = content.replace(dcum_orig, dcum_new)

dcum_mut_orig = """    mutate(
        z_inv_b = as.numeric(scale(z_inv_b_agro)),
        z_n = as.numeric(scale(mean_n_agro)),
        ln_P_CO2 = log(mean_P_CO2),"""

dcum_mut_new = """    mutate(
        z_inv_b = as.numeric(scale(z_inv_b_agro)),
        z_n = as.numeric(scale(mean_n_agro)),
        z_b = as.numeric(scale(mean_b_agro)),
        ln_P_CO2 = log(mean_P_CO2),"""

content = content.replace(dcum_mut_orig, dcum_mut_new)


# 6. Add Balance Models
bal_models_orig = """# 4. Fit Null Models
m_bal_co2_null <- lmer(Cumulated_P_Balance ~ ln_P_CO2 + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)"""

bal_models_new = """# 4. Fit Buffer b Models
m_bal_co2_b <- lmer(Cumulated_P_Balance ~ ln_P_CO2 * z_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_thm_b <- lmer(Cumulated_P_Balance ~ ln_a_CO2 * z_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_aae_b <- lmer(Cumulated_P_Balance ~ ln_P_AAE10 * z_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)

# 5. Fit Null Models
m_bal_co2_null <- lmer(Cumulated_P_Balance ~ ln_P_CO2 + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)"""

content = content.replace(bal_models_orig, bal_models_new)


# 7. Update bal_table
bal_tab_orig = """    extract_bal(m_bal_aae_n, "3. Freundlich n - Legacy P_AAE10"),
    extract_bal(m_bal_co2_null, "1. Null - Raw P_CO2 (No 1/b)"),"""

bal_tab_new = """    extract_bal(m_bal_aae_n, "3. Freundlich n - Legacy P_AAE10"),
    extract_bal(m_bal_co2_b, "1. Buffer b - Raw P_CO2"),
    extract_bal(m_bal_thm_b, "2. Buffer b - Thermo a_CO2"),
    extract_bal(m_bal_aae_b, "3. Buffer b - Legacy P_AAE10"),
    extract_bal(m_bal_co2_null, "1. Null - Raw P_CO2 (No 1/b)"),"""

content = content.replace(bal_tab_orig, bal_tab_new)

with open("notebooks/qi_modelling1.qmd", "w") as f:
    f.write(content)

