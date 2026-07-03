import re

with open("notebooks/qi_modelling1.qmd", "r") as f:
    content = f.read()

# 1. Update D2 to include fert_P_tot and annual_P_balance
d2_orig = """    mutate(
        soil_0_20_P_CO2 = soil_0_20_P_test * 0.155,
        crop = crop_abr,
        annual_P_uptake = rowSums(across(starts_with("P_harv")), na.rm = TRUE),
        annual_yield_mp_DM = rowSums(across(matches("^harv.*mp_yield_DM$")), na.rm = TRUE),
        annual_yield_bp_DM = rowSums(across(matches("^harv.*bp[1-2]_yield_DM$")), na.rm = TRUE)
    ) # 40-Year Full Dataset (All Treatments)"""

d2_new = """    mutate(
        soil_0_20_P_CO2 = soil_0_20_P_test * 0.155,
        crop = crop_abr,
        annual_P_uptake = rowSums(across(starts_with("P_harv")), na.rm = TRUE),
        fert_P_tot = fert_P2O5_tot / 2.291,
        annual_P_balance = fert_P_tot - annual_P_uptake,
        annual_yield_mp_DM = rowSums(across(matches("^harv.*mp_yield_DM$")), na.rm = TRUE),
        annual_yield_bp_DM = rowSums(across(matches("^harv.*bp[1-2]_yield_DM$")), na.rm = TRUE)
    ) # 40-Year Full Dataset (All Treatments)"""

content = content.replace(d2_orig, d2_new)

# 2. Plant Uptake Models Comparison text
uptake_orig = """## 7. Phase 5: Plant Uptake Models Comparison

We calculate the physical inverse buffer power ($1/b$) for all harvests from 2010 to 2022. Because our goal is a practical field tool, we calculate $1/b$ twice: once using the rigorous **Geochemical PTF** (which requires `Feox/Alox`), and once using our generalized **Practical Agronomic PTF** (which uses routinely available data)."""

uptake_new = """## 7. Phase 5: Plant Uptake Models Comparison

### Mechanistic Hypothesis: The Diffusion Bottleneck ($D_e \\propto 1/b$)
The rate-limiting step for P acquisition is diffusion through the soil matrix to the root surface. The effective diffusion coefficient ($D_e$) is directly proportional to $1/b$. Thus, $1/b$ acts as a **Diffusion Bottleneck**. Soils with high buffer capacity (low $1/b$) replenish the root depletion zone too slowly. Therefore, even if two soils have the exact same bulk $P_{CO2}$ concentration, the plant in the high-buffer soil will uptake less P because the dynamic supply to the root surface is restricted.

To statistically prove this, we compare "Full" models (where $1/b$ penalizes the Michaelis-Menten affinity constant) against "Null" models (which omit the $1/b$ modifier entirely). If the diffusion bottleneck is real, the Full models should exhibit significantly higher Marginal $R^2$ values.

We calculate the physical inverse buffer power ($1/b$) for all harvests from 2010 to 2022. Because our goal is a practical field tool, we calculate $1/b$ twice: once using the rigorous **Geochemical PTF** (which requires `Feox/Alox`), and once using our generalized **Practical Agronomic PTF** (which uses routinely available data)."""

content = content.replace(uptake_orig, uptake_new)

# 3. Add Null models for Agro dataset (since practical model is the one highlighted)
# Wait, let's add Null models for the Agro dataset right after mod_raw_aae_agro_den.
agro_models_orig = """mod_raw_aae_agro_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_AAE10) /
        ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + soil_0_20_P_AAE10),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + K_base + beta_invb + beta_v0 ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Agro$soil_0_20_P_AAE10), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)"""

agro_models_new = agro_models_orig + """

# ---------------------------------------------------------
# NULL MODELS (NO 1/b PENALTY) FOR COMPARISON
# ---------------------------------------------------------
mod_raw_co2_agro_null <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) /
        (K_base + soil_0_20_P_CO2),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Agro$soil_0_20_P_CO2)), control = nlmeControl(maxIter = 1000)
)

mod_thm_co2_agro_null <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * a_CO2_total_mg_L) /
        (K_base + a_CO2_total_mg_L),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Agro$a_CO2_total_mg_L)), control = nlmeControl(maxIter = 1000)
)

mod_raw_aae_agro_null <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_AAE10) /
        (K_base + soil_0_20_P_AAE10),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Agro$soil_0_20_P_AAE10)), control = nlmeControl(maxIter = 1000)
)"""

content = content.replace(agro_models_orig, agro_models_new)

# 4. Update extract_perf to handle missing beta_invb
extract_perf_orig = """extract_perf <- function(mod, name, y) {
    preds_c <- predict(mod)
    preds_m <- predict(mod, level = 0)
    r2_c <- round(cor(y, preds_c)^2, 3)
    r2_m <- round(cor(y, preds_m)^2, 3)
    aic <- round(AIC(mod), 1)
    tt <- summary(mod)$tTable
    est_invb <- round(tt["beta_invb", "Value"], 4)
    p_invb <- round(tt["beta_invb", "p-value"], 4)
    p_N    <- round(tt["beta_N",    "p-value"], 4)
    p_v0   <- round(tt["beta_v0",   "p-value"], 4)
    data.frame(Model = name, Marginal_R2 = r2_m, Conditional_R2 = r2_c, AIC = aic, Beta_1b_Estimate = est_invb, p_val_Physical_1b = p_invb, p_val_J0 = p_v0)
}"""

extract_perf_new = """extract_perf <- function(mod, name, y) {
    preds_c <- predict(mod)
    preds_m <- predict(mod, level = 0)
    r2_c <- round(cor(y, preds_c)^2, 3)
    r2_m <- round(cor(y, preds_m)^2, 3)
    aic <- round(AIC(mod), 1)
    tt <- summary(mod)$tTable
    est_invb <- if("beta_invb" %in% rownames(tt)) round(tt["beta_invb", "Value"], 4) else NA
    p_invb <- if("beta_invb" %in% rownames(tt)) round(tt["beta_invb", "p-value"], 4) else NA
    p_N    <- if("beta_N" %in% rownames(tt)) round(tt["beta_N", "p-value"], 4) else NA
    p_v0   <- if("beta_v0" %in% rownames(tt)) round(tt["beta_v0", "p-value"], 4) else NA
    data.frame(Model = name, Marginal_R2 = r2_m, Conditional_R2 = r2_c, AIC = aic, Beta_1b_Estimate = est_invb, p_val_Physical_1b = p_invb, p_val_J0 = p_v0)
}"""

content = content.replace(extract_perf_orig, extract_perf_new)

# 5. Add to res_table
res_table_orig = """    extract_perf(mod_raw_aae_agro, "3. Agro PBC - Legacy P_AAE10 (Num J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_aae_agro_den, "3. Agro PBC - Legacy P_AAE10 (Den J0)", D_Long_Agro$Relative_Uptake)
)"""

res_table_new = """    extract_perf(mod_raw_aae_agro, "3. Agro PBC - Legacy P_AAE10 (Num J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_aae_agro_den, "3. Agro PBC - Legacy P_AAE10 (Den J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_co2_agro_null, "1. Null Model - Raw P_CO2 (No 1/b)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_thm_co2_agro_null, "2. Null Model - Thermo a_CO2 (No 1/b)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_aae_agro_null, "3. Null Model - Legacy P_AAE10 (No 1/b)", D_Long_Agro$Relative_Uptake)
)"""

content = content.replace(res_table_orig, res_table_new)

# Update pack_rows in table slightly to include null models
res_table_format_orig = """    pack_rows("Raw Empirical (P_CO2)", 1, 2) |>
    pack_rows("Thermodynamic (a_CO2)", 3, 4) |>
    pack_rows("Bound Legacy (P_AAE10)", 5, 6)"""
res_table_format_new = """    pack_rows("Raw Empirical (P_CO2)", 1, 2) |>
    pack_rows("Thermodynamic (a_CO2)", 3, 4) |>
    pack_rows("Bound Legacy (P_AAE10)", 5, 6) |>
    pack_rows("Null Models (No 1/b Penalty)", 13, 15)"""
# actually there are 12 models before the null models. 
# Geo Num, Geo Den, Agro Num, Agro Den (x3 pools) = 12 models.
content = content.replace(res_table_format_orig, res_table_format_new)

with open("notebooks/qi_modelling1.qmd", "w") as f:
    f.write(content)
