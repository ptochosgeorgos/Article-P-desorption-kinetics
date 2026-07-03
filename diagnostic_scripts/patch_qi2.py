import re

with open("notebooks/qi_modelling1.qmd", "r") as f:
    content = f.read()

# 1. Yield Hypothesis
yield_orig = """## 7. The Yield-STP Comparison (Mitscherlich)

### Modelling rationale: path to the parsimonious model"""

yield_new = """## 7. The Yield-STP Comparison (Mitscherlich)

### Mechanistic Hypothesis: The Efficiency Penalty
Yield is the long-term biological integration of daily plant uptake, capped by environmental constraints (Mitscherlich asymptote). Because it is a downstream consequence of uptake, it inherits the same physical diffusion limitations. Here, $1/b$ acts as an **Efficiency Penalty** on the Mitscherlich rate constant ($c$). If the soil cannot physically supply P fast enough during critical early growth phases due to low $1/b$, the crop's yield potential is stunted. However, because the plant can store and reallocate P internally over the season, we expect the sensitivity of Yield to $1/b$ to be slightly dampened compared to instantaneous Uptake.

To statistically prove this penalty, we compare "Full" Mitscherlich models (where $1/b$ modifies the rate constant) against "Null" models (which omit $1/b$).

### Modelling rationale: path to the parsimonious model"""

content = content.replace(yield_orig, yield_new)

# 2. Add Null models for Yield
aae_model_orig = """m_yield_raw_aae <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_invb * z_inv_b + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * soil_0_20_P_AAE10),
    data = D_Yield,
    fixed = c_base + beta_invb + beta_pH + beta_fertK + beta_fertMg + beta_N + beta_Temp + beta_Prec ~ 1,
    random = c_base ~ 1 | site,
    start = c(c_base = 1.2, beta_invb = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)"""

aae_model_new = aae_model_orig + """

# ---------------------------------------------------------
# NULL MODELS (NO 1/b PENALTY) FOR YIELD COMPARISON
# ---------------------------------------------------------
m_yield_raw_co2_null <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * soil_0_20_P_CO2),
    data = D_Yield,
    fixed = c_base + beta_pH + beta_fertK + beta_fertMg + beta_N + beta_Temp + beta_Prec ~ 1,
    random = c_base ~ 1 | site,
    start = c(c_base = 1.2, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_thm_co2_null <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * a_CO2_total_mg_L),
    data = D_Yield,
    fixed = c_base + beta_pH + beta_fertK + beta_fertMg + beta_N + beta_Temp + beta_Prec ~ 1,
    random = c_base ~ 1 | site,
    start = c(c_base = 1.2, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_raw_aae_null <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * soil_0_20_P_AAE10),
    data = D_Yield,
    fixed = c_base + beta_pH + beta_fertK + beta_fertMg + beta_N + beta_Temp + beta_Prec ~ 1,
    random = c_base ~ 1 | site,
    start = c(c_base = 1.2, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)"""

content = content.replace(aae_model_orig, aae_model_new)

# 3. Update extract_yield
ext_orig = """extract_yield <- function(mod, name, y) {
    preds_m <- predict(mod, level = 0)
    r2_m <- round(cor(y, preds_m)^2, 3)
    aic <- round(AIC(mod), 1)
    tt <- summary(mod)$tTable
    data.frame(Model = name, Marginal_R2 = r2_m, AIC = aic,
               p_val_Physical_1b = round(tt["beta_invb", "p-value"], 4),
               p_val_fertK = round(tt["beta_fertK", "p-value"], 4),
               p_val_fertMg = round(tt["beta_fertMg", "p-value"], 4))
}"""

ext_new = """extract_yield <- function(mod, name, y) {
    preds_m <- predict(mod, level = 0)
    r2_m <- round(cor(y, preds_m)^2, 3)
    aic <- round(AIC(mod), 1)
    tt <- summary(mod)$tTable
    p_invb <- if("beta_invb" %in% rownames(tt)) round(tt["beta_invb", "p-value"], 4) else NA
    data.frame(Model = name, Marginal_R2 = r2_m, AIC = aic,
               p_val_Physical_1b = p_invb,
               p_val_fertK = round(tt["beta_fertK", "p-value"], 4),
               p_val_fertMg = round(tt["beta_fertMg", "p-value"], 4))
}"""

content = content.replace(ext_orig, ext_new)

# 4. Update yield_table
ytable_orig = """yield_table <- bind_rows(
    extract_yield(m_yield_raw_co2, "1. Raw P_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_thm_co2, "2. Thermo a_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_aae, "3. Legacy P_AAE10", D_Yield$Relative_Yield)
)"""

ytable_new = """yield_table <- bind_rows(
    extract_yield(m_yield_raw_co2, "1. Raw P_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_thm_co2, "2. Thermo a_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_aae, "3. Legacy P_AAE10", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_co2_null, "1. Null Model - Raw P_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_thm_co2_null, "2. Null Model - Thermo a_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_aae_null, "3. Null Model - Legacy P_AAE10", D_Yield$Relative_Yield)
)"""

content = content.replace(ytable_orig, ytable_new)


with open("notebooks/qi_modelling1.qmd", "w") as f:
    f.write(content)
