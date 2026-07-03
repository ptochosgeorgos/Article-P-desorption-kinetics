import re

with open("notebooks/qi_modelling1.qmd", "r") as f:
    content = f.read()

# Replace the D_Cum summarise block to include the covariates
dcum_orig = """    group_by(site, plot_nr, treatment) |>
    summarise(
        Cumulated_P_Balance = sum(annual_P_balance, na.rm = TRUE),
        mean_P_CO2 = mean(soil_0_20_P_CO2, na.rm = TRUE),
        mean_a_CO2 = mean(a_CO2_total_mg_L, na.rm = TRUE),
        mean_P_AAE10 = mean(soil_0_20_P_AAE10, na.rm = TRUE),
        z_inv_b_agro = mean(inv_b_agro, na.rm = TRUE),
        .groups = "drop"
    ) |>
    filter(is.finite(Cumulated_P_Balance), is.finite(z_inv_b_agro), !is.na(mean_P_CO2)) |>
    mutate(
        z_inv_b = as.numeric(scale(z_inv_b_agro)),
        ln_P_CO2 = log(mean_P_CO2),
        ln_a_CO2 = log(mean_a_CO2),
        ln_P_AAE10 = log(mean_P_AAE10),
        site = as.factor(site)
    )"""

dcum_new = """    group_by(site, plot_nr, treatment) |>
    summarise(
        Cumulated_P_Balance = sum(annual_P_balance, na.rm = TRUE),
        mean_P_CO2 = mean(soil_0_20_P_CO2, na.rm = TRUE),
        mean_a_CO2 = mean(a_CO2_total_mg_L, na.rm = TRUE),
        mean_P_AAE10 = mean(soil_0_20_P_AAE10, na.rm = TRUE),
        z_inv_b_agro = mean(inv_b_agro, na.rm = TRUE),
        m_pH = mean(z_pH, na.rm = TRUE),
        m_Temp = mean(z_Temp_Mean, na.rm = TRUE),
        m_Tex = mean(z_ln_FineTexture, na.rm = TRUE),
        m_fert_N = mean(fert_N_tot, na.rm = TRUE),
        m_fert_K = mean(fert_K_tot, na.rm = TRUE),
        m_fert_Mg = mean(fert_Mg_tot, na.rm = TRUE),
        .groups = "drop"
    ) |>
    filter(is.finite(Cumulated_P_Balance), is.finite(z_inv_b_agro), !is.na(mean_P_CO2)) |>
    mutate(
        z_inv_b = as.numeric(scale(z_inv_b_agro)),
        ln_P_CO2 = log(mean_P_CO2),
        ln_a_CO2 = log(mean_a_CO2),
        ln_P_AAE10 = log(mean_P_AAE10),
        z_pH = as.numeric(scale(m_pH)),
        z_Temp = as.numeric(scale(m_Temp)),
        z_Tex = as.numeric(scale(m_Tex)),
        z_fert_N = as.numeric(scale(m_fert_N)),
        z_fert_K = as.numeric(scale(tidyr::replace_na(m_fert_K, 0))),
        z_fert_Mg = as.numeric(scale(tidyr::replace_na(m_fert_Mg, 0))),
        site = as.factor(site)
    )"""

content = content.replace(dcum_orig, dcum_new)

# Replace the LMER models to include the covariates
models_orig = """# 2. Fit Full Models
m_bal_co2 <- lmer(Cumulated_P_Balance ~ ln_P_CO2 * z_inv_b + (1 | site), data = D_Cum)
m_bal_thm <- lmer(Cumulated_P_Balance ~ ln_a_CO2 * z_inv_b + (1 | site), data = D_Cum)
m_bal_aae <- lmer(Cumulated_P_Balance ~ ln_P_AAE10 * z_inv_b + (1 | site), data = D_Cum)

# 3. Fit Null Models
m_bal_co2_null <- lmer(Cumulated_P_Balance ~ ln_P_CO2 + (1 | site), data = D_Cum)
m_bal_thm_null <- lmer(Cumulated_P_Balance ~ ln_a_CO2 + (1 | site), data = D_Cum)
m_bal_aae_null <- lmer(Cumulated_P_Balance ~ ln_P_AAE10 + (1 | site), data = D_Cum)"""

models_new = """# 2. Fit Full Models (with fixed covariates to isolate buffer effects)
m_bal_co2 <- lmer(Cumulated_P_Balance ~ ln_P_CO2 * z_inv_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_thm <- lmer(Cumulated_P_Balance ~ ln_a_CO2 * z_inv_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_aae <- lmer(Cumulated_P_Balance ~ ln_P_AAE10 * z_inv_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)

# 3. Fit Null Models
m_bal_co2_null <- lmer(Cumulated_P_Balance ~ ln_P_CO2 + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_thm_null <- lmer(Cumulated_P_Balance ~ ln_a_CO2 + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_aae_null <- lmer(Cumulated_P_Balance ~ ln_P_AAE10 + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)"""

content = content.replace(models_orig, models_new)

with open("notebooks/qi_modelling1.qmd", "w") as f:
    f.write(content)

