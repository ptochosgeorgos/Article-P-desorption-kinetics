import re

with open("notebooks/qi_modelling1.qmd", "r") as f:
    content = f.read()

dcum_orig = """D_Cum <- D_ready |>
    group_by(site, plot_nr, treatment) |>
    summarise(
        Cumulated_P_Balance = sum(annual_P_balance, na.rm = TRUE),
        mean_P_CO2 = mean(soil_0_20_P_CO2, na.rm = TRUE),
        mean_a_CO2 = mean(a_CO2_total_mg_L, na.rm = TRUE),
        mean_P_AAE10 = mean(soil_0_20_P_AAE10, na.rm = TRUE),
        z_inv_b_agro = mean(inv_b_agro, na.rm = TRUE),
        .groups = "drop"
    ) |>"""

dcum_new = """D_Cum <- D_ready |>
    mutate(
        n_pred_agro = C_agro("ln_P_CO2") +
            get_int_agro("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture +
            get_int_agro("ln_P_CO2", "z_pH") * z_pH +
            get_int_agro("ln_P_CO2", "z_ln_Ca") * z_ln_Ca +
            get_int_agro("ln_P_CO2", "z_ln_Mg") * z_ln_Mg +
            get_int_agro("ln_P_CO2", "z_ln_K") * z_ln_K +
            get_int_agro("ln_P_CO2", "z_ln_Corg") * z_ln_Corg +
            get_int_agro("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom +
            get_int_agro("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,
        ln_K_pred_agro = C_agro("(Intercept)") + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
        b_power_agro = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
        inv_b_agro = 1 / b_power_agro
    ) |>
    group_by(site, plot_nr, treatment) |>
    summarise(
        Cumulated_P_Balance = sum(annual_P_balance, na.rm = TRUE),
        mean_P_CO2 = mean(soil_0_20_P_CO2, na.rm = TRUE),
        mean_a_CO2 = mean(a_CO2_total_mg_L, na.rm = TRUE),
        mean_P_AAE10 = mean(soil_0_20_P_AAE10, na.rm = TRUE),
        z_inv_b_agro = mean(inv_b_agro, na.rm = TRUE),
        .groups = "drop"
    ) |>"""

content = content.replace(dcum_orig, dcum_new)

with open("notebooks/qi_modelling1.qmd", "w") as f:
    f.write(content)
