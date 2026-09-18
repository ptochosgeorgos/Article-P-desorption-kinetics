source("scratch/qi_code_subset.R")

D_Yield1 <- D_ready |>
    filter(annual_yield_mp_DM > 0, !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot))

cat("Step 1 (Basic Filter):", nrow(D_Yield1), "\n")

D_Yield2 <- D_Yield1 |>
    mutate(
        n_pred_agro = C_agro("ln_P_CO2") + get_int_agro("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture + get_int_agro("ln_P_CO2", "z_pH") * z_pH + get_int_agro("ln_P_CO2", "z_ln_Ca") * z_ln_Ca + get_int_agro("ln_P_CO2", "z_ln_Mg") * z_ln_Mg + get_int_agro("ln_P_CO2", "z_ln_K") * z_ln_K + get_int_agro("ln_P_CO2", "z_ln_Corg") * z_ln_Corg + get_int_agro("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom + get_int_agro("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,
        ln_K_pred_agro = C_agro("(Intercept)") + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
        b_power = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
        inv_b = 1 / b_power,
        total_yield = tidyr::replace_na(annual_yield_mp_DM, 0)
    )

cat("Step 2 (Mutate inv_b):", nrow(D_Yield2), "\n")

D_Yield3 <- D_Yield2 |>
    mutate(
        n_pred_cons = C_cons("ln_P_CO2") + get_int_cons("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture + get_int_cons("ln_P_CO2", "z_pH") * z_pH + get_int_cons("ln_P_CO2", "z_ln_Ca") * z_ln_Ca + get_int_cons("ln_P_CO2", "z_ln_Mg") * z_ln_Mg + get_int_cons("ln_P_CO2", "z_ln_K") * z_ln_K + get_int_cons("ln_P_CO2", "z_ln_Corg") * z_ln_Corg + get_int_cons("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom + get_int_cons("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,
        ln_K_pred_cons = C_cons("(Intercept)") + C_cons("z_ln_FineTexture") * z_ln_FineTexture + C_cons("z_pH") * z_pH + C_cons("z_ln_Ca") * z_ln_Ca + C_cons("z_ln_Mg") * z_ln_Mg + C_cons("z_ln_K") * z_ln_K + C_cons("z_ln_Corg") * z_ln_Corg + C_cons("z_Temp_Anom") * z_Temp_Anom + C_cons("z_Prec_Anom") * z_Prec_Anom + C_cons("z_Temp_Mean") * z_Temp_Mean,
        b_power_cons = n_pred_cons * exp(ln_K_pred_cons) * (soil_0_20_P_CO2^(n_pred_cons - 1)),
        inv_b_cons = 1 / b_power_cons
    )

cat("Step 3 (Mutate inv_b_cons):", nrow(D_Yield3), "\n")

D_Yield4 <- D_Yield3 |> filter(crop %in% c("WW", "WG", "SW", "KM", "SM", "KA", "ZR", "RA"))
cat("Step 4 (Filter crops):", nrow(D_Yield4), "\n")

D_Yield5 <- D_Yield4 |> filter(total_yield > 0)
cat("Step 5 (Filter total_yield > 0):", nrow(D_Yield5), "\n")

D_Yield6 <- D_Yield5 |> filter(!is.na(rollMean_soil_0_20_K_AAE10), !is.na(rollMean_soil_0_20_pH_H2O), !is.na(rollMean_soil_0_20_Mg_AAE10), !is.na(fert_N_tot), !is.na(site_juv_temp_mean), !is.na(prec_anomaly))
cat("Step 6 (Filter NAs):", nrow(D_Yield6), "\n")

D_Yield7 <- D_Yield6 |> filter(is.finite(inv_b))
cat("Step 7 (Filter is.finite inv_b):", nrow(D_Yield7), "\n")

D_Yield8 <- D_Yield7 |> filter(is.finite(inv_b_cons))
cat("Step 8 (Filter is.finite inv_b_cons):", nrow(D_Yield8), "\n")

