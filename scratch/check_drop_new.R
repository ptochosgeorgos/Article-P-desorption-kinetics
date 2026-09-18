source("scratch/qi_code_subset.R")
cat("Dimensions of D_Yield:", dim(D_Yield), "\n")
cat("Checking where rows drop...\n")

D_Y1 <- D_ready |> filter(annual_yield_mp_DM > 0, !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot))
cat("Step 1:", nrow(D_Y1), "\n")

D_Y2 <- D_Y1 |> mutate(
    n_pred_agro = C_agro("ln_P_CO2") + get_int_agro("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture + get_int_agro("ln_P_CO2", "z_pH") * z_pH + get_int_agro("ln_P_CO2", "z_ln_Ca") * z_ln_Ca + get_int_agro("ln_P_CO2", "z_ln_Mg") * z_ln_Mg + get_int_agro("ln_P_CO2", "z_ln_K") * z_ln_K + get_int_agro("ln_P_CO2", "z_ln_Corg") * z_ln_Corg + get_int_agro("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom + get_int_agro("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,
    ln_K_pred_agro = C_agro("(Intercept)") + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
    b_power = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
    inv_b = 1 / b_power
)
cat("Step 2:", nrow(D_Y2), "\n")

D_Y3 <- D_Y2 |> mutate(
    n_pred_cons = C_cons("ln_P_CO2") + get_int_cons("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture + get_int_cons("ln_P_CO2", "z_pH") * z_pH + get_int_cons("ln_P_CO2", "z_ln_Ca") * z_ln_Ca + get_int_cons("ln_P_CO2", "z_ln_Mg") * z_ln_Mg + get_int_cons("ln_P_CO2", "z_ln_K") * z_ln_K + get_int_cons("ln_P_CO2", "z_ln_Corg") * z_ln_Corg,
    ln_K_pred_cons = C_cons("(Intercept)") + C_cons("z_ln_FineTexture") * z_ln_FineTexture + C_cons("z_pH") * z_pH + C_cons("z_ln_Ca") * z_ln_Ca + C_cons("z_ln_Mg") * z_ln_Mg + C_cons("z_ln_K") * z_ln_K + C_cons("z_ln_Corg") * z_ln_Corg,
    b_power_cons = n_pred_cons * exp(ln_K_pred_cons) * (soil_0_20_P_CO2^(n_pred_cons - 1)),
    inv_b_cons = 1 / b_power_cons
)
cat("Step 3:", nrow(D_Y3), "\n")

D_Y4 <- D_Y3 |> filter(crop %in% c("WW", "WG", "SW", "KM", "SM", "KA", "ZR", "RA"))
cat("Step 4:", nrow(D_Y4), "\n")

D_Y5 <- D_Y4 |> filter(!is.na(rollMean_soil_0_20_K_AAE10), !is.na(rollMean_soil_0_20_pH_H2O), !is.na(rollMean_soil_0_20_Mg_AAE10), !is.na(fert_N_tot), !is.na(site_juv_temp_mean), !is.na(prec_anomaly))
cat("Step 5:", nrow(D_Y5), "\n")

D_Y6 <- D_Y5 |> filter(is.finite(inv_b))
cat("Step 6:", nrow(D_Y6), "\n")

D_Y7 <- D_Y6 |> filter(is.finite(inv_b_cons))
cat("Step 7:", nrow(D_Y7), "\n")

