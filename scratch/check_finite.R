source("scratch/qi_code_subset.R")
D_Yield <- D_ready |>
    filter(annual_yield_mp_DM > 0, !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot)) |>
    mutate(
        n_pred_agro = C_agro("ln_P_CO2") + get_int_agro("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture + get_int_agro("ln_P_CO2", "z_pH") * z_pH + get_int_agro("ln_P_CO2", "z_ln_Ca") * z_ln_Ca + get_int_agro("ln_P_CO2", "z_ln_Mg") * z_ln_Mg + get_int_agro("ln_P_CO2", "z_ln_K") * z_ln_K + get_int_agro("ln_P_CO2", "z_ln_Corg") * z_ln_Corg + get_int_agro("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom + get_int_agro("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,
        ln_K_pred_agro = C_agro("(Intercept)") + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
        b_power = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
        inv_b = 1 / b_power,
        total_yield = tidyr::replace_na(annual_yield_mp_DM, 0)
    )

cat("Total D_Yield after inv_b:", nrow(D_Yield), "\n")
cat("NAs in n_pred_agro:", sum(is.na(D_Yield$n_pred_agro)), "\n")
cat("NAs in inv_b:", sum(is.na(D_Yield$inv_b)), "\n")
cat("Non-finite inv_b:", sum(!is.finite(D_Yield$inv_b)), "\n")

cat("Checking input NAs:\n")
for(col in c("z_ln_FineTexture", "z_pH", "z_ln_Ca", "z_ln_Mg", "z_ln_K", "z_ln_Corg", "z_Temp_Anom", "z_Prec_Anom", "z_Temp_Mean")) {
    cat("  ", col, "NAs:", sum(is.na(D_Yield[[col]])), "\n")
}
