source("scratch/qi_code_subset.R")
D_Y1 <- D_ready |> filter(annual_yield_mp_DM > 0, !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot))

cat("D_Y1 rows:", nrow(D_Y1), "\n")

D_Y3 <- D_Y1 |> mutate(
    n_pred_cons = C_cons("ln_P_CO2") + get_int_cons("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture + get_int_cons("ln_P_CO2", "z_pH") * z_pH + get_int_cons("ln_P_CO2", "z_ln_Ca") * z_ln_Ca + get_int_cons("ln_P_CO2", "z_ln_Mg") * z_ln_Mg + get_int_cons("ln_P_CO2", "z_ln_K") * z_ln_K + get_int_cons("ln_P_CO2", "z_ln_Corg") * z_ln_Corg,
    ln_K_pred_cons = C_cons("(Intercept)") + C_cons("z_ln_FineTexture") * z_ln_FineTexture + C_cons("z_pH") * z_pH + C_cons("z_ln_Ca") * z_ln_Ca + C_cons("z_ln_Mg") * z_ln_Mg + C_cons("z_ln_K") * z_ln_K + C_cons("z_ln_Corg") * z_ln_Corg,
    b_power_cons = n_pred_cons * exp(ln_K_pred_cons) * (soil_0_20_P_CO2^(n_pred_cons - 1)),
    inv_b_cons = 1 / b_power_cons
)
cat("n_pred_cons NA count:", sum(is.na(D_Y3$n_pred_cons)), "\n")
cat("ln_K_pred_cons NA count:", sum(is.na(D_Y3$ln_K_pred_cons)), "\n")
cat("b_power_cons NA count:", sum(is.na(D_Y3$b_power_cons)), "\n")
cat("inv_b_cons NA count:", sum(is.na(D_Y3$inv_b_cons)), "\n")

