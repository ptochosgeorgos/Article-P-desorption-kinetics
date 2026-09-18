suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readxl))

RES <- readRDS("data/RES.rds")
D_main <- RES$D

climate_data <- readRDS("data/all_P.rds") |> dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec) |> dplyr::distinct()
D2 <- read_excel("data/STYCS_data_2023_260511.xlsx") |> rename(rep = replicate) |> mutate(site = gsub("STYCS_", "", LtE_name)) |> left_join(climate_data, by = c("site", "year")) |> mutate(crop = crop_abr, annual_P_uptake = rowSums(across(starts_with("P_harv")), na.rm = TRUE), fert_P_tot = fert_P2O5_tot / 2.291, annual_P_balance = fert_P_tot - annual_P_uptake)

D_k <- D_main |> filter(!is.na(k)) |> dplyr::select(site, treatment_ID, rep, k, v0_kPS = kPS, Pmax_PS = PS) |> distinct(site, treatment_ID, rep, .keep_all = TRUE)
D_main <- D2 |> left_join(D_k, by = c("site", "treatment_ID", "rep"))

D_main <- D_main |> mutate(
    soil_0_20_P_CO2 = soil_0_20_P_test * 0.155,
    a_CO2_total_mg_L = soil_0_20_P_CO2,
    Temp_Anom = juvdev_temp,
    fert_N_tot = ifelse(is.na(fert_N_tot), 0, fert_N_tot)
)

D_ready <- D_main |> mutate(
    z_fert_N = as.numeric(scale(fert_N_tot)),
    z_Temp_Anom = as.numeric(scale(Temp_Anom)),
    z_ln_FineTexture = as.numeric(scale(log(rollMean_soil_0_20_clay + rollMean_soil_0_20_silt))),
    z_ln_Ca = as.numeric(scale(log(rollMean_soil_0_20_Ca_AAE10))),
    z_pH = as.numeric(scale(rollMean_soil_0_20_pH_H2O))
)

cat("annual_P_uptake > 0:", sum(D_ready$annual_P_uptake > 0, na.rm=TRUE), "\n")
D_Long <- D_ready |> filter(annual_P_uptake > 0, !is.na(k), !is.na(soil_0_20_P_CO2), !is.na(z_fert_N), !is.na(z_Temp_Anom))

D_Long <- D_Long |> mutate(
    n_pred_agro = 0.5 + 0.1 * z_ln_FineTexture + 0.05 * z_pH + 0.02 * z_ln_Ca,
    ln_K_pred_agro = 1.0 + 0.2 * z_ln_FineTexture,
    b_power_agro = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
    inv_b_agro = 1 / b_power_agro,
    z_inv_b_agro = as.numeric(scale(inv_b_agro))
)

cat("D_Long rows after filter:", nrow(D_Long), "\n")
cat("Missing inv_b_agro:", sum(is.na(D_Long$inv_b_agro)), "\n")
cat("Missing z_inv_b_agro:", sum(is.na(D_Long$z_inv_b_agro)), "\n")

D_Long_Agro <- D_Long |> filter(is.finite(z_inv_b_agro))
cat("D_Long_Agro rows:", nrow(D_Long_Agro), "\n")
cat("Crops:", length(unique(D_Long_Agro$crop)), "\n")
print(table(D_Long_Agro$crop))

