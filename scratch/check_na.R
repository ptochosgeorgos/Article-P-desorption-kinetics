source("scratch/qi_code_subset.R")
D_Yield <- D_ready |>
    filter(annual_yield_mp_DM > 0, !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot)) |>
    filter(crop %in% c("WW", "WG", "SW", "KM", "SM", "KA", "ZR", "RA"))

cat("Rows before filtering:", nrow(D_Yield), "\n")
cat("NAs in K_AAE10:", sum(is.na(D_Yield$rollMean_soil_0_20_K_AAE10)), "\n")
cat("NAs in pH_H2O:", sum(is.na(D_Yield$rollMean_soil_0_20_pH_H2O)), "\n")
cat("NAs in Mg_AAE10:", sum(is.na(D_Yield$rollMean_soil_0_20_Mg_AAE10)), "\n")
cat("NAs in fert_N_tot:", sum(is.na(D_Yield$fert_N_tot)), "\n")
cat("NAs in site_juv_temp_mean:", sum(is.na(D_Yield$site_juv_temp_mean)), "\n")
cat("NAs in prec_anomaly:", sum(is.na(D_Yield$prec_anomaly)), "\n")
cat("NAs in anavg_temp:", sum(is.na(D_Yield$anavg_temp)), "\n")
cat("NAs in ansum_prec:", sum(is.na(D_Yield$ansum_prec)), "\n")
