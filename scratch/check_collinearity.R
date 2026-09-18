library(tidyverse)
library(nlme)
library(lme4)

# Load the cached environment right before the model
load("notebooks/qi_modelling_parallel_cache/html/create-comprehensive-dataset_9b4e54823a31c518b2649b80f08149eb.RData")

D_ready <- D_main |>
    mutate(
        ln_P_AAE = log(soil_0_20_P_AAE10),
        ln_P_CO2 = log(soil_0_20_P_CO2),
        a_CO2_total_mg_L = soil_0_20_P_CO2,
        ln_a_CO2 = log(a_CO2_total_mg_L),
        z_ln_FineTexture = as.numeric(scale(log(rollMean_soil_0_20_clay + rollMean_soil_0_20_silt))),
        z_ln_Ca = as.numeric(scale(log(rollMean_soil_0_20_Ca_AAE10))),
        z_ln_Mg = as.numeric(scale(log(rollMean_soil_0_20_Mg_AAE10))),
        z_ln_K  = as.numeric(scale(log(rollMean_soil_0_20_K_AAE10))),
        z_pH    = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
        z_ln_Corg = as.numeric(scale(log(rollMean_soil_0_20_Corg))),
        z_ln_Feox = as.numeric(scale(log(feox_mean))),
        z_ln_Alox = as.numeric(scale(log(alox_mean))),
        z_Temp_Mean = as.numeric(scale(site_juv_temp_mean)), z_Temp_Anom = as.numeric(scale(temp_anomaly)),
        z_Prec_Anom = as.numeric(scale(prec_anomaly)), z_k = as.numeric(scale(k))
    ) |>
    group_by(site) |>
    mutate(
        z_ln_Corg = ifelse(is.na(z_ln_Corg), mean(z_ln_Corg, na.rm = TRUE), z_ln_Corg),
        z_ln_Feox = ifelse(is.na(z_ln_Feox), mean(z_ln_Feox, na.rm = TRUE), z_ln_Feox),
        z_ln_Alox = ifelse(is.na(z_ln_Alox), mean(z_ln_Alox, na.rm = TRUE), z_ln_Alox),
        z_Temp_Anom = ifelse(is.na(z_Temp_Anom), mean(z_Temp_Anom, na.rm = TRUE), z_Temp_Anom),
        z_Prec_Anom = ifelse(is.na(z_Prec_Anom), mean(z_Prec_Anom, na.rm = TRUE), z_Prec_Anom)
    ) |> ungroup()

D_ptf_agro <- D_ready |> drop_na(ln_P_AAE, ln_P_CO2, ln_a_CO2, z_ln_FineTexture, z_pH, z_ln_Ca, z_ln_Mg, z_ln_K, z_ln_Corg, z_Temp_Anom, z_Prec_Anom, z_Temp_Mean)
ptf_practical_raw <- lmer(ln_P_AAE ~ ln_P_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf_agro)

C_agro <- function(term) fixef(ptf_practical_raw)[term]
get_int_agro <- function(base, inter) fixef(ptf_practical_raw)[paste0(base, ":", inter)]

D_Long <- D_ready |>
    filter(annual_P_uptake > 0, !is.na(k), !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot)) |>
    mutate(
        n_pred_agro = C_agro("ln_P_CO2") + get_int_agro("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture + get_int_agro("ln_P_CO2", "z_pH") * z_pH + get_int_agro("ln_P_CO2", "z_ln_Ca") * z_ln_Ca + get_int_agro("ln_P_CO2", "z_ln_Mg") * z_ln_Mg + get_int_agro("ln_P_CO2", "z_ln_K") * z_ln_K + get_int_agro("ln_P_CO2", "z_ln_Corg") * z_ln_Corg + get_int_agro("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom + get_int_agro("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,
        ln_K_pred_agro = C_agro("(Intercept)") + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
        b_power_agro = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
        inv_b_agro = 1 / b_power_agro,
        site = as.factor(site),
        year_f = as.factor(year)
    ) |>
    mutate(z_inv_b_agro = as.numeric(scale(inv_b_agro)))

D_Long_Agro <- D_Long |> filter(crop %in% c("WW", "WG", "SW", "KM", "SM", "KA", "ZR", "RA")) |> filter(is.finite(z_inv_b_agro), !is.na(fert_N_tot), !is.na(z_Temp_Anom)) |> mutate(crop = as.factor(crop)) |> droplevels() |> mutate(z_inv_b_agro = as.numeric(scale(inv_b_agro)), z_v0 = as.numeric(scale(k * soil_0_20_P_CO2)), z_fert_N = as.numeric(scale(fert_N_tot)))

print("Testing Collinearity of fixed effects:")
mod_lm <- lm(annual_P_uptake ~ crop + z_Temp_Anom + z_fert_N + z_inv_b_agro + z_v0, data = D_Long_Agro)
print(alias(mod_lm))

cat("\nSummary of SD:\n")
print(c(sd(D_Long_Agro$z_Temp_Anom), sd(D_Long_Agro$z_fert_N), sd(D_Long_Agro$z_inv_b_agro), sd(D_Long_Agro$z_v0)))
