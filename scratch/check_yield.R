library(tidyverse)
library(nlme)
library(readxl)

RES <- readRDS("data/RES.rds")
D <- RES$D

climate_data <- readRDS("data/all_P.rds") |> dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec) |> dplyr::distinct()

D2 <- read_excel("data/STYCS_data_2023_260511.xlsx") |>
    rename(rep = replicate) |>
    mutate(site = gsub("STYCS_", "", LtE_name)) |>
    left_join(climate_data, by = c("site", "year")) |>
    mutate(
        soil_0_20_P_CO2 = soil_0_20_P_test * 0.155,
        crop = crop_abr,
        annual_P_uptake = rowSums(across(starts_with("P_harv")), na.rm = TRUE),
        fert_P_tot = fert_P2O5_tot / 2.291,
        annual_yield_mp_DM = rowSums(across(matches("^harv.*mp_yield_DM$")), na.rm = TRUE)
    )

site_geochemistry <- D |> group_by(site) |> summarise(feox_mean = mean(Feox, na.rm = TRUE), alox_mean = mean(Alox, na.rm = TRUE)) |> ungroup()

D_main <- D2 |> filter(year >= 1990) |>
    group_by(site) |>
    mutate(
        site_juv_temp_mean = mean(juvdev_temp, na.rm = TRUE), site_juv_prec_mean = mean(juvdev_prec, na.rm = TRUE),
        temp_anomaly = juvdev_temp - site_juv_temp_mean, prec_anomaly = juvdev_prec - site_juv_prec_mean
    ) |> ungroup() |> left_join(site_geochemistry, by = "site")

global_med_Ca_H2O10 <- median(D_main$soil_0_20_Ca_H2O10, na.rm = TRUE)
global_med_Mg_H2O10 <- median(D_main$soil_0_20_Mg_H2O10, na.rm = TRUE)
global_med_K_H2O10  <- median(D_main$soil_0_20_K_H2O10, na.rm = TRUE)
global_med_Ca_AAE10 <- median(D_main$soil_0_20_Ca_AAE10, na.rm = TRUE)
global_med_Mg_AAE10 <- median(D_main$soil_0_20_Mg_AAE10, na.rm = TRUE)
global_med_K_AAE10  <- median(D_main$soil_0_20_K_AAE10, na.rm = TRUE)

D_main <- D_main |>
    group_by(site) |>
    mutate(
        soil_0_20_Ca_H2O10 = ifelse(is.na(soil_0_20_Ca_H2O10), median(soil_0_20_Ca_H2O10, na.rm = TRUE), soil_0_20_Ca_H2O10),
        soil_0_20_Mg_H2O10 = ifelse(is.na(soil_0_20_Mg_H2O10), median(soil_0_20_Mg_H2O10, na.rm = TRUE), soil_0_20_Mg_H2O10),
        soil_0_20_K_H2O10  = ifelse(is.na(soil_0_20_K_H2O10),  median(soil_0_20_K_H2O10, na.rm = TRUE), soil_0_20_K_H2O10),
        soil_0_20_Ca_AAE10 = ifelse(is.na(soil_0_20_Ca_AAE10), median(soil_0_20_Ca_AAE10, na.rm = TRUE), soil_0_20_Ca_AAE10),
        soil_0_20_Mg_AAE10 = ifelse(is.na(soil_0_20_Mg_AAE10), median(soil_0_20_Mg_AAE10, na.rm = TRUE), soil_0_20_Mg_AAE10),
        soil_0_20_K_AAE10  = ifelse(is.na(soil_0_20_K_AAE10),  median(soil_0_20_K_AAE10, na.rm = TRUE), soil_0_20_K_AAE10)
    ) |> ungroup() |>
    mutate(
        soil_0_20_Ca_H2O10 = ifelse(is.na(soil_0_20_Ca_H2O10), global_med_Ca_H2O10, soil_0_20_Ca_H2O10),
        soil_0_20_Mg_H2O10 = ifelse(is.na(soil_0_20_Mg_H2O10), global_med_Mg_H2O10, soil_0_20_Mg_H2O10),
        soil_0_20_K_H2O10  = ifelse(is.na(soil_0_20_K_H2O10),  global_med_K_H2O10,  soil_0_20_K_H2O10),
        soil_0_20_Ca_AAE10 = ifelse(is.na(soil_0_20_Ca_AAE10), global_med_Ca_AAE10, soil_0_20_Ca_AAE10),
        soil_0_20_Mg_AAE10 = ifelse(is.na(soil_0_20_Mg_AAE10), global_med_Mg_AAE10, soil_0_20_Mg_AAE10),
        soil_0_20_K_AAE10  = ifelse(is.na(soil_0_20_K_AAE10),  global_med_K_AAE10,  soil_0_20_K_AAE10)
    )

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
        z_Prec_Anom = as.numeric(scale(prec_anomaly))
    ) |>
    group_by(site) |>
    mutate(
        z_ln_Corg = ifelse(is.na(z_ln_Corg), mean(z_ln_Corg, na.rm = TRUE), z_ln_Corg),
        z_ln_Feox = ifelse(is.na(z_ln_Feox), mean(z_ln_Feox, na.rm = TRUE), z_ln_Feox),
        z_ln_Alox = ifelse(is.na(z_ln_Alox), mean(z_ln_Alox, na.rm = TRUE), z_ln_Alox),
        z_Temp_Anom = ifelse(is.na(z_Temp_Anom), mean(z_Temp_Anom, na.rm = TRUE), z_Temp_Anom),
        z_Prec_Anom = ifelse(is.na(z_Prec_Anom), mean(z_Prec_Anom, na.rm = TRUE), z_Prec_Anom)
    ) |> ungroup()

library(lme4)
D_ptf_agro <- D_ready |> drop_na(ln_P_AAE, ln_P_CO2, ln_a_CO2, z_ln_FineTexture, z_pH, z_ln_Ca, z_ln_Mg, z_ln_K, z_ln_Corg, z_Temp_Anom, z_Prec_Anom, z_Temp_Mean)
ptf_practical_raw <- lmer(ln_P_AAE ~ ln_P_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf_agro)

C_agro <- function(term) fixef(ptf_practical_raw)[term]
get_int_agro <- function(base, inter) fixef(ptf_practical_raw)[paste0(base, ":", inter)]

D_Yield <- D_ready |>
    filter(annual_yield_mp_DM > 0, !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot)) |>
    mutate(
        n_pred_agro = C_agro("ln_P_CO2") + get_int_agro("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture + get_int_agro("ln_P_CO2", "z_pH") * z_pH + get_int_agro("ln_P_CO2", "z_ln_Ca") * z_ln_Ca + get_int_agro("ln_P_CO2", "z_ln_Mg") * z_ln_Mg + get_int_agro("ln_P_CO2", "z_ln_K") * z_ln_K + get_int_agro("ln_P_CO2", "z_ln_Corg") * z_ln_Corg + get_int_agro("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom + get_int_agro("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,
        ln_K_pred_agro = C_agro("(Intercept)") + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
        b_power = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
        inv_b = 1 / b_power,
        total_yield = tidyr::replace_na(annual_yield_mp_DM, 0)
    ) |>
    filter(crop %in% c("WW", "WG", "SW", "KM", "SM", "KA", "ZR", "RA")) |>
    filter(total_yield > 0) |>
    filter(!is.na(rollMean_soil_0_20_K_AAE10), !is.na(rollMean_soil_0_20_pH_H2O), !is.na(rollMean_soil_0_20_Mg_AAE10), !is.na(fert_N_tot), !is.na(site_juv_temp_mean), !is.na(prec_anomaly)) |>
    mutate(crop = droplevels(as.factor(crop))) |>
    filter(is.finite(inv_b), is.finite(total_yield), total_yield > 0) |>
    mutate(
        z_inv_b = as.numeric(scale(inv_b)),
        z_pH = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
        z_ln_K = as.numeric(scale(log(rollMean_soil_0_20_K_AAE10))),
        z_ln_Mg = as.numeric(scale(log(rollMean_soil_0_20_Mg_AAE10))),
        z_fert_N = as.numeric(scale(fert_N_tot)),
        z_Temp_Mean = as.numeric(scale(site_juv_temp_mean)),
        z_Prec_Anom = as.numeric(scale(prec_anomaly)),
        site = as.factor(site)
    )

n_crops_yield <- length(levels(D_Yield$crop))

cat("Testing m_yield_raw_co2...\n")
res <- tryCatch({
    nlme(
        total_yield ~ Y0 + (A - Y0) * (1 - exp(-(c_base * exp(
            beta_invb * z_inv_b + beta_pH * z_pH + beta_K * z_ln_K +
            beta_Mg * z_ln_Mg + beta_N * z_fert_N + beta_Temp * z_Temp_Mean + beta_Prec * z_Prec_Anom
        ) * (soil_0_20_P_CO2 + E_base)))),
        data = D_Yield,
        fixed = list(A ~ crop, Y0 ~ crop, c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_K ~ 1, beta_Mg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
        random = E_base ~ 1 | site,
        weights = varPower(form = ~ soil_0_20_P_CO2),
        start = c(10, rep(0, n_crops_yield - 1), 2, rep(0, n_crops_yield - 1), 1.2, rep(0, n_crops_yield - 1), rep(0, 7), 0),
        control = nlmeControl(maxIter = 2000, returnObject = TRUE)
    )
}, error = function(e) e$message)

print(res)
