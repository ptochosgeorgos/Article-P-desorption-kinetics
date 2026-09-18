library(tidyverse)
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
        annual_yield_mp_DM = rowSums(across(matches("^harv.*mp_yield_DM$")), na.rm = TRUE)
    )

site_geochemistry <- D |> group_by(site) |> summarise(feox_mean = mean(Feox, na.rm = TRUE), alox_mean = mean(Alox, na.rm = TRUE)) |> ungroup()
D_main <- D2 |> filter(year >= 1990) |> group_by(site) |> mutate(site_juv_temp_mean = mean(juvdev_temp, na.rm = TRUE), site_juv_prec_mean = mean(juvdev_prec, na.rm = TRUE), temp_anomaly = juvdev_temp - site_juv_temp_mean, prec_anomaly = juvdev_prec - site_juv_prec_mean) |> ungroup() |> left_join(site_geochemistry, by = "site")
# simple impute
for(col in c("soil_0_20_Mg_AAE10", "soil_0_20_K_AAE10", "soil_0_20_pH_H2O", "rollMean_soil_0_20_clay", "rollMean_soil_0_20_silt", "rollMean_soil_0_20_Ca_AAE10", "rollMean_soil_0_20_Corg")) {
    m = median(D_main[[col]], na.rm=TRUE)
    D_main[[col]] = ifelse(is.na(D_main[[col]]), m, D_main[[col]])
}
D_main <- D_main |> mutate(rollMean_soil_0_20_K_AAE10 = soil_0_20_K_AAE10, rollMean_soil_0_20_Mg_AAE10 = soil_0_20_Mg_AAE10, rollMean_soil_0_20_pH_H2O = soil_0_20_pH_H2O)

D_Yield <- D_main |>
    filter(annual_yield_mp_DM > 0, !is.na(soil_0_20_P_CO2), !is.na(fert_N_tot)) |>
    filter(crop %in% c("WW", "WG", "SW", "KM", "SM", "KA", "ZR", "RA")) |>
    filter(!is.na(rollMean_soil_0_20_K_AAE10), !is.na(rollMean_soil_0_20_pH_H2O), !is.na(rollMean_soil_0_20_Mg_AAE10), !is.na(site_juv_temp_mean), !is.na(prec_anomaly)) |>
    mutate(
        z_pH = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
        z_ln_K = as.numeric(scale(log(rollMean_soil_0_20_K_AAE10))),
        z_ln_Mg = as.numeric(scale(log(rollMean_soil_0_20_Mg_AAE10))),
        z_fert_N = as.numeric(scale(fert_N_tot)),
        z_Temp_Mean = as.numeric(scale(site_juv_temp_mean)),
        z_Prec_Anom = as.numeric(scale(prec_anomaly)),
        crop = as.factor(crop)
    )

mod <- lm(rnorm(nrow(D_Yield)) ~ crop + z_pH + z_ln_K + z_ln_Mg + z_fert_N + z_Temp_Mean + z_Prec_Anom, data = D_Yield)
print(summary(mod))
print(alias(mod))
