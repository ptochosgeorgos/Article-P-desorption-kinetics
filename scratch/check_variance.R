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
        annual_P_uptake = rowSums(across(starts_with("P_harv")), na.rm = TRUE),
        fert_P_tot = fert_P2O5_tot / 2.291
    )

site_geochemistry <- D |> group_by(site) |> summarise(feox_mean = mean(Feox, na.rm = TRUE), alox_mean = mean(Alox, na.rm = TRUE)) |> ungroup()
kinetics_stable <- D |> dplyr::select(site, treatment_ID, rep, k) |> distinct(site, treatment_ID, rep, .keep_all = TRUE)

D_main <- D2 |>
    filter(year >= 1990) |>
    left_join(site_geochemistry, by = "site") |>
    left_join(kinetics_stable, by = c("site", "treatment_ID", "rep"))

kin_train <- D_main |> filter(!is.na(k))
k_ptf <- lm(log(k) ~ log(alox_mean / feox_mean) + soil_0_20_pH_H2O, data = kin_train)
D_main$k_pred <- exp(predict(k_ptf, newdata = D_main))
D_main$k <- D_main$k_pred

D_Long <- D_main |>
    filter(annual_P_uptake > 0, !is.na(k), !is.na(soil_0_20_P_CO2), !is.na(fert_N_tot)) |>
    mutate(site = as.factor(site), year_f = as.factor(year))

D_Long_Agro <- D_Long |> filter(crop %in% c("WW", "WG", "SW", "KM", "SM", "KA", "ZR", "RA")) |> droplevels() |> mutate(z_v0 = as.numeric(scale(k * soil_0_20_P_CO2)), z_P = as.numeric(scale(soil_0_20_P_CO2)))

cat("\n--- Standard Deviation of z_v0 per Crop ---\n")
D_Long_Agro |> group_by(crop) |> summarise(sd_v0 = sd(z_v0), sd_P = sd(soil_0_20_P_CO2), cor_v0_P = cor(z_v0, soil_0_20_P_CO2)) |> print()

