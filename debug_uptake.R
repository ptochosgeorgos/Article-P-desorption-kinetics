suppressPackageStartupMessages({
    require(parallel)
    require(lme4)
    require(nlme)
    require(dplyr)
    require(readxl)
})

climate_data <- readRDS("data/all_P.rds") |>
    dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec) |>
    dplyr::distinct()

D2 <- read_excel("data/STYCS_data_2023_260511.xlsx") |>
    rename(rep = replicate) |>
    mutate(site = gsub("STYCS_", "", LtE_name)) |>
    left_join(climate_data, by = c("site", "year")) |>
    mutate(
        soil_0_20_P_CO2 = soil_0_20_P_test * 0.155,
        crop = crop_abr,
        annual_P_uptake = rowSums(across(starts_with("P_harv")), na.rm = TRUE),
        fert_P_tot = fert_P2O5_tot / 2.291
    ) |>
    droplevels()

RES <- readRDS("data/RES.rds")
D_k <- RES$D
D_k$k <- 10^D_k$pred_log_k
D_k <- D_k |> rename(site = LtE_name) |> dplyr::select(site, soil_0_20_plot, year, k, inv_b)
D2 <- D2 |> left_join(D_k, by = c("site", "soil_0_20_plot", "year"))

climate_normals <- D2 |> group_by(site) |> summarise(Temp_Mean = mean(anavg_temp, na.rm = TRUE), Prec_Mean = mean(ansum_prec, na.rm = TRUE))
D2 <- D2 |> left_join(climate_normals, by = "site") |> mutate(Temp_Anom = anavg_temp - Temp_Mean, Prec_Anom = ansum_prec - Prec_Mean)

D_Long <- D2 |>
    filter(annual_P_uptake > 0, !is.na(k), !is.na(soil_0_20_P_CO2), !is.na(fert_N_tot)) |>
    mutate(
        a_CO2_total_mg_L = 10^(2.46 - 0.72 * log10(soil_0_20_pH_H2O) + 1.15 * log10(soil_0_20_P_CO2) - 0.52 * log10(soil_0_20_Ca)),
        soil_0_20_P_AAE10 = soil_0_20_P_AAE10_EDTA / 2.291,
        z_ln_FineTexture = scale(log(soil_0_20_clay + soil_0_20_silt)),
        z_pH = scale(soil_0_20_pH_H2O),
        z_ln_Ca = scale(log(soil_0_20_Ca)),
        z_ln_Mg = scale(log(soil_0_20_Mg)),
        z_ln_K = scale(log(soil_0_20_K)),
        z_ln_Corg = scale(log(soil_0_20_C_org)),
        z_Temp_Anom = scale(Temp_Anom),
        z_Prec_Anom = scale(Prec_Anom),
        z_Temp_Mean = scale(Temp_Mean)
    )
    
ptf_practical_raw <- readRDS("data/processed/ptf_practical_raw.rds")
coefs_agro <- lme4::fixef(ptf_practical_raw)
C_agro <- function(name) { coefs_agro[name] }
get_int_agro <- function(base_name, interaction_name) {
    term1 <- paste0(base_name, ":", interaction_name)
    term2 <- paste0(interaction_name, ":", base_name)
    if (term1 %in% names(coefs_agro)) return(coefs_agro[term1])
    if (term2 %in% names(coefs_agro)) return(coefs_agro[term2])
    return(0)
}

D_Long <- D_Long |>
    mutate(
        n_pred_agro = C_agro("ln_P_CO2") + get_int_agro("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture + get_int_agro("ln_P_CO2", "z_pH") * z_pH + get_int_agro("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom + get_int_agro("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,
        ln_K_pred_agro = C_agro("(Intercept)") + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
        b_power_agro = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
        inv_b_agro = 1 / b_power_agro
    ) |>
    filter(annual_P_uptake > 0) |>
    mutate(
        z_inv_b_agro = as.numeric(scale(inv_b_agro)),
        z_k = as.numeric(scale(k)),
        z_v0 = as.numeric(scale(k * soil_0_20_P_CO2)),
        z_fert_N = as.numeric(scale(fert_N_tot)),
        site = as.factor(site),
        year_f = as.factor(year),
        crop = droplevels(as.factor(crop))
    )
    
D_Long_Agro <- D_Long |> filter(is.finite(z_inv_b_agro))
n_crops <- length(levels(D_Long_Agro$crop))

cat("D_Long_Agro rows:", nrow(D_Long_Agro), "\n")
cat("n_crops:", n_crops, "\n")
cat("Crop levels:", paste(levels(D_Long_Agro$crop), collapse=", "), "\n")

cat("\n--- Running Absolute Uptake Model ---\n")
mod <- tryCatch({
    nlme(
        annual_P_uptake ~ ((V_max + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_CO2) /
            ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + soil_0_20_P_CO2),
        data = D_Long_Agro, 
        fixed = list(V_max ~ crop, beta_temp ~ 1, beta_N ~ 1, K_base ~ crop, beta_invb ~ 1, beta_v0 ~ 1), 
        random = V_max ~ 1 | site/year_f,
        weights = varPower(form = ~ fitted(.)),
        start = c(30, rep(0, n_crops - 1), 0, 0, median(D_Long_Agro$soil_0_20_P_CO2), rep(0, n_crops - 1), 0, 0), 
        control = nlmeControl(maxIter = 1000, msVerbose = TRUE)
    )
}, error = function(e) {
    cat("ERROR IN NLME:\n")
    print(e)
    return(NULL)
})

cat("\nDone!\n")
