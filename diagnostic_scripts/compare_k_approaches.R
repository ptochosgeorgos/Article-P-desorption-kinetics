library(dplyr)
library(ggplot2)
library(nlme)
library(patchwork)

# 1. Load data exactly as in the notebook
climate_data <- readRDS("data/all_P.rds") |> dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec) |> dplyr::distinct()
D2 <- readxl::read_excel("data/STYCS_data_2023_260511.xlsx") |>
    rename(rep = replicate) |>
    mutate(site = gsub("STYCS_", "", LtE_name)) |>
    left_join(climate_data, by = c("site", "year")) |>
    mutate(
        soil_0_20_P_CO2 = soil_0_20_P_test * 0.155,
        annual_P_uptake = rowSums(across(starts_with("P_harv")), na.rm = TRUE),
        fert_P_tot = fert_P2O5_tot / 2.291,
        annual_P_balance = fert_P_tot - annual_P_uptake
    )

D <- readRDS("data/RES.rds")$D
site_geochemistry <- D |> group_by(site) |> summarise(feox_mean = mean(Feox, na.rm = TRUE), alox_mean = mean(Alox, na.rm = TRUE)) |> ungroup()
kinetics_stable <- D |> dplyr::select(site, treatment_ID, rep, k) |> distinct(site, treatment_ID, rep, .keep_all = TRUE)

D_main <- D2 |> filter(year >= 1990) |> group_by(site) |> mutate(site_juv_temp_mean = mean(juvdev_temp, na.rm = TRUE), site_juv_prec_mean = mean(juvdev_prec, na.rm = TRUE), temp_anomaly = juvdev_temp - site_juv_temp_mean, prec_anomaly = juvdev_prec - site_juv_prec_mean) |> ungroup() |> left_join(site_geochemistry, by = "site") |> left_join(kinetics_stable, by = c("site", "treatment_ID", "rep"))

global_med_Ca_AAE10 <- median(D_main$soil_0_20_Ca_AAE10, na.rm = TRUE)
global_med_Mg_AAE10 <- median(D_main$soil_0_20_Mg_AAE10, na.rm = TRUE)
global_med_K_AAE10  <- median(D_main$soil_0_20_K_AAE10, na.rm = TRUE)

D_main <- D_main |> group_by(site) |> mutate(
    soil_0_20_Ca_AAE10 = ifelse(is.na(soil_0_20_Ca_AAE10), global_med_Ca_AAE10, soil_0_20_Ca_AAE10),
    soil_0_20_Mg_AAE10 = ifelse(is.na(soil_0_20_Mg_AAE10), global_med_Mg_AAE10, soil_0_20_Mg_AAE10),
    soil_0_20_K_AAE10  = ifelse(is.na(soil_0_20_K_AAE10),  global_med_K_AAE10,  soil_0_20_K_AAE10)
) |> ungroup()

# --- PREDICT k ---
# Approach 1: Site Median (drops GRA and unmeasured sites if not available, though kinetics_stable gives k per plot. For site median, we calc it)
k_site_median <- D_main |> filter(!is.na(k)) |> group_by(site) |> summarise(k_median = median(k, na.rm=TRUE)) |> ungroup()

# Approach 2: PTF
k_ptf_model <- lm(log(k) ~ log(alox_mean / feox_mean) + soil_0_20_pH_H2O, data = D_main |> filter(!is.na(k)))

D_main <- D_main |> 
    left_join(k_site_median, by = "site") |>
    mutate(
        k_ptf = exp(predict(k_ptf_model, newdata = D_main))
    )

# --- SCALING ---
D_ready <- D_main |>
    mutate(
        # Agronomic Traits
        z_ln_FineTexture = as.numeric(scale(log(rollMean_soil_0_20_clay + rollMean_soil_0_20_silt))),
        z_ln_Ca = as.numeric(scale(log(rollMean_soil_0_20_Ca_AAE10))),
        z_ln_Mg = as.numeric(scale(log(rollMean_soil_0_20_Mg_AAE10))),
        z_ln_K  = as.numeric(scale(log(rollMean_soil_0_20_K_AAE10))),
        z_pH    = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
        z_ln_Corg = as.numeric(scale(log(rollMean_soil_0_20_Corg))),
        # Geo
        z_ln_Feox = as.numeric(scale(log(feox_mean))),
        z_ln_Alox = as.numeric(scale(log(alox_mean))),
        # Climate
        z_Temp_Mean = as.numeric(scale(site_juv_temp_mean)), z_Temp_Anom = as.numeric(scale(temp_anomaly)),
        z_Prec_Anom = as.numeric(scale(prec_anomaly)),
        # Bottleneck (using Geo)
        inv_b_geo = z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom + z_ln_Feox + z_ln_Alox,
        z_inv_b_geo = as.numeric(scale(inv_b_geo)),
        fert_N_tot = ifelse(is.na(fert_N_tot), 0, fert_N_tot),
        z_fert_N = as.numeric(scale(fert_N_tot)),
        site = as.factor(site),
        year_f = as.factor(year)
    )

# Calculate Relative Uptake
D_Long <- D_ready |> filter(annual_P_uptake > 0, !is.na(soil_0_20_P_CO2), !is.na(fert_N_tot)) |>
    group_by(site, year, crop) |>
    mutate(
        ref_uptake = mean(annual_P_uptake[treatment_ID == "P166"], na.rm = TRUE),
        ref_uptake = ifelse(is.na(ref_uptake) | is.nan(ref_uptake), max(annual_P_uptake, na.rm = TRUE), ref_uptake),
        Relative_Uptake = annual_P_uptake / ref_uptake
    ) |> ungroup() |> filter(is.finite(Relative_Uptake))

# Dataset 1: Site Median (will drop GRA)
D_median <- D_Long |> filter(!is.na(k_median), !is.na(z_inv_b_geo)) |> mutate(z_v0 = as.numeric(scale(k_median * soil_0_20_P_CO2)))

# Dataset 2: PTF (includes GRA)
D_ptf <- D_Long |> filter(!is.na(k_ptf), !is.na(z_inv_b_geo)) |> mutate(z_v0 = as.numeric(scale(k_ptf * soil_0_20_P_CO2)))

# --- MODEL FITTING ---
cat("\nFitting Median Model (N=", nrow(D_median), ")\n")
mod_median <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_geo + beta_v0 * z_v0)) + soil_0_20_P_CO2),
    data = D_median,
    fixed = U_base + beta_temp + beta_N + beta_v0 + K_base + beta_invb ~ 1,
    random = list(site = pdDiag(U_base + K_base ~ 1), year_f = pdDiag(U_base + K_base ~ 1)),
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_median$soil_0_20_P_CO2), beta_invb = 0), 
    control = nlmeControl(maxIter = 1000)
)

cat("\nFitting PTF Model (N=", nrow(D_ptf), ")\n")
mod_ptf <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_geo + beta_v0 * z_v0)) + soil_0_20_P_CO2),
    data = D_ptf,
    fixed = U_base + beta_temp + beta_N + beta_v0 + K_base + beta_invb ~ 1,
    random = list(site = pdDiag(U_base + K_base ~ 1), year_f = pdDiag(U_base + K_base ~ 1)),
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_ptf$soil_0_20_P_CO2), beta_invb = 0), 
    control = nlmeControl(maxIter = 1000)
)

# Metrics
get_metrics <- function(mod, d) {
    p <- predict(mod)
    rmse <- sqrt(mean((d$Relative_Uptake - p)^2))
    mae <- mean(abs(d$Relative_Uptake - p))
    ccc <- DescTools::CCC(d$Relative_Uptake, p)$rho.c$est
    cor2 <- cor(d$Relative_Uptake, p)^2
    return(c(RMSE=rmse, MAE=mae, CCC=ccc, R2=cor2))
}
print("Metrics Median:")
print(get_metrics(mod_median, D_median))
print("Metrics PTF:")
print(get_metrics(mod_ptf, D_ptf))

# PLOTTING
D_median$Pred <- predict(mod_median)
D_ptf$Pred <- predict(mod_ptf)

p1 <- ggplot(D_median, aes(x = Pred, y = Relative_Uptake)) +
    geom_point(aes(color = site), alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    theme_bw() + labs(title = "Site-Median k: Predicted vs Real", x = "Predicted Relative Uptake", y = "Real Relative Uptake")

p2 <- ggplot(D_ptf, aes(x = Pred, y = Relative_Uptake)) +
    geom_point(aes(color = site), alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    theme_bw() + labs(title = "PTF k: Predicted vs Real", x = "Predicted Relative Uptake", y = "Real Relative Uptake")

p3 <- ggplot(D_median, aes(x = soil_0_20_P_CO2)) +
    geom_point(aes(y = Relative_Uptake), color = "black", alpha = 0.3) +
    geom_point(aes(y = Pred, color = site), alpha = 0.8) +
    theme_bw() + labs(title = "Site-Median k: Data vs Prediction", x = "P_CO2 (mg/L)", y = "Relative Uptake")

p4 <- ggplot(D_ptf, aes(x = soil_0_20_P_CO2)) +
    geom_point(aes(y = Relative_Uptake), color = "black", alpha = 0.3) +
    geom_point(aes(y = Pred, color = site), alpha = 0.8) +
    theme_bw() + labs(title = "PTF k: Data vs Prediction", x = "P_CO2 (mg/L)", y = "Relative Uptake")

ggsave("diagnostic_scripts/compare_k_pred_vs_real.png", (p1 | p2), width = 12, height = 5)
ggsave("diagnostic_scripts/compare_k_data_vs_pred.png", (p3 | p4), width = 12, height = 5)

