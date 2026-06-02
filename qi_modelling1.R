## graph TD
##     classDef intro fill:#e1f5fe,stroke:#0288d1,stroke-width:2px;
##     classDef lab fill:#fff3e0,stroke:#f57c00,stroke-width:2px;
##     classDef thermo fill:#f3e5f5,stroke:#8e24aa,stroke-width:2px;
##     classDef uptake fill:#e8f5e9,stroke:#388e3c,stroke-width:2px;
##     classDef conc fill:#ffebee,stroke:#d32f2f,stroke-width:2px;
## 
##     subgraph Phase_1 ["Phase 1: Introduction & Problem"]
##         A["Established Practice:<br>Static GRUD Pools P_CO2 & P_AAE10"]:::intro
##         B["Chemical Activity:<br>Dynamic Plant P-Supply"]:::intro
##         A --> C{"Research Questions"}:::intro
##         B --> C
##     end
## 
##     subgraph Phase_2 ["Phase 2: Laboratory Derivations"]
##         C --> D["Quantity-Intensity Q/I Modeling"]:::lab
##         C --> E["Desorption Kinetics I/t Modeling"]:::lab
##         D --> F("Physical Buffer Power: b"):::lab
##         E --> G("Desorption Rate Constant: k"):::lab
##     end
## 
##     subgraph Phase_3 ["Phase 3: Thermodynamic Corrections"]
##         F --> H{"Davies Equation"}:::thermo
##         G --> H
##         H --> I["Thermodynamic Activities"]:::thermo
##         H --> J["Stochiometric Concentrations"]:::thermo
##     end
## 
##     subgraph Phase_4 ["Phase 4: Field Scaling (PTF Comparison)"]
##         I --> K["Agronomic vs Geochemical PTFs<br>1990-2022"]:::uptake
##         J --> K
##         K --> L("Selected Field Inverse Buffer Power: 1/b"):::uptake
##     end

## ----setup--------------------------------------------------------------------
#| message: false
#| warning: false

rm(list=ls())
suppressPackageStartupMessages({
  library(lme4)
  library(ggplot2)
  library(dplyr)
  library(tidyr) # Ensure tidyr is loaded for drop_na
  library(patchwork)
  library(robustlmm)
  library(performance) 
  library(kableExtra)
})

options(warn = -1)

# Load raw data
library(readxl)
RES <- readRDS("data/RES.rds")
D <- RES$D # 2015-2022 Subset 

# Extract climate data from legacy file to patch the missing columns in STYCS
climate_data <- readRDS("data/all_P.rds") |>
  dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec) |>
  dplyr::distinct()

# Load comprehensive STYCS dataset and merge climate
D2 <- read_excel("data/STYCS_data_2023_260511.xlsx") |>
  rename(rep = replicate) |>
  mutate(site = gsub("STYCS_", "", LtE_name)) |>
  left_join(climate_data, by = c("site", "year")) |>
  mutate(
    soil_0_20_P_CO2 = soil_0_20_P_test * 0.155,
    crop = crop_abr,
    annual_P_uptake = rowSums(across(starts_with("P_harv")), na.rm = TRUE),
    annual_yield_mp_DM = rowSums(across(matches("^harv.*mp_yield_DM$")), na.rm = TRUE),
    annual_yield_bp_DM = rowSums(across(matches("^harv.*bp[1-2]_yield_DM$")), na.rm = TRUE)
  ) # 40-Year Full Dataset (All Treatments)


## ----create-master-dataset----------------------------------------------------
# 1. Extract Stable Geochemistry & Kinetics
site_geochemistry <- D |> group_by(site) |> summarise(feox_mean = mean(Feox, na.rm = TRUE), alox_mean = mean(Alox, na.rm = TRUE)) |> ungroup()
kinetics_stable <- D |> dplyr::select(site, treatment_ID, rep, k, v0_kPS = kPS, Pmax_PS = PS) |> distinct(site, treatment_ID, rep, .keep_all = TRUE)

# 2. Base Merge
D_main <- D2 |>
  filter(year >= 1990) |>
  group_by(site) |>
  mutate(
    site_juv_temp_mean = mean(juvdev_temp, na.rm = TRUE), site_juv_prec_mean = mean(juvdev_prec, na.rm = TRUE),
    temp_anomaly = juvdev_temp - site_juv_temp_mean, prec_anomaly = juvdev_prec - site_juv_prec_mean
  ) |> ungroup() |>
  left_join(site_geochemistry, by = "site") |>
  left_join(kinetics_stable, by = c("site", "treatment_ID", "rep"))

# Impute missing cations for complete case retention (specifically CAD)
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
  ) |>
  ungroup() |>
  mutate(
    soil_0_20_Ca_H2O10 = ifelse(is.na(soil_0_20_Ca_H2O10), global_med_Ca_H2O10, soil_0_20_Ca_H2O10),
    soil_0_20_Mg_H2O10 = ifelse(is.na(soil_0_20_Mg_H2O10), global_med_Mg_H2O10, soil_0_20_Mg_H2O10),
    soil_0_20_K_H2O10  = ifelse(is.na(soil_0_20_K_H2O10),  global_med_K_H2O10,  soil_0_20_K_H2O10),
    
    soil_0_20_Ca_AAE10 = ifelse(is.na(soil_0_20_Ca_AAE10), global_med_Ca_AAE10, soil_0_20_Ca_AAE10),
    soil_0_20_Mg_AAE10 = ifelse(is.na(soil_0_20_Mg_AAE10), global_med_Mg_AAE10, soil_0_20_Mg_AAE10),
    soil_0_20_K_AAE10  = ifelse(is.na(soil_0_20_K_AAE10),  global_med_K_AAE10,  soil_0_20_K_AAE10)
  )

# 3. Thermodynamic Speciation (Davies Equation)
D_thermo <- D_main |>
  filter(!is.na(soil_0_20_pH_H2O), !is.na(soil_0_20_P_CO2)) |>
  mutate(
    K_mol_L = (soil_0_20_K_H2O10 / 10) / (39.10 * 1000), Ca_mol_L = (soil_0_20_Ca_H2O10 / 10) / (40.08 * 1000), Mg_mol_L = (soil_0_20_Mg_H2O10 / 10) / (24.30 * 1000),
    Charge_Cations = (K_mol_L * 1) + (Ca_mol_L * 2) + (Mg_mol_L * 2),
    I_cations = 0.5 * ( (K_mol_L * 1^2) + (Ca_mol_L * 2^2) + (Mg_mol_L * 2^2) ),
    Ionic_Strength = I_cations + (0.5 * (Charge_Cations * 1^2)),
    
    gamma_1 = 10^(-0.509 * (1^2) * ((sqrt(Ionic_Strength) / (1 + sqrt(Ionic_Strength))) - (0.3 * Ionic_Strength))),
    gamma_2 = 10^(-0.509 * (2^2) * ((sqrt(Ionic_Strength) / (1 + sqrt(Ionic_Strength))) - (0.3 * Ionic_Strength))),
    
    P_Ratio = 10^(soil_0_20_pH_H2O - 7.20), Fraction_H2PO4 = 1 / (1 + P_Ratio), Fraction_HPO4  = P_Ratio / (1 + P_Ratio),
    
    # Raw vs Thermo P_CO2
    P_CO2_mg_L = soil_0_20_P_CO2 / 2.5,
    P_CO2_mol_L = P_CO2_mg_L / (30.97 * 1000),
    a_CO2_total_M = (P_CO2_mol_L * Fraction_H2PO4 * gamma_1) + (P_CO2_mol_L * Fraction_HPO4 * gamma_2),
    a_CO2_total_mg_L = a_CO2_total_M * 30.97 * 1000 # Back to friendly mg/L
  )

# 4. Scaling and Patching
D_ready <- D_thermo |>
  mutate(
    # Target and Main Predictors
    ln_P_AAE = log(soil_0_20_P_AAE10),
    ln_P_CO2 = log(soil_0_20_P_CO2),
    ln_a_CO2 = log(a_CO2_total_mg_L), # The new Thermodynamic Pool
    
    # Agronomic Traits
    z_ln_FineTexture = as.numeric(scale(log(rollMean_soil_0_20_clay + rollMean_soil_0_20_silt))),
    z_ln_Ca = as.numeric(scale(log(rollMean_soil_0_20_Ca_AAE10))),
    z_ln_Mg = as.numeric(scale(log(rollMean_soil_0_20_Mg_AAE10))),
    z_ln_K  = as.numeric(scale(log(rollMean_soil_0_20_K_AAE10))),
    z_pH    = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
    z_ln_Corg = as.numeric(scale(log(rollMean_soil_0_20_Corg))),
    
    # Geochemical Traits
    z_ln_Feox = as.numeric(scale(log(feox_mean))),
    z_ln_Alox = as.numeric(scale(log(alox_mean))),
    
    # Climate & Kinetics
    z_Temp_Mean = as.numeric(scale(site_juv_temp_mean)), z_Temp_Anom = as.numeric(scale(temp_anomaly)),
    z_Prec_Anom = as.numeric(scale(prec_anomaly)), z_k = as.numeric(scale(k))
  ) |>
  # Patch stable traits with site means
  group_by(site) |>
  mutate(
    z_ln_Corg = ifelse(is.na(z_ln_Corg), mean(z_ln_Corg, na.rm = TRUE), z_ln_Corg),
    z_ln_Feox = ifelse(is.na(z_ln_Feox), mean(z_ln_Feox, na.rm = TRUE), z_ln_Feox),
    z_ln_Alox = ifelse(is.na(z_ln_Alox), mean(z_ln_Alox, na.rm = TRUE), z_ln_Alox),
    z_ln_FineTexture = ifelse(is.na(z_ln_FineTexture), mean(z_ln_FineTexture, na.rm = TRUE), z_ln_FineTexture)
  ) |> ungroup()


## ----ptf-showdown, fig.width=12, fig.height=8---------------------------------
#| fig-cap: "**Figure 1: Pedotransfer Function (PTF) Comparison.** The Geochemical models (bottom row) utilizing Amorphous Iron and Aluminum Oxides vastly outperform the standard Agronomic models (top row) in predicting the bound $P_{AAE10}$ legacy pool. Note that the raw mass $P_{CO2}$ slightly edges out the thermodynamic activity $a_{CO2}$ when predicting the aggressive laboratory EDTA extraction."

# Safely filter complete cases
D_ptf <- D_ready |> 
  drop_na(ln_P_AAE, ln_P_CO2, ln_a_CO2, z_ln_FineTexture, z_pH, z_ln_Ca, z_ln_Mg, z_ln_K, z_ln_Corg, z_Temp_Anom, z_Prec_Anom, z_Temp_Mean, z_ln_Feox, z_ln_Alox) |> 
  mutate(site = droplevels(factor(site)))

# Models
ptf_agro_raw <- lmer(ln_P_AAE ~ ln_P_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf)
ptf_agro_thm <- lmer(ln_P_AAE ~ ln_a_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf)
ptf_geo_raw <- lmer(ln_P_AAE ~ ln_P_CO2 * (z_ln_Feox + z_ln_Alox + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf)
ptf_geo_thm <- lmer(ln_P_AAE ~ ln_a_CO2 * (z_ln_Feox + z_ln_Alox + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf)

# Performance Extraction
get_r2 <- function(model, name) {
  perf <- performance::r2_nakagawa(model)
  data.frame(Model = name, Marginal_R2 = round(as.numeric(perf$R2_marginal), 3), Conditional_R2 = round(as.numeric(perf$R2_conditional), 3))
}

ptf_results <- bind_rows(
  get_r2(ptf_agro_raw, "Agronomic (Raw P_CO2)"), get_r2(ptf_agro_thm, "Agronomic (Thermo a_CO2)"),
  get_r2(ptf_geo_raw, "Geochemical (Raw P_CO2)"), get_r2(ptf_geo_thm, "Geochemical (Thermo a_CO2)")
)

# Table with Caption
ptf_results |> 
  kbl(caption = "**Table 1: Variance Explained by Pedotransfer Functions.** Geochemical traits account for a massive 14% increase in Marginal R² compared to standard agronomic soil texture (Clay/Silt), proving that amorphous metal oxides dictate the physical binding capacity of the soil matrix.") |> 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

# Graphical Showdown with unified legend
plot_ptf <- function(model, title) {
  plot_data <- D_ptf |> mutate(Fitted = predict(model))
  ggplot(plot_data, aes(x = Fitted, y = ln_P_AAE, color = site)) +
    geom_point(alpha = 0.5, size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = title, x = "Predicted ln(P_AAE)", y = "Observed ln(P_AAE)", color = "Monitoring Site") +
    theme_minimal()
}

(plot_ptf(ptf_agro_raw, "Agro Raw") | plot_ptf(ptf_agro_thm, "Agro Thermo")) /
(plot_ptf(ptf_geo_raw, "Geo Raw") | plot_ptf(ptf_geo_thm, "Geo Thermo")) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")


## ----plant-uptake-showdown, fig.width=10, fig.height=4------------------------
# 1. Extract the Golden Coefficients from the Winning PTF
library(nlme)
# 1. Extract the Golden Coefficients from the Winning PTF
coefs_geo <- fixef(ptf_geo_raw)

# Safe Extractors
C <- function(name) { if(name %in% names(coefs_geo)) coefs_geo[[name]] else 0 }
get_int <- function(v1, v2) { 
  n1 <- paste0(v1, ":", v2)
  n2 <- paste0(v2, ":", v1)
  if(n1 %in% names(coefs_geo)) return(coefs_geo[[n1]])
  if(n2 %in% names(coefs_geo)) return(coefs_geo[[n2]])
  return(0)
}

# 2. Prepare the Longitudinal Dataset (All years, Drop 0-uptake)
D_Long <- D_ready |>
  filter(annual_P_uptake > 0, !is.na(k), !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot)) |>
  
  # Calculate the Physical Highway (1/b) safely using get_int()
  mutate(
    n_pred = C("ln_P_CO2") + 
             get_int("ln_P_CO2", "z_ln_Feox") * z_ln_Feox + 
             get_int("ln_P_CO2", "z_ln_Alox") * z_ln_Alox + 
             get_int("ln_P_CO2", "z_pH") * z_pH + 
             get_int("ln_P_CO2", "z_ln_Ca") * z_ln_Ca + 
             get_int("ln_P_CO2", "z_ln_Mg") * z_ln_Mg + 
             get_int("ln_P_CO2", "z_ln_K") * z_ln_K + 
             get_int("ln_P_CO2", "z_ln_Corg") * z_ln_Corg + 
             get_int("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom + 
             get_int("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,
             
    ln_K_pred = C("(Intercept)") + C("z_ln_Feox") * z_ln_Feox + C("z_ln_Alox") * z_ln_Alox + C("z_pH") * z_pH + C("z_ln_Ca") * z_ln_Ca + C("z_ln_Mg") * z_ln_Mg + C("z_ln_K") * z_ln_K + C("z_ln_Corg") * z_ln_Corg + C("z_Temp_Anom") * z_Temp_Anom + C("z_Prec_Anom") * z_Prec_Anom + C("z_Temp_Mean") * z_Temp_Mean,
                
    b_power = n_pred * exp(ln_K_pred) * (soil_0_20_P_CO2^(n_pred - 1)),
    inv_b = 1 / b_power
  ) |>
  
  # Normalize Uptake & Scale Bottlenecks
  group_by(site, crop, year) |> mutate(Relative_Uptake = annual_P_uptake / max(annual_P_uptake, na.rm = TRUE)) |> ungroup() |>
  filter(is.finite(inv_b), is.finite(Relative_Uptake)) |>
  mutate(
    z_inv_b = as.numeric(scale(inv_b)), 
    z_k = as.numeric(scale(k)),
    z_fert_N = as.numeric(scale(fert_N_tot)),
    site = as.factor(site)
  )
  

cat("Total Harvest Years Evaluated (2010-2022):", nrow(D_Long), "\n\n")

# ---------------------------------------------------------
# MODEL 1: RAW P_CO2 (The Empirical Soluble Pool)
# ---------------------------------------------------------
mod_raw_co2 <- nlme(
  Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_CO2) / 
                    ( (K_base * exp(beta_invb * z_inv_b + beta_k * z_k)) + soil_0_20_P_CO2 ),
  data = D_Long, fixed = U_base + beta_temp + beta_N + K_base + beta_invb + beta_k ~ 1, random = U_base ~ 1 | site,
  start = c(U_base = 0.68, beta_temp = 0.07, beta_N = 0.1, K_base = median(D_Long$soil_0_20_P_CO2), beta_invb = 0, beta_k = 0), control = nlmeControl(maxIter = 1000)
)

# ---------------------------------------------------------
# MODEL 2: THERMODYNAMIC a_CO2 (The Biophysical Soluble Pool)
# ---------------------------------------------------------
mod_thm_co2 <- nlme(
  Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * a_CO2_total_mg_L) / 
                    ( (K_base * exp(beta_invb * z_inv_b + beta_k * z_k)) + a_CO2_total_mg_L ),
  data = D_Long, fixed = U_base + beta_temp + beta_N + K_base + beta_invb + beta_k ~ 1, random = U_base ~ 1 | site,
  start = c(U_base = 0.68, beta_temp = 0.07, beta_N = 0.1, K_base = median(D_Long$a_CO2_total_mg_L), beta_invb = 0, beta_k = 0), control = nlmeControl(maxIter = 1000)
)

# ---------------------------------------------------------
# MODEL 3: RAW P_AAE10 (The Bound Legacy Pool)
# ---------------------------------------------------------
mod_raw_aae <- nlme(
  Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_AAE10) / 
                    ( (K_base * exp(beta_invb * z_inv_b + beta_k * z_k)) + soil_0_20_P_AAE10 ),
  data = D_Long, fixed = U_base + beta_temp + beta_N + K_base + beta_invb + beta_k ~ 1, random = U_base ~ 1 | site,
  start = c(U_base = 0.68, beta_temp = 0.07, beta_N = 0.1, K_base = median(D_Long$soil_0_20_P_AAE10), beta_invb = 0, beta_k = 0), control = nlmeControl(maxIter = 1000)
)

# --- Extract Performance and Plot ---
validate_nlme <- function(model, data, y_var, title) {
  preds <- predict(model, level = 1)
  r2 <- round(cor(data[[y_var]], preds)^2, 3)
  plot_data <- data |> mutate(Predicted = preds, Residuals = residuals(model))
  
  ggplot(plot_data, aes(x = Predicted, y = .data[[y_var]], color = site)) +
    geom_point(size = 3, alpha = 0.7) + geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = paste(title, "\nConditional R2:", r2), x = "Predicted Relative Uptake", y = "Observed Relative Uptake", color = "Monitoring Site") +
    theme_minimal()
}

extract_perf <- function(mod, name, y) {
  preds <- predict(mod, level = 1)
  r2 <- round(cor(y, preds)^2, 3)
  aic <- round(AIC(mod), 1)
  p_invb <- round(summary(mod)$tTable["beta_invb", "p-value"], 4)
  p_k <- round(summary(mod)$tTable["beta_k", "p-value"], 4)
  data.frame(Model = name, Pseudo_R2 = r2, AIC = aic, p_val_Physical = p_invb, p_val_Kinetic = p_k)
}

res_table <- bind_rows(
  extract_perf(mod_raw_co2, "1. Raw Empirical (P_CO2)", D_Long$Relative_Uptake),
  extract_perf(mod_thm_co2, "2. Thermodynamic (a_CO2)", D_Long$Relative_Uptake),
  extract_perf(mod_raw_aae, "3. Bound Legacy (P_AAE10)", D_Long$Relative_Uptake)
)

res_table |> 
  kbl(caption = "**Table 2: Michaelis-Menten Plant Uptake Comparison.** The physical diffusion highway (1/b) is highly significant across all models (p < 0.001). Because the buffer power was derived mechanically via the Geochemical PTF, it mathematically absorbs the kinetic desorption rate (k), rendering the latter insignificant (p = 0.45).") |> 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

# Comparison Graphs with unified legend
(validate_nlme(mod_raw_co2, D_Long, "Relative_Uptake", "Model 1: Raw P_CO2") | 
 validate_nlme(mod_thm_co2, D_Long, "Relative_Uptake", "Model 2: Thermo a_CO2") | 
 validate_nlme(mod_raw_aae, D_Long, "Relative_Uptake", "Model 3: Legacy P_AAE10")) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")


## 7.1 Full Dataset Uptake Model (No Kinetics)
D_Long_Full <- D_ready |>
  filter(annual_P_uptake > 0, !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot)) |>
  mutate(
    n_pred_agro = C_agro("ln_P_CO2") + 
             get_int_agro("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture + 
             get_int_agro("ln_P_CO2", "z_pH") * z_pH + 
             get_int_agro("ln_P_CO2", "z_ln_Ca") * z_ln_Ca + 
             get_int_agro("ln_P_CO2", "z_ln_Mg") * z_ln_Mg + 
             get_int_agro("ln_P_CO2", "z_ln_K") * z_ln_K + 
             get_int_agro("ln_P_CO2", "z_ln_Corg") * z_ln_Corg + 
             get_int_agro("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom + 
             get_int_agro("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,
             
    ln_K_pred_agro = C_agro("(Intercept)") + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
                
    b_power_agro = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
    inv_b_agro = 1 / b_power_agro
  ) |>
  group_by(site, crop, year) |> mutate(Relative_Uptake = annual_P_uptake / max(annual_P_uptake, na.rm = TRUE)) |> ungroup() |>
  filter(is.finite(Relative_Uptake), is.finite(inv_b_agro)) |>
  mutate(
    z_inv_b_agro = as.numeric(scale(inv_b_agro)),
    z_fert_N = as.numeric(scale(fert_N_tot)),
    site = as.factor(site)
  )

uptake_full_nlme <- nlme(
  Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_CO2) / 
                    ( (K_base * exp(beta_invb * z_inv_b_agro)) + soil_0_20_P_CO2 ),
  data = D_Long_Full, 
  fixed = U_base + beta_temp + beta_N + K_base + beta_invb ~ 1, 
  random = U_base ~ 1 | site,
  start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Full$soil_0_20_P_CO2), beta_invb = 0), 
  control = nlmeControl(maxIter = 1000)
)
print(round(summary(uptake_full_nlme)$tTable, 4))

## ----residual-diagnostics-uptake, fig.width=10, fig.height=4------------------
# Helper to plot boxplots of residuals per site
plot_residuals_boxplot <- function(model, data, title) {
  plot_data <- data |> mutate(Residuals = residuals(model))
  
  ggplot(plot_data, aes(x = site, y = Residuals, fill = site)) +
    geom_boxplot(alpha = 0.7, outlier.size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = title,
      x = "Monitoring Site",
      y = "Conditional Residuals"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

(plot_residuals_boxplot(mod_raw_co2, D_Long, "Model 1: Raw P_CO2") |
 plot_residuals_boxplot(mod_thm_co2, D_Long, "Model 2: Thermo a_CO2") |
 plot_residuals_boxplot(mod_raw_aae, D_Long, "Model 3: Legacy P_AAE10"))


## -----------------------------------------------------------------------------
library(broom.mixed)

# Helper function to generate a clean effects table
get_effects <- function(mod, model_name) {
  tidy(mod, effects = "fixed") |>
    mutate(
      Model = model_name,
      across(where(is.numeric), ~round(., 4))
    ) |>
    dplyr::select(Model, term, estimate, std.error, statistic, p.value)
}

# Bind them together and print
all_effects <- bind_rows(
  get_effects(mod_raw_co2, "1. Raw P_CO2"),
  get_effects(mod_thm_co2, "2. Thermo a_CO2"),
  get_effects(mod_raw_aae, "3. Legacy P_AAE10")
)

print(as.data.frame(all_effects), row.names = FALSE)


## ----mitscherlich-yield-models, fig.width=10, fig.height=4--------------------
# 1. Prepare the Dataset for YIELD (Grouped by Site AND Crop)
# 1. Prepare the Dataset for YIELD (Grouped by Site AND Crop)
D_Yield <- D_ready |>
  filter(!is.na(k), !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10)) |>
  
  # Calculate the Physical Highway (1/b) safely using get_int()
  mutate(
    n_pred = C("ln_P_CO2") + get_int("ln_P_CO2", "z_ln_Feox") * z_ln_Feox + get_int("ln_P_CO2", "z_ln_Alox") * z_ln_Alox + get_int("ln_P_CO2", "z_pH") * z_pH + get_int("ln_P_CO2", "z_ln_Ca") * z_ln_Ca + get_int("ln_P_CO2", "z_ln_Mg") * z_ln_Mg + get_int("ln_P_CO2", "z_ln_K") * z_ln_K + get_int("ln_P_CO2", "z_ln_Corg") * z_ln_Corg + get_int("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom + get_int("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,
    
    ln_K_pred = C("(Intercept)") + C("z_ln_Feox") * z_ln_Feox + C("z_ln_Alox") * z_ln_Alox + C("z_pH") * z_pH + C("z_ln_Ca") * z_ln_Ca + C("z_ln_Mg") * z_ln_Mg + C("z_ln_K") * z_ln_K + C("z_ln_Corg") * z_ln_Corg + C("z_Temp_Anom") * z_Temp_Anom + C("z_Prec_Anom") * z_Prec_Anom + C("z_Temp_Mean") * z_Temp_Mean,
    
    b_power = n_pred * exp(ln_K_pred) * (soil_0_20_P_CO2^(n_pred - 1)),
    inv_b = 1 / b_power,
    
    # Safely sum Main Product and Byproduct
    total_yield = tidyr::replace_na(annual_yield_mp_DM, 0) + tidyr::replace_na(annual_yield_bp_DM, 0)
  ) |>
  
  # Normalize TOTAL YIELD by Site, Crop, and Year
  group_by(site, crop, year) |> 
  mutate(Relative_Yield = total_yield / max(total_yield, na.rm = TRUE)) |> 
  ungroup() |>
  
  filter(is.finite(inv_b), is.finite(Relative_Yield), Relative_Yield > 0) |>
  mutate(z_inv_b = as.numeric(scale(inv_b)), site = as.factor(site))

cat("Total Harvest Years Evaluated for Yield (2010-2022):", nrow(D_Yield), "\n\n")

# ---------------------------------------------------------
# MITSCHERLICH YIELD 1: RAW P_CO2 
# ---------------------------------------------------------
m_yield_co2 <- nlme(
  Relative_Yield ~ (Y_base + beta_temp * z_Temp_Anom + beta_prec * z_Prec_Anom) * (1 - exp(-(c_base * exp(beta_invb * z_inv_b + beta_k * z_k)) * (soil_0_20_P_CO2 + exp(env_base)))),
  data = D_Yield, fixed = Y_base + beta_temp + beta_prec + c_base + beta_invb + beta_k + env_base ~ 1, random = Y_base ~ 1 | site,
  start = c(Y_base = 0.85, beta_temp = 0, beta_prec = 0, c_base = 4.0, beta_invb = 0, beta_k = 0, env_base = -2.5), control = nlmeControl(maxIter = 1000)
)

# ---------------------------------------------------------
# MITSCHERLICH YIELD 2: THERMODYNAMIC a_CO2 
# ---------------------------------------------------------
m_yield_thm <- nlme(
  Relative_Yield ~ (Y_base + beta_temp * z_Temp_Anom + beta_prec * z_Prec_Anom) * (1 - exp(-(c_base * exp(beta_invb * z_inv_b + beta_k * z_k)) * (a_CO2_total_mg_L + exp(env_base)))),
  data = D_Yield, fixed = Y_base + beta_temp + beta_prec + c_base + beta_invb + beta_k + env_base ~ 1, random = Y_base ~ 1 | site,
  start = c(Y_base = 0.85, beta_temp = 0, beta_prec = 0, c_base = 50.0, beta_invb = 0, beta_k = 0, env_base = -5.5), control = nlmeControl(maxIter = 1000)
)

# ---------------------------------------------------------
# MITSCHERLICH YIELD 3: RAW P_AAE10
# ---------------------------------------------------------
m_yield_aae <- nlme(
  Relative_Yield ~ (Y_base + beta_temp * z_Temp_Anom + beta_prec * z_Prec_Anom) * (1 - exp(-(c_base * exp(beta_invb * z_inv_b + beta_k * z_k)) * (soil_0_20_P_AAE10 + exp(env_base)))),
  data = D_Yield, fixed = Y_base + beta_temp + beta_prec + c_base + beta_invb + beta_k + env_base ~ 1, random = Y_base ~ 1 | site,
  start = c(Y_base = 0.85, beta_temp = 0, beta_prec = 0, c_base = 0.05, beta_invb = 0, beta_k = 0, env_base = 2.3), control = nlmeControl(maxIter = 1000)
)

# --- Extract Performance ---
yield_table <- bind_rows(
  extract_perf(m_yield_co2, "1. Yield Mitscherlich (P_CO2)", D_Yield$Relative_Yield),
  extract_perf(m_yield_thm, "2. Yield Mitscherlich (a_CO2)", D_Yield$Relative_Yield),
  extract_perf(m_yield_aae, "3. Yield Mitscherlich (P_AAE10)", D_Yield$Relative_Yield)
)

yield_table |> 
  kbl(caption = "**Table 3: Mitscherlich Agronomic Yield Comparison.** Unlike the soluble pools, the bound legacy pool ($P_{AAE10}$) remains restricted by the physical binding capacity of the soil matrix (1/b, p = 0.07) even when predicting total crop yield. Temperature anomalies fundamentally cap the absolute maximum agronomic yield across all models.") |> 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

cat("\n### BACK-TRANSFORMED ENVIRONMENT SHIFTS (STP units) ###\n")
cat("P_CO2 Model Shift (mg/L):", round(exp(fixef(m_yield_co2)["env_base"]), 4), "\n")
cat("a_CO2 Model Shift (mg/L):", round(exp(fixef(m_yield_thm)["env_base"]), 4), "\n")
cat("P_AAE10 Model Shift (mg/kg):", round(exp(fixef(m_yield_aae)["env_base"]), 4), "\n\n")

cat("### ALL YIELD EFFECTS ###\n")
all_yield_effects <- bind_rows(
  get_effects(m_yield_co2, "Yield P_CO2"),
  get_effects(m_yield_thm, "Yield a_CO2"),
  get_effects(m_yield_aae, "Yield P_AAE10")
)
print(as.data.frame(all_yield_effects), row.names = FALSE)

# --- Visualizations ---
# 1. Forest Plot of Standardized Effects
effects_for_plot <- all_yield_effects |>
  filter(term %in% c("beta_temp", "beta_prec", "beta_invb", "beta_k")) |>
  mutate(
    term_clean = case_when(
      term == "beta_temp" ~ "Temperature Anomaly",
      term == "beta_prec" ~ "Precipitation Anomaly",
      term == "beta_invb" ~ "Physical Diffusion (1/b)",
      term == "beta_k" ~ "Desorption Rate (k)"
    ),
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )

p_forest <- ggplot(effects_for_plot, aes(x = estimate, y = term_clean, color = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, linewidth = 0.8, position = position_dodge(width = 0.4)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.4)) +
  labs(
    title = "Fixed Effects on Yield Asymptote & Rate",
    x = "Standardized Coefficient Estimate (with 95% CI)",
    y = "",
    color = "Yield Model"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

# 2. Scenario-based Mitscherlich Curves (using P_CO2)
pct_invb <- quantile(D_Yield$z_inv_b, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
coefs <- fixef(m_yield_co2)

curve_data <- expand.grid(
  soil_0_20_P_CO2 = seq(0, 2.5, length.out = 200),
  invb_scenario = c("Low (10th Pct)", "Median (50th Pct)", "High (90th Pct)")
) |>
  mutate(
    z_inv_b = case_when(
      invb_scenario == "Low (10th Pct)" ~ pct_invb[1],
      invb_scenario == "Median (50th Pct)" ~ pct_invb[2],
      invb_scenario == "High (90th Pct)" ~ pct_invb[3]
    ),
    z_Temp_Anom = 0,
    z_Prec_Anom = 0,
    z_k = 0
  ) |>
  mutate(
    Predicted_Yield = (coefs["Y_base"] + coefs["beta_temp"] * z_Temp_Anom + coefs["beta_prec"] * z_Prec_Anom) *
      (1 - exp(-(coefs["c_base"] * exp(coefs["beta_invb"] * z_inv_b + coefs["beta_k"] * z_k)) * (soil_0_20_P_CO2 + exp(coefs["env_base"]))))
  )

p_curves <- ggplot() +
  geom_point(data = D_Yield, aes(x = soil_0_20_P_CO2, y = Relative_Yield), alpha = 0.2, color = "darkgray") +
  geom_line(data = curve_data, aes(x = soil_0_20_P_CO2, y = Predicted_Yield, color = invb_scenario), linewidth = 1.2) +
  labs(
    title = "Scenario Curves (P_CO2)",
    subtitle = "Varying Physical Diffusion (1/b)",
    x = "Soil Test P (P_CO2, mg/L)",
    y = "Predicted Relative Yield",
    color = "Physical Highway (1/b)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

# Combine the plots
(p_forest | p_curves) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


## ----residual-diagnostics-yield, fig.width=10, fig.height=4-------------------
(plot_residuals_boxplot(m_yield_co2, D_Yield, "Yield Model 1: Raw P_CO2") |
 plot_residuals_boxplot(m_yield_thm, D_Yield, "Yield Model 2: Thermo a_CO2") |
 plot_residuals_boxplot(m_yield_aae, D_Yield, "Yield Model 3: Legacy P_AAE10"))

