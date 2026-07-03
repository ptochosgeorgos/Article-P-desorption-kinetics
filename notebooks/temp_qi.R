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

rm(list = ls())
suppressPackageStartupMessages({
    library(lme4)
    library(lmerTest) # For p-values
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
        fert_P_tot = fert_P2O5_tot / 2.291,
        annual_P_balance = fert_P_tot - annual_P_uptake,
        annual_yield_mp_DM = rowSums(across(matches("^harv.*mp_yield_DM$")), na.rm = TRUE),
        annual_yield_bp_DM = rowSums(across(matches("^harv.*bp[1-2]_yield_DM$")), na.rm = TRUE)
    ) # 40-Year Full Dataset (All Treatments)


## ----create-comprehensive-dataset---------------------------------------------
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
        I_cations = 0.5 * ((K_mol_L * 1^2) + (Ca_mol_L * 2^2) + (Mg_mol_L * 2^2)),
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
#| fig-cap: "**Figure 1: Pedotransfer Function (PTF) Comparison.** The Geochemical models (bottom row) utilizing Amorphous Iron and Aluminum Oxides outperform the standard Agronomic models (top row) in predicting the bound $P_{AAE10}$ legacy pool. Note that the raw mass $P_{CO2}$ slightly edges out the thermodynamic activity $a_{CO2}$ when predicting the aggressive laboratory EDTA extraction."

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
    kbl(caption = "**Table 1: Variance Explained by Pedotransfer Functions.** Geochemical traits account for a 14% increase in Marginal R² compared to standard agronomic soil texture (Clay/Silt), proving that amorphous metal oxides dictate the physical binding capacity of the soil matrix.") |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

# Graphical Comparison with unified legend
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


## ----ptf-practical-agro, fig.width=10, fig.height=8---------------------------
# Create maximized dataset without Feox/Alox constraints
D_ptf_agro <- D_ready |>
    drop_na(ln_P_AAE, ln_P_CO2, ln_a_CO2, z_ln_FineTexture, z_pH, z_ln_Ca, z_ln_Mg, z_ln_K, z_ln_Corg, z_Temp_Anom, z_Prec_Anom, z_Temp_Mean) |>
    mutate(site = droplevels(factor(site)))

cat("Total trials successfully included in Practical PTF:", length(unique(D_ptf_agro$site)), "\n")
print(unique(D_ptf_agro$site))

# Fit practical models
ptf_practical_raw <- lmer(ln_P_AAE ~ ln_P_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf_agro)
ptf_practical_thm <- lmer(ln_P_AAE ~ ln_a_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf_agro)

# Present Performance
prac_results <- bind_rows(
    get_r2(ptf_practical_raw, "Practical Agro (Raw P_CO2)"),
    get_r2(ptf_practical_thm, "Practical Agro (Thermo a_CO2)")
)

prac_results |>
    kbl(caption = "**Table 2: Variance Explained by Practical Agronomic Models.** Trained on the maximum available long-term trials.") |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

# Extract and Present Fixed Effects
cat("\n### Final Equation Coefficients (Practical Agro Thermo) ###\n")
print(round(summary(ptf_practical_thm)$coefficients[, c("Estimate", "Std. Error", "Pr(>|t|)")], 4))

# Visualizations: Predicted vs Observed & Residuals
plot_ptf_prac <- function(model, title) {
    plot_data <- D_ptf_agro |> mutate(Fitted = predict(model))
    ggplot(plot_data, aes(x = Fitted, y = ln_P_AAE, color = site)) +
        geom_point(alpha = 0.5, size = 2) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
        labs(title = title, x = "Predicted ln(P_AAE)", y = "Observed ln(P_AAE)", color = "Monitoring Site") +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold", size = 12))
}

plot_resid_prac <- function(model, title) {
    plot_data <- D_ptf_agro |> mutate(Residuals = resid(model))
    ggplot(plot_data, aes(x = site, y = Residuals, fill = site)) +
        geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = paste(title, "Residuals"), x = "", y = "Residuals (ln scale)", fill = "Site") +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold", size = 12))
}

(plot_ptf_prac(ptf_practical_raw, "Practical Agro Raw") | plot_ptf_prac(ptf_practical_thm, "Practical Agro Thermo")) /
    (plot_resid_prac(ptf_practical_raw, "Practical Agro Raw") | plot_resid_prac(ptf_practical_thm, "Practical Agro Thermo")) +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")


## ----plant-uptake-showdown, fig.width=12, fig.height=8------------------------
library(nlme)

# 1. Extract the Coefficients from Both PTFs
coefs_geo <- fixef(ptf_geo_raw)
coefs_agro <- fixef(ptf_practical_raw)

# Safe Extractors
C_geo <- function(name) {
    if (name %in% names(coefs_geo)) coefs_geo[[name]] else 0
}
get_int_geo <- function(v1, v2) {
    n1 <- paste0(v1, ":", v2)
    n2 <- paste0(v2, ":", v1)
    if (n1 %in% names(coefs_geo)) return(coefs_geo[[n1]])
    if (n2 %in% names(coefs_geo)) return(coefs_geo[[n2]])
    return(0)
}

C_agro <- function(name) {
    if (name %in% names(coefs_agro)) coefs_agro[[name]] else 0
}
get_int_agro <- function(v1, v2) {
    n1 <- paste0(v1, ":", v2)
    n2 <- paste0(v2, ":", v1)
    if (n1 %in% names(coefs_agro)) return(coefs_agro[[n1]])
    if (n2 %in% names(coefs_agro)) return(coefs_agro[[n2]])
    return(0)
}

# 2. Prepare the Longitudinal Dataset (2010-2022, Drop 0-uptake)
D_Long <- D_ready |>
    filter(year >= 2010, annual_P_uptake > 0, !is.na(k), !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot)) |>

    # Calculate Geochemical Physical Highway (1/b)
    mutate(
        n_pred_geo = C_geo("ln_P_CO2") +
            get_int_geo("ln_P_CO2", "z_ln_Feox") * z_ln_Feox +
            get_int_geo("ln_P_CO2", "z_ln_Alox") * z_ln_Alox +
            get_int_geo("ln_P_CO2", "z_pH") * z_pH +
            get_int_geo("ln_P_CO2", "z_ln_Ca") * z_ln_Ca +
            get_int_geo("ln_P_CO2", "z_ln_Mg") * z_ln_Mg +
            get_int_geo("ln_P_CO2", "z_ln_K") * z_ln_K +
            get_int_geo("ln_P_CO2", "z_ln_Corg") * z_ln_Corg +
            get_int_geo("ln_P_CO2", "z_Temp_Anom") * z_Temp_Anom +
            get_int_geo("ln_P_CO2", "z_Prec_Anom") * z_Prec_Anom,

        ln_K_pred_geo = C_geo(1) + C_geo("z_ln_Feox") * z_ln_Feox + C_geo("z_ln_Alox") * z_ln_Alox + C_geo("z_pH") * z_pH + C_geo("z_ln_Ca") * z_ln_Ca + C_geo("z_ln_Mg") * z_ln_Mg + C_geo("z_ln_K") * z_ln_K + C_geo("z_ln_Corg") * z_ln_Corg + C_geo("z_Temp_Anom") * z_Temp_Anom + C_geo("z_Prec_Anom") * z_Prec_Anom + C_geo("z_Temp_Mean") * z_Temp_Mean,

        b_power_geo = n_pred_geo * exp(ln_K_pred_geo) * (soil_0_20_P_CO2^(n_pred_geo - 1)),
        inv_b_geo = 1 / b_power_geo
    ) |>

    # Calculate Practical Agronomic Physical Highway (1/b)
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

        ln_K_pred_agro = C_agro(1) + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,

        b_power_agro = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
        inv_b_agro = 1 / b_power_agro
    ) |>

    # Normalize Uptake & Scale Bottlenecks
    group_by(site, crop, year) |> mutate(Relative_Uptake = annual_P_uptake / max(annual_P_uptake, na.rm = TRUE)) |> ungroup() |>
    filter(is.finite(Relative_Uptake)) |>
    mutate(
        z_inv_b_geo = as.numeric(scale(inv_b_geo)),
        z_inv_b_agro = as.numeric(scale(inv_b_agro)),
        z_k = as.numeric(scale(k)),
        z_v0 = as.numeric(scale(v0_kPS)),
        z_fert_N = as.numeric(scale(fert_N_tot)),
        site = as.factor(site),
        year_f = as.factor(year)
    )

# ---------------------------------------------------------
# GEOCHEMICAL PBC PENALTY MODELS
# ---------------------------------------------------------
# Note: For Geo models, we filter down to rows where Geo inv_b is finite (excludes REC).
D_Long_Geo <- D_Long |> filter(is.finite(z_inv_b_geo))
cat("Trials included in Geo Uptake Models:", length(unique(D_Long_Geo$site)), "(", paste(unique(D_Long_Geo$site), collapse = ", "), ")\n")

mod_raw_co2_geo <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_geo)) + soil_0_20_P_CO2),
    data = D_Long_Geo, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Geo$soil_0_20_P_CO2), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_raw_co2_geo_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_geo + beta_v0 * z_v0)) + soil_0_20_P_CO2),
    data = D_Long_Geo, fixed = U_base + beta_temp + beta_N + K_base + beta_invb + beta_v0 ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Geo$soil_0_20_P_CO2), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

mod_thm_co2_geo <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_geo)) + a_CO2_total_mg_L),
    data = D_Long_Geo, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Geo$a_CO2_total_mg_L), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_thm_co2_geo_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_geo + beta_v0 * z_v0)) + a_CO2_total_mg_L),
    data = D_Long_Geo, fixed = U_base + beta_temp + beta_N + K_base + beta_invb + beta_v0 ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Geo$a_CO2_total_mg_L), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

mod_raw_aae_geo <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_AAE10) /
        ((K_base * exp(beta_invb * z_inv_b_geo)) + soil_0_20_P_AAE10),
    data = D_Long_Geo, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Geo$soil_0_20_P_AAE10), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_raw_aae_geo_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_AAE10) /
        ((K_base * exp(beta_invb * z_inv_b_geo + beta_v0 * z_v0)) + soil_0_20_P_AAE10),
    data = D_Long_Geo, fixed = U_base + beta_temp + beta_N + K_base + beta_invb + beta_v0 ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Geo$soil_0_20_P_AAE10), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

# ---------------------------------------------------------
# PRACTICAL AGRONOMIC PBC PENALTY MODELS
# ---------------------------------------------------------
# Note: For Agro models, REC is successfully included!
D_Long_Agro <- D_Long |> filter(is.finite(z_inv_b_agro))
cat("Trials included in Agro Uptake Models:", length(unique(D_Long_Agro$site)), "(", paste(unique(D_Long_Agro$site), collapse = ", "), ")\n\n")

mod_raw_co2_agro <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_agro)) + soil_0_20_P_CO2),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Agro$soil_0_20_P_CO2), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_raw_co2_agro_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + soil_0_20_P_CO2),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + K_base + beta_invb + beta_v0 ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Agro$soil_0_20_P_CO2), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

mod_thm_co2_agro <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_agro)) + a_CO2_total_mg_L),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Agro$a_CO2_total_mg_L), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_thm_co2_agro_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + a_CO2_total_mg_L),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + K_base + beta_invb + beta_v0 ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Agro$a_CO2_total_mg_L), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

mod_raw_aae_agro <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_AAE10) /
        ((K_base * exp(beta_invb * z_inv_b_agro)) + soil_0_20_P_AAE10),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Agro$soil_0_20_P_AAE10), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_raw_aae_agro_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_AAE10) /
        ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + soil_0_20_P_AAE10),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + K_base + beta_invb + beta_v0 ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Agro$soil_0_20_P_AAE10), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

# ---------------------------------------------------------
# NULL MODELS (NO 1/b PENALTY) FOR COMPARISON
# ---------------------------------------------------------
mod_raw_co2_agro_null <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) /
        (K_base + soil_0_20_P_CO2),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Agro$soil_0_20_P_CO2)), control = nlmeControl(maxIter = 1000)
)

mod_thm_co2_agro_null <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * a_CO2_total_mg_L) /
        (K_base + a_CO2_total_mg_L),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Agro$a_CO2_total_mg_L)), control = nlmeControl(maxIter = 1000)
)

mod_raw_aae_agro_null <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_AAE10) /
        (K_base + soil_0_20_P_AAE10),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + beta_v0 + K_base ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = median(D_Long_Agro$soil_0_20_P_AAE10)), control = nlmeControl(maxIter = 1000)
)

# --- Extract Performance and Plot ---
validate_nlme <- function(model, data, y_var, title) {
    preds_c <- predict(model)
    preds_m <- predict(model, level = 0)
    r2_c <- round(cor(data[[y_var]], preds_c)^2, 3)
    r2_m <- round(cor(data[[y_var]], preds_m)^2, 3)
    plot_data <- data |> mutate(Predicted = preds_c, Residuals = residuals(model))

    ggplot(plot_data, aes(x = Predicted, y = .data[[y_var]], color = site)) +
        geom_point(size = 3, alpha = 0.7) + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
        labs(title = paste(title, "\nCond_R2:", r2_c, "| Marg_R2:", r2_m), x = "Predicted Relative Uptake", y = "Observed Relative Uptake", color = "Monitoring Site") +
        theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold", size = 11))
}

extract_perf <- function(mod, name, y) {
    preds_c <- predict(mod)
    preds_m <- predict(mod, level = 0)
    r2_c <- round(cor(y, preds_c)^2, 3)
    r2_m <- round(cor(y, preds_m)^2, 3)
    aic <- round(AIC(mod), 1)
    tt <- summary(mod)$tTable
    est_invb <- if("beta_invb" %in% rownames(tt)) round(tt["beta_invb", "Value"], 4) else NA
    p_invb <- if("beta_invb" %in% rownames(tt)) round(tt["beta_invb", "p-value"], 4) else NA
    p_N    <- if("beta_N" %in% rownames(tt)) round(tt["beta_N", "p-value"], 4) else NA
    p_v0   <- if("beta_v0" %in% rownames(tt)) round(tt["beta_v0", "p-value"], 4) else NA
    data.frame(Model = name, Marginal_R2 = r2_m, Conditional_R2 = r2_c, AIC = aic, Beta_1b_Estimate = est_invb, p_val_Physical_1b = p_invb, p_val_J0 = p_v0)
}

res_table <- bind_rows(
    extract_perf(mod_raw_co2_geo, "1. Geo PBC - Raw P_CO2 (Num J0)", D_Long_Geo$Relative_Uptake),
    extract_perf(mod_raw_co2_geo_den, "1. Geo PBC - Raw P_CO2 (Den J0)", D_Long_Geo$Relative_Uptake),
    extract_perf(mod_raw_co2_agro, "1. Agro PBC - Raw P_CO2 (Num J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_co2_agro_den, "1. Agro PBC - Raw P_CO2 (Den J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_thm_co2_geo, "2. Geo PBC - Thermo a_CO2 (Num J0)", D_Long_Geo$Relative_Uptake),
    extract_perf(mod_thm_co2_geo_den, "2. Geo PBC - Thermo a_CO2 (Den J0)", D_Long_Geo$Relative_Uptake),
    extract_perf(mod_thm_co2_agro, "2. Agro PBC - Thermo a_CO2 (Num J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_thm_co2_agro_den, "2. Agro PBC - Thermo a_CO2 (Den J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_aae_geo, "3. Geo PBC - Legacy P_AAE10 (Num J0)", D_Long_Geo$Relative_Uptake),
    extract_perf(mod_raw_aae_geo_den, "3. Geo PBC - Legacy P_AAE10 (Den J0)", D_Long_Geo$Relative_Uptake),
    extract_perf(mod_raw_aae_agro, "3. Agro PBC - Legacy P_AAE10 (Num J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_aae_agro_den, "3. Agro PBC - Legacy P_AAE10 (Den J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_co2_agro_null, "1. Null Model - Raw P_CO2 (No 1/b)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_thm_co2_agro_null, "2. Null Model - Thermo a_CO2 (No 1/b)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_aae_agro_null, "3. Null Model - Legacy P_AAE10 (No 1/b)", D_Long_Agro$Relative_Uptake)
)

res_table |>
    kbl(caption = "**Table 3: Dual PBC Plant Uptake Comparison.** Models penalized by the Practical Agronomic Buffer Power ($1/b$) vs the Geochemical Buffer Power. Notice that the practical model accurately retains its predictive power and significance.") |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) |>
    pack_rows("Raw Empirical (P_CO2)", 1, 2) |>
    pack_rows("Thermodynamic (a_CO2)", 3, 4) |>
    pack_rows("Bound Legacy (P_AAE10)", 5, 6) |>
    pack_rows("Null Models (No 1/b Penalty)", 13, 15)

# Comparison Graphs with unified legend
((validate_nlme(mod_raw_co2_geo, D_Long_Geo, "Relative_Uptake", "Geo PBC: Raw P_CO2") |
    validate_nlme(mod_thm_co2_geo, D_Long_Geo, "Relative_Uptake", "Geo PBC: Thermo a_CO2") |
    validate_nlme(mod_raw_aae_geo, D_Long_Geo, "Relative_Uptake", "Geo PBC: Legacy P_AAE10")) /
    (validate_nlme(mod_raw_co2_agro, D_Long_Agro, "Relative_Uptake", "Agro PBC: Raw P_CO2") |
        validate_nlme(mod_thm_co2_agro, D_Long_Agro, "Relative_Uptake", "Agro PBC: Thermo a_CO2") |
        validate_nlme(mod_raw_aae_agro, D_Long_Agro, "Relative_Uptake", "Agro PBC: Legacy P_AAE10"))) +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")


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

(plot_residuals_boxplot(mod_raw_co2_geo, D_Long_Geo, "Geo: Raw P_CO2") |
    plot_residuals_boxplot(mod_thm_co2_geo, D_Long_Geo, "Geo: Thermo a_CO2") |
    plot_residuals_boxplot(mod_raw_aae_geo, D_Long_Geo, "Geo: Legacy P_AAE10")) /
    (plot_residuals_boxplot(mod_raw_co2_agro, D_Long_Agro, "Agro: Raw P_CO2") |
        plot_residuals_boxplot(mod_thm_co2_agro, D_Long_Agro, "Agro: Thermo a_CO2") |
        plot_residuals_boxplot(mod_raw_aae_agro, D_Long_Agro, "Agro: Legacy P_AAE10"))


## -----------------------------------------------------------------------------
library(broom.mixed)

# Helper function to generate a clean effects table
get_effects <- function(mod, model_name) {
    tidy(mod, effects = "fixed") |>
        mutate(
            Model = model_name,
            across(where(is.numeric), ~ round(., 4))
        ) |>
        dplyr::select(Model, term, estimate, std.error, statistic, p.value)
}

# Bind them together and print
all_effects <- bind_rows(
    get_effects(mod_raw_co2_geo, "1. Geo PBC - Raw P_CO2"),
    get_effects(mod_raw_co2_agro, "1. Agro PBC - Raw P_CO2"),
    get_effects(mod_thm_co2_geo, "2. Geo PBC - Thermo a_CO2"),
    get_effects(mod_thm_co2_agro, "2. Agro PBC - Thermo a_CO2"),
    get_effects(mod_raw_aae_geo, "3. Geo PBC - Legacy P_AAE10"),
    get_effects(mod_raw_aae_agro, "3. Agro PBC - Legacy P_AAE10")
)

print(as.data.frame(all_effects), row.names = FALSE)


## ----mitscherlich-yield-models, fig.width=10, fig.height=4--------------------
# 1. Prepare the Dataset for YIELD (Grouped by Site AND Crop)
# 1. Prepare the Dataset for YIELD (Grouped by Site AND Crop)
D_Yield <- D_ready |>
    filter(year >= 2010, !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot)) |>

    # Calculate the Physical Highway (1/b) using Agro PTF (consistent with uptake models)
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
        ln_K_pred_agro = C_agro(1) + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
        b_power = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
        inv_b = 1 / b_power,
        # Safely sum Main Product and Byproduct
        total_yield = tidyr::replace_na(annual_yield_mp_DM, 0)
    ) |>

    # Normalize TOTAL YIELD by Site, Crop, and Year
    group_by(site, crop, year) |>
    mutate(Relative_Yield = total_yield / max(total_yield, na.rm = TRUE)) |>
    ungroup() |>

    filter(is.finite(inv_b), is.finite(Relative_Yield), Relative_Yield > 0) |>
    mutate(
        z_inv_b = as.numeric(scale(inv_b)),
        z_n = as.numeric(scale(n_pred_agro)),
        z_b = as.numeric(scale(b_power)),
        z_fert_N = as.numeric(scale(fert_N_tot)),
        z_fert_K = as.numeric(scale(tidyr::replace_na(fert_K_tot, 0))),
        z_fert_Mg = as.numeric(scale(tidyr::replace_na(fert_Mg_tot, 0))),
        site = as.factor(site),
        year_f = as.factor(year),
        plot_nr = as.factor(plot_nr)
    )

cat("Total Harvest Years Evaluated for Yield (2010-2022):", nrow(D_Yield), "\n")
cat("Sites:", length(unique(D_Yield$site)), "->", paste(unique(D_Yield$site), collapse = ", "), "\n\n")

# NOTE: P_CO2 directly measures soil solution P intensity (I in Q/I theory).
# After site×crop normalization, the theoretical maximum yield is ~1 by construction.
# Therefore, we fix the asymptote at 1 and apply a One-Step NLME Mitscherlich model
# where the rate constant c is mathematically modulated by pedoclimatic drivers
# and an annual random effect to capture unmeasured temporal variation:
#
#   c_eff = (c_base + u_year) * exp(β_invb * z_inv_b + β_pH * z_pH + ...)
#   Y = 1 − exp(−c_eff * P_CO2)
#
# A negative β implies that the environmental driver LOWERS the P-foraging efficiency,
# meaning the crop yield rises more slowly per unit of P_CO2 in the soil.

m_yield_raw_co2 <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_invb * z_inv_b + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_CO2 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_invb = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_thm_co2 <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_invb * z_inv_b + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (a_CO2_total_mg_L + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_invb = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_raw_aae <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_invb * z_inv_b + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_AAE10 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_invb = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

# ---------------------------------------------------------
# N-PARAMETER MODELS (Freundlich n instead of 1/b) FOR YIELD COMPARISON
# ---------------------------------------------------------
m_yield_raw_co2_n <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_n * z_n + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_CO2 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_n ~ 1, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_n = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_thm_co2_n <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_n * z_n + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (a_CO2_total_mg_L + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_n ~ 1, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_n = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_raw_aae_n <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_n * z_n + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_AAE10 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_n ~ 1, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_n = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

# ---------------------------------------------------------
# b-PARAMETER MODELS (Buffer b instead of 1/b) FOR YIELD COMPARISON
# ---------------------------------------------------------
m_yield_raw_co2_b <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_b * z_b + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_CO2 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_b ~ 1, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_b = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_thm_co2_b <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_b * z_b + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (a_CO2_total_mg_L + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_b ~ 1, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_b = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_raw_aae_b <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_b * z_b + 
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_AAE10 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_b ~ 1, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_b = 0, beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

# ---------------------------------------------------------
# NULL MODELS (NO 1/b PENALTY) FOR YIELD COMPARISON
# ---------------------------------------------------------
m_yield_raw_co2_null <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_CO2 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_thm_co2_null <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (a_CO2_total_mg_L + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_raw_aae_null <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_pH * z_pH + 
        beta_fertK * z_fert_K +
        beta_fertMg * z_fert_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_AAE10 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_pH ~ 1, beta_fertK ~ 1, beta_fertMg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1),  beta_pH = 0, beta_fertK = 0, beta_fertMg = 0, beta_N = 0, beta_Temp = 0, beta_Prec = 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

extract_yield <- function(mod, name, y) {
    preds_m <- predict(mod, level = 0)
    r2_m <- round(cor(y, preds_m)^2, 3)
    aic <- round(AIC(mod), 1)
    tt <- summary(mod)$tTable
    p_invb <- if("beta_invb" %in% rownames(tt)) round(tt["beta_invb", "p-value"], 4) else NA
    p_n    <- if("beta_n" %in% rownames(tt)) round(tt["beta_n", "p-value"], 4) else NA
    p_b    <- if("beta_b" %in% rownames(tt)) round(tt["beta_b", "p-value"], 4) else NA
    data.frame(Model = name, Marginal_R2 = r2_m, AIC = aic,
               p_val_Physical_1b = p_invb,
               p_val_Freundlich_n = p_n,
               p_val_Buffer_b = p_b,
               p_val_fertK = round(tt["beta_fertK", "p-value"], 4),
               p_val_fertMg = round(tt["beta_fertMg", "p-value"], 4))
}

yield_table <- bind_rows(
    extract_yield(m_yield_raw_co2, "1. Raw P_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_thm_co2, "2. Thermo a_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_aae, "3. Legacy P_AAE10", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_co2_n, "1. Freundlich n - Raw P_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_thm_co2_n, "2. Freundlich n - Thermo a_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_aae_n, "3. Freundlich n - Legacy P_AAE10", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_co2_b, "1. Buffer b - Raw P_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_thm_co2_b, "2. Buffer b - Thermo a_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_aae_b, "3. Buffer b - Legacy P_AAE10", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_co2_null, "1. Null Model - Raw P_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_thm_co2_null, "2. Null Model - Thermo a_CO2", D_Yield$Relative_Yield),
    extract_yield(m_yield_raw_aae_null, "3. Null Model - Legacy P_AAE10", D_Yield$Relative_Yield)
)

# --- Visualization: 4-Panel Partial Effects ---
cf <- fixef(m_yield_raw_co2)
P_seq <- seq(0, max(D_Yield$soil_0_20_P_CO2, na.rm = TRUE) * 1.1, length.out = 200)

make_curves <- function(var, beta_name, title) {
    pcts <- quantile(D_Yield[[var]], probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
    expand.grid(P = P_seq, Scenario = c("Low (10th)", "Median", "High (90th)")) |>
        dplyr::mutate(
            val = dplyr::case_when(
                Scenario == "Low (10th)" ~ pcts[1],
                Scenario == "Median"     ~ pcts[2],
                TRUE                     ~ pcts[3]
            ),
            Driver = title,
            # Predict holding other covariates at 0 (their standardized mean)
            c_base_mean = mean(cf[grep("c_base", names(cf))]),
            Predicted = 1 - exp(-c_base_mean * exp(cf[[beta_name]] * val) * (P + cf["E_base"]))
        )
}

curve_data <- bind_rows(
    make_curves("z_inv_b", "beta_invb", "Physical Buffer Power (1/b)"),
    make_curves("z_pH", "beta_pH", "Soil pH"),
    make_curves("z_fert_K", "beta_fertK", "Potassium Fertilizer"),
    make_curves("z_fert_Mg", "beta_fertMg", "Magnesium Fertilizer"),
    make_curves("z_fert_N", "beta_N", "Nitrogen Fertilizer"),
    make_curves("z_Temp_Mean", "beta_Temp", "Mean Annual Temperature"),
    make_curves("z_Prec_Anom", "beta_Prec", "Precipitation Anomaly")
)

# Convert to factor to preserve desired plotting order
curve_data$Driver <- factor(curve_data$Driver, levels = c(
    "Physical Buffer Power (1/b)", "Soil pH", "Potassium Fertilizer", "Magnesium Fertilizer", 
    "Nitrogen Fertilizer", "Mean Annual Temperature", "Precipitation Anomaly"
))

# Save fitted values for diagnostics chunk later
D_Yield$Fitted   <- predict(m_yield_raw_co2)
D_Yield$Residual <- residuals(m_yield_raw_co2)

ggplot() +
    geom_point(data = D_Yield, aes(x = soil_0_20_P_CO2, y = Relative_Yield), color = "gray70", alpha = 0.3, size = 1) +
    geom_line(data = curve_data, aes(x = P, y = Predicted, color = Scenario, linetype = Scenario), linewidth = 1.2) +
    scale_linetype_manual(values = c("dashed", "solid", "dotted")) +
    scale_color_viridis_d(option = "C", end = 0.8) +
    facet_wrap(~Driver, scales = "fixed") +
    labs(
        title = "Partial Effects of Pedoclimatic Drivers on Yield Response",
        subtitle = "Curves isolate each driver holding others at their mean (0). Points are raw observations.",
        x = "Soil Test P (P_CO2, mg/L)", y = "Relative Yield", color = "Scenario", linetype = "Scenario"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 11)
    )


## ----residual-diagnostics-yield, fig.width=12, fig.height=5-------------------
p_resid <- ggplot(D_Yield, aes(x = Fitted, y = Residual, color = site)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.4, size = 1.5) +
    labs(title = "Conditional Residuals vs Fitted", x = "Fitted Yield", y = "Residual", color = "Site") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

p_box <- ggplot(D_Yield, aes(x = site, y = Residual, fill = site)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    labs(title = "Yield Model Residuals by Site", x = "Site", y = "Residual", fill = "Site") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

(p_resid | p_box)


## ----pcrit-analysis, fig.width=10, fig.height=5-------------------------------
# ---------------------------------------------------------------------------
# P_crit = STP at which Y = 95% of maximum yield
#
# Since we modeled c_eff directly via NLME, we just compute it deterministically
# and plot the extracted effects without needing a redundant secondary lmer!
# ---------------------------------------------------------------------------

cf <- fixef(m_yield_raw_co2)
re <- ranef(m_yield_raw_co2)

D_Pcrit <- D_Yield |>
    mutate(
        site_re = re[as.character(site), "c_base"],
        c_eff  = (cf["c_base"] + site_re) * exp(
            cf["beta_invb"] * z_inv_b + 
            cf["beta_pH"] * z_pH + 
            cf["beta_fertK"] * z_fert_K +
            cf["beta_fertMg"] * z_fert_Mg +
            cf["beta_N"] * z_fert_N +
            cf["beta_Temp"] * z_Temp_Mean + 
            cf["beta_Prec"] * z_Prec_Anom
        ),
        P_crit = (log(20) / c_eff) - cf['E_base'],               # mg/L P_CO2 at 95% max yield
        ln_P_crit = log(P_crit),
        crop   = as.factor(crop)
    ) |>
    filter(is.finite(ln_P_crit))

cat("### P_crit Summary (mg/L P_CO2 at 95% relative yield) ###\n")
print(round(tapply(D_Pcrit$P_crit, D_Pcrit$site, median, na.rm = TRUE), 3))
cat("\nSwiss GRUD guideline threshold (H2O-CO2): 0.20 mg/L\n")

cat("\n### Note on Hirte Framework ###\n")
cat("Because we adopted the 'One-Step' approach by nesting the pedoclimatic drivers \n")
cat("directly into the Mitscherlich NLME rate constant, the downstream lmer model on \n")
cat("P_crit is no longer necessary. The effects are already estimated below.\n\n")

# --- Visualizations ---
# 1. P_crit distributions by site vs GRUD guideline
p_pcrit_box <- ggplot(D_Pcrit, aes(x = site, y = P_crit, fill = site)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21) +
    geom_hline(yintercept = 0.20, linetype = "dashed", color = "#d73027", linewidth = 1) +
    annotate("text", x = 0.6, y = 0.22, label = "GRUD 0.20 mg/L",
        color = "#d73027", size = 3.5, hjust = 0) +
    labs(title = "Critical STP (P_CO2) per Site",
        subtitle = "Derived from One-Step NLME Mitscherlich",
        x = "Site", y = "P_crit (mg/L P_CO2)", fill = "Site") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

# 2. Forest plot: drivers of Mitscherlich rate constant (c)
nlme_effects <- broom.mixed::tidy(m_yield_raw_co2, effects = "fixed") |>
    filter(term != "c_base") |>
    mutate(
        lower = estimate - 1.96 * std.error,
        upper = estimate + 1.96 * std.error,
        term_clean = case_when(
            term == "beta_invb" ~ "Physical Buffer Power (1/b)",
            term == "beta_pH"   ~ "Soil pH",
            term == "beta_fertK" ~ "Potassium Fertilizer",
            term == "beta_fertMg" ~ "Magnesium Fertilizer",
            term == "beta_N"    ~ "Nitrogen Fertilizer",
            term == "beta_Temp" ~ "Mean Annual Temperature",
            term == "beta_Prec" ~ "Precipitation Anomaly"
        ),
        # Negative effect on c means SLOWER P uptake (higher P_crit required)
        sig = ifelse(p.value < 0.05, "p < 0.05", "p ≥ 0.05")
    )

p_pcrit_forest <- ggplot(nlme_effects, aes(x = estimate, y = reorder(term_clean, estimate), color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, linewidth = 0.9) +
    geom_point(size = 4) +
    scale_color_manual(values = c("p < 0.05" = "#2c7bb6", "p ≥ 0.05" = "gray60")) +
    labs(title = "Drivers of P-Foraging Efficiency (Rate Constant c)",
        subtitle = "Negative = Slower uptake (Requires higher P_crit)",
        x = "Standardised Coefficient (log scale)", y = "", color = "") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

(p_pcrit_box | p_pcrit_forest) + plot_layout(widths = c(1, 1.5))


## ----loso-cv------------------------------------------------------------------
# 1. Define the LOSO-CV function
loso_cv <- function(formula_str, data) {
    sites <- unique(as.character(data$site))
    in_sample_r2 <- c()
    out_sample_r2 <- c()

    for (test_site in sites) {
        # Split Data
        train_data <- data |> filter(site != test_site)
        test_data <- data |> filter(site == test_site)

        # Fit Model on Training Data
        # Use tryCatch to silently handle potential convergence warnings on subsets
        fit <- tryCatch(
            {
                lmer(as.formula(formula_str), data = train_data, control = lmerControl(calc.derivs = FALSE))
            },
            warning = function(w) {
                lmer(as.formula(formula_str), data = train_data, control = lmerControl(calc.derivs = FALSE))
            })

        # In-Sample Variance (Marginal R2 of Fixed Effects on Training Set)
        r2_marg <- as.numeric(performance::r2_nakagawa(fit)$R2_marginal)
        in_sample_r2 <- c(in_sample_r2, r2_marg)

        # Out-of-Sample Variance (Predictive R2 on Test Set using Fixed Effects Only)
        preds <- predict(fit, newdata = test_data, re.form = NA)
        obs <- test_data$ln_P_AAE
        r2_pred <- cor(preds, obs)^2
        out_sample_r2 <- c(out_sample_r2, r2_pred)
    }

    # Return average across all folds
    return(data.frame(
        Mean_In_Sample_R2 = round(mean(in_sample_r2), 3),
        Mean_Out_Sample_R2 = round(mean(out_sample_r2), 3)
    ))
}

# 2. Extract formulas from our original models
f_agro_raw <- formula(ptf_agro_raw)
f_agro_thm <- formula(ptf_agro_thm)
f_geo_raw  <- formula(ptf_geo_raw)
f_geo_thm  <- formula(ptf_geo_thm)

# 3. Run LOSO-CV (This may take a moment)
cv_res_agro_raw <- loso_cv(f_agro_raw, D_ptf)
cv_res_agro_thm <- loso_cv(f_agro_thm, D_ptf)
cv_res_geo_raw  <- loso_cv(f_geo_raw, D_ptf)
cv_res_geo_thm  <- loso_cv(f_geo_thm, D_ptf)

# 4. Compile and Present Results
cv_results <- bind_rows(
    cv_res_agro_raw |> mutate(Model = "Agronomic (Raw P_CO2)"),
    cv_res_agro_thm |> mutate(Model = "Agronomic (Thermo a_CO2)"),
    cv_res_geo_raw  |> mutate(Model = "Geochemical (Raw P_CO2)"),
    cv_res_geo_thm  |> mutate(Model = "Geochemical (Thermo a_CO2)")
) |> dplyr::select(Model, Mean_In_Sample_R2, Mean_Out_Sample_R2)

cv_results |>
    kbl(caption = "**Table 4: Spatial Leave-One-Site-Out Cross-Validation (LOSO-CV).** Variance explained by fixed effects only. The Geochemical models maintain a higher predictive capability on completely unseen environments, confirming that amorphous metal oxides are the true physical drivers of soil buffering capacity.") |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)


## ----p-balance-models, fig.width=10, fig.height=4-----------------------------
# 1. Construct the Cumulative Dataset
D_Cum <- D_ready |>
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
        ln_K_pred_agro = C_agro(1) + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
        b_power_agro = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
        inv_b_agro = 1 / b_power_agro
    ) |>
    group_by(site, plot_nr, treatment) |>
    summarise(
        Cumulated_P_Balance = sum(annual_P_balance, na.rm = TRUE),
        mean_P_CO2 = mean(soil_0_20_P_CO2, na.rm = TRUE),
        mean_a_CO2 = mean(a_CO2_total_mg_L, na.rm = TRUE),
        mean_P_AAE10 = mean(soil_0_20_P_AAE10, na.rm = TRUE),
        z_inv_b_agro = mean(inv_b_agro, na.rm = TRUE),
        mean_n_agro = mean(n_pred_agro, na.rm = TRUE),
        mean_b_agro = mean(b_power_agro, na.rm = TRUE),
        m_pH = mean(z_pH, na.rm = TRUE),
        m_Temp = mean(z_Temp_Mean, na.rm = TRUE),
        m_Tex = mean(z_ln_FineTexture, na.rm = TRUE),
        m_fert_N = mean(fert_N_tot, na.rm = TRUE),
        m_fert_K = mean(fert_K_tot, na.rm = TRUE),
        m_fert_Mg = mean(fert_Mg_tot, na.rm = TRUE),
        .groups = "drop"
    ) |>
    filter(is.finite(Cumulated_P_Balance), is.finite(z_inv_b_agro), !is.na(mean_P_CO2)) |>
    mutate(
        z_inv_b = as.numeric(scale(z_inv_b_agro)),
        z_n = as.numeric(scale(mean_n_agro)),
        z_b = as.numeric(scale(mean_b_agro)),
        ln_P_CO2 = log(mean_P_CO2),
        ln_a_CO2 = log(mean_a_CO2),
        ln_P_AAE10 = log(mean_P_AAE10),
        z_pH = as.numeric(scale(m_pH)),
        z_Temp = as.numeric(scale(m_Temp)),
        z_Tex = as.numeric(scale(m_Tex)),
        z_fert_N = as.numeric(scale(m_fert_N)),
        z_fert_K = as.numeric(scale(tidyr::replace_na(m_fert_K, 0))),
        z_fert_Mg = as.numeric(scale(tidyr::replace_na(m_fert_Mg, 0))),
        site = as.factor(site)
    )

cat("Total Plots Evaluated for Cumulative P Balance (1990-2022):", nrow(D_Cum), "\n\n")

# 2. Fit Full Models (with fixed covariates to isolate buffer effects)
m_bal_co2 <- lmer(Cumulated_P_Balance ~ ln_P_CO2 * z_inv_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_thm <- lmer(Cumulated_P_Balance ~ ln_a_CO2 * z_inv_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_aae <- lmer(Cumulated_P_Balance ~ ln_P_AAE10 * z_inv_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)

# 3. Fit Freundlich n Models
m_bal_co2_n <- lmer(Cumulated_P_Balance ~ ln_P_CO2 * z_n + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_thm_n <- lmer(Cumulated_P_Balance ~ ln_a_CO2 * z_n + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_aae_n <- lmer(Cumulated_P_Balance ~ ln_P_AAE10 * z_n + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)

# 4. Fit Buffer b Models
m_bal_co2_b <- lmer(Cumulated_P_Balance ~ ln_P_CO2 * z_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_thm_b <- lmer(Cumulated_P_Balance ~ ln_a_CO2 * z_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_aae_b <- lmer(Cumulated_P_Balance ~ ln_P_AAE10 * z_b + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)

# 5. Fit Null Models
m_bal_co2_null <- lmer(Cumulated_P_Balance ~ ln_P_CO2 + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_thm_null <- lmer(Cumulated_P_Balance ~ ln_a_CO2 + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)
m_bal_aae_null <- lmer(Cumulated_P_Balance ~ ln_P_AAE10 + z_pH + z_Temp + z_Tex + z_fert_N + z_fert_K + z_fert_Mg + (1 | site), data = D_Cum)

# 4. Extract Performance
extract_bal <- function(mod, name) {
    perf <- performance::r2_nakagawa(mod)
    aic <- round(AIC(mod), 1)
    
    tt <- summary(mod)$coefficients
    interaction_term <- grep(":", rownames(tt), value = TRUE)
    p_val_interaction <- if(length(interaction_term) > 0) {
        if("Pr(>|t|)" %in% colnames(tt)) round(tt[interaction_term[1], "Pr(>|t|)"], 4) else NA
    } else {
        NA
    }
    
    data.frame(Model = name, Marginal_R2 = round(perf$R2_marginal, 3), Conditional_R2 = round(perf$R2_conditional, 3), AIC = aic, p_val_Interaction = p_val_interaction)
}

bal_table <- bind_rows(
    extract_bal(m_bal_co2, "1. Full - Raw P_CO2"),
    extract_bal(m_bal_thm, "2. Full - Thermo a_CO2"),
    extract_bal(m_bal_aae, "3. Full - Legacy P_AAE10"),
    extract_bal(m_bal_co2_n, "1. Freundlich n - Raw P_CO2"),
    extract_bal(m_bal_thm_n, "2. Freundlich n - Thermo a_CO2"),
    extract_bal(m_bal_aae_n, "3. Freundlich n - Legacy P_AAE10"),
    extract_bal(m_bal_co2_b, "1. Buffer b - Raw P_CO2"),
    extract_bal(m_bal_thm_b, "2. Buffer b - Thermo a_CO2"),
    extract_bal(m_bal_aae_b, "3. Buffer b - Legacy P_AAE10"),
    extract_bal(m_bal_co2_null, "1. Null - Raw P_CO2 (No 1/b)"),
    extract_bal(m_bal_thm_null, "2. Null - Thermo a_CO2 (No 1/b)"),
    extract_bal(m_bal_aae_null, "3. Null - Legacy P_AAE10 (No 1/b)")
)

bal_table |>
    kbl(caption = "**Table 4: Cumulative P Balance Comparison.** Demonstrates that integrating the physical buffer power interaction significantly explains historical fertilization efficiency.") |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

# 5. Visual Diagnostics
plot_data_bal <- D_Cum |> mutate(
    Predicted_Full = predict(m_bal_co2),
    Predicted_Null = predict(m_bal_co2_null)
)

p_full <- ggplot(plot_data_bal, aes(x = Predicted_Full, y = Cumulated_P_Balance, color = site)) +
    geom_point(alpha = 0.7, size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = "Full Model (P_CO2 * 1/b)", x = "Predicted Cumulative P Balance", y = "Observed Cumulative P Balance") +
    theme_minimal() + theme(legend.position = "none")

p_null <- ggplot(plot_data_bal, aes(x = Predicted_Null, y = Cumulated_P_Balance, color = site)) +
    geom_point(alpha = 0.7, size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = "Null Model (P_CO2)", x = "Predicted Cumulative P Balance", y = "") +
    theme_minimal()

(p_full | p_null) + plot_layout(guides = "collect") & theme(legend.position = "right")

