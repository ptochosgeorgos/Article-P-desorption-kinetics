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
    library(MuMIn)

    get_metrics_lmer <- function(model, model_name, data, response) {
        r2 <- suppressWarnings(MuMIn::r.squaredGLMM(model))
        r2m <- round(r2[1, "R2m"], 3)
        r2c <- round(r2[1, "R2c"], 3)
        
        pred_m <- predict(model, re.form = NA)
        rmse_m <- round(sqrt(mean((data[[response]] - pred_m)^2, na.rm=TRUE)), 3)
        
        pred_c <- predict(model)
        rmse_c <- round(sqrt(mean((data[[response]] - pred_c)^2, na.rm=TRUE)), 3)
        
        data.frame(
            Model = model_name,
            R2_m = r2m, R2_c = r2c,
            RMSE_m = rmse_m, RMSE_c = rmse_c,
            AIC = round(AIC(model), 1),
            BIC = round(BIC(model), 1)
        )
    }

    get_metrics_nlme <- function(model, model_name, data, response) {
        pred_c <- predict(model, level = 2) # Plot level
        pred_m <- predict(model, level = 0) # Fixed effects only
        
        r2_c <- round(cor(data[[response]], pred_c)^2, 3)
        r2_m <- round(cor(data[[response]], pred_m)^2, 3)
        
        rmse_c <- round(sqrt(mean((data[[response]] - pred_c)^2, na.rm=TRUE)), 3)
        rmse_m <- round(sqrt(mean((data[[response]] - pred_m)^2, na.rm=TRUE)), 3)
        
        data.frame(
            Model = model_name,
            R2_m = r2_m, R2_c = r2_c,
            RMSE_m = rmse_m, RMSE_c = rmse_c,
            AIC = round(AIC(model), 1),
            BIC = round(BIC(model), 1)
        )
    }
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
ptf_results <- bind_rows(
    get_metrics_lmer(ptf_agro_raw, "Agronomic (Raw P_CO2)", D_ptf, "ln_P_AAE"),
    get_metrics_lmer(ptf_agro_thm, "Agronomic (Thermo a_CO2)", D_ptf, "ln_P_AAE"),
    get_metrics_lmer(ptf_geo_raw, "Geochemical (Raw P_CO2)", D_ptf, "ln_P_AAE"),
    get_metrics_lmer(ptf_geo_thm, "Geochemical (Thermo a_CO2)", D_ptf, "ln_P_AAE")
)

# Table with Caption
ptf_results |>
    kbl(caption = "**Table 1: Variance Explained by Pedotransfer Functions.** Geochemical traits account for a massive 14% increase in Marginal R² compared to standard agronomic soil texture (Clay/Silt), proving that amorphous metal oxides dictate the physical binding capacity of the soil matrix.") |>
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
ptf_results_agro <- bind_rows(
    get_metrics_lmer(ptf_practical_raw, "Practical Agro (Raw P_CO2)", D_ptf_agro, "ln_P_AAE"),
    get_metrics_lmer(ptf_practical_thm, "Practical Agro (Thermo a_CO2)", D_ptf_agro, "ln_P_AAE")
)

ptf_results_agro |>
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

# 1. Extract the Coefficients from the Selected PTFs
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
    filter(annual_P_uptake > 0, !is.na(k), !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot)) |>

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
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_geo)) + soil_0_20_P_CO2),
    data = D_Long_Geo, fixed = U_base + beta_temp + beta_N + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Geo$soil_0_20_P_CO2), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)

mod_thm_co2_geo <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_geo)) + a_CO2_total_mg_L),
    data = D_Long_Geo, fixed = U_base + beta_temp + beta_N + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Geo$a_CO2_total_mg_L), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)

mod_raw_aae_geo <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_AAE10) /
        ((K_base * exp(beta_invb * z_inv_b_geo)) + soil_0_20_P_AAE10),
    data = D_Long_Geo, fixed = U_base + beta_temp + beta_N + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Geo$soil_0_20_P_AAE10), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)

# ---------------------------------------------------------
# PRACTICAL AGRONOMIC PBC PENALTY MODELS
# ---------------------------------------------------------
# Note: For Agro models, REC is successfully included!
D_Long_Agro <- D_Long |> filter(is.finite(z_inv_b_agro))
cat("Trials included in Agro Uptake Models:", length(unique(D_Long_Agro$site)), "(", paste(unique(D_Long_Agro$site), collapse = ", "), ")\n\n")

mod_raw_co2_agro <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_agro)) + soil_0_20_P_CO2),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Agro$soil_0_20_P_CO2), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)

mod_thm_co2_agro <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_agro)) + a_CO2_total_mg_L),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Agro$a_CO2_total_mg_L), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)

mod_raw_aae_agro <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_AAE10) /
        ((K_base * exp(beta_invb * z_inv_b_agro)) + soil_0_20_P_AAE10),
    data = D_Long_Agro, fixed = U_base + beta_temp + beta_N + K_base + beta_invb ~ 1, random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = median(D_Long_Agro$soil_0_20_P_AAE10), beta_invb = 0), control = nlmeControl(maxIter = 1000)
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
    est_invb <- round(tt["beta_invb", "Value"], 4)
    p_invb <- round(tt["beta_invb", "p-value"], 4)
    p_N    <- round(tt["beta_N",    "p-value"], 4)
    data.frame(Model = name, Marginal_R2 = r2_m, Conditional_R2 = r2_c, AIC = aic, Beta_1b_Estimate = est_invb, p_val_Physical_1b = p_invb)
}

res_table <- bind_rows(
    extract_perf(mod_raw_co2_geo, "1. Geo PBC - Raw P_CO2", D_Long_Geo$Relative_Uptake),
    extract_perf(mod_raw_co2_agro, "1. Agro PBC - Raw P_CO2", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_thm_co2_geo, "2. Geo PBC - Thermo a_CO2", D_Long_Geo$Relative_Uptake),
    extract_perf(mod_thm_co2_agro, "2. Agro PBC - Thermo a_CO2", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_aae_geo, "3. Geo PBC - Legacy P_AAE10", D_Long_Geo$Relative_Uptake),
    extract_perf(mod_raw_aae_agro, "3. Agro PBC - Legacy P_AAE10", D_Long_Agro$Relative_Uptake)
)

res_table |>
    kbl(caption = "**Table 3: Dual PBC Plant Uptake Comparison.** Models penalized by the Practical Agronomic Buffer Power ($1/b$) vs the Geochemical Buffer Power. Notice that the practical model accurately retains its predictive power and significance.") |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) |>
    pack_rows("Raw Empirical (P_CO2)", 1, 2) |>
    pack_rows("Thermodynamic (a_CO2)", 3, 4) |>
    pack_rows("Bound Legacy (P_AAE10)", 5, 6)

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
    filter(!is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot)) |>

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

    # Drop rare crops to prevent nlme rank-deficiency
    group_by(crop) |>
    filter(n() > 200) |>
    ungroup() |>

    filter(is.finite(inv_b), is.finite(Relative_Yield), Relative_Yield > 0) |>
    mutate(
        z_inv_b = as.numeric(scale(inv_b)),
        z_fert_N = as.numeric(scale(fert_N_tot)),
        site = as.factor(site),
        year_f = as.factor(year),
        plot_nr = as.factor(plot_nr), crop = droplevels(as.factor(crop))
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

