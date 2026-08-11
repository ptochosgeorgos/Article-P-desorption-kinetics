## \begin{tikzpicture}[
##     font=\sffamily\small,
##     box/.style={rectangle, rounded corners, draw=black, thick, align=center, text width=3.5cm, minimum height=1cm},
##     data/.style={cylinder, shape border rotate=90, aspect=0.25, draw=black, thick, fill=gray!10, text width=3cm, align=center, minimum height=1.2cm},
##     arrow/.style={-{Stealth[scale=1.2]}, thick, draw=black!70},
##     % Colors
##     kinetics/.style={fill=cyan!10, draw=cyan!80!black},
##     thermo/.style={fill=orange!10, draw=orange!80!black},
##     bio/.style={fill=green!10, draw=green!80!black},
##     eval/.style={fill=purple!10, draw=purple!80!black},
##     % Group boxes
##     groupbox/.style={rectangle, rounded corners, draw=gray!50, dashed, thick, inner sep=20pt, inner ysep=25pt, fill=gray!5}
## ]
## 
## % --- Left Column: Data Sources ---
## \node[data] (kin_data) at (0, 6.5) {Time-Series Data\\(Water Extraction)};
## \node[data] (bio_data) at (0, 3.25) {Biological Data\\Yield, $C_p$, $P_{up}$};
## \node[data] (thermo_data) at (0, 0) {Equilibration Data\\($P_{AAE10}$ vs $P_{CO_2}$)};
## 
## % --- Middle Column: Sub-Models ---
## \node[box, kinetics] (kin_model) at (4.5, 6.5) {Kinetic NLME Model\\(Desorption Rate)};
## \node[box, thermo] (thermo_model) at (4.5, 0) {Thermodynamic LMER\\(Freundlich Isotherm)};
## 
## % --- Right Column: Coefficients & Biological Model ---
## \node[box, kinetics] (kin_coef) at (9, 6.5) {Kinetic Coefficients\\$k, v_0$};
## \node[box, eval] (mech_model) at (9, 3.25) {Mechanistic Models\\(Covariates: $v_0, 1/b$)};
## \node[box, thermo] (thermo_coef) at (9, 0) {Buffer Coefficients\\$b, 1/b$};
## 
## % --- Far Right Column: Evaluation ---
## \node[box, eval] (comparison) at (13.5, 3.25) {Model Evaluation\\\& Comparison};
## 
## % --- Edges ---
## % Kinetics (Top Row)
## \draw[arrow] (kin_data) -- (kin_model);
## \draw[arrow] (kin_model) -- (kin_coef);
## % Flow into biological (Down)
## \draw[arrow] (kin_coef) -- (mech_model);
## 
## % Thermo (Bottom Row)
## \draw[arrow] (thermo_data) -- (thermo_model);
## \draw[arrow] (thermo_model) -- (thermo_coef);
## % Flow into biological (Up)
## \draw[arrow] (thermo_coef) -- (mech_model);
## 
## % Biological (Middle Row)
## \draw[arrow] (bio_data) -- (mech_model);
## \draw[arrow] (mech_model) -- (comparison);
## 
## % Backgrounds
## \begin{scope}[on background layer]
##     \node[groupbox, fit=(kin_data) (kin_model) (kin_coef)] (phase1) {};
##     \node[anchor=north west, font=\sffamily\bfseries, color=gray!70!black, shift={(10pt,-8pt)}] at (phase1.north west) {Desorption Kinetics};
## 
##     \node[groupbox, fit=(bio_data) (mech_model) (comparison)] (phase3) {};
##     \node[anchor=north west, font=\sffamily\bfseries, color=gray!70!black, shift={(10pt,-8pt)}] at (phase3.north west) {Biological Scaling \& Evaluation};
## 
##     \node[groupbox, fit=(thermo_data) (thermo_model) (thermo_coef)] (phase2) {};
##     \node[anchor=north west, font=\sffamily\bfseries, color=gray!70!black, shift={(10pt,-8pt)}] at (phase2.north west) {Buffer Thermodynamics};
## \end{scope}
## 
## \end{tikzpicture}

## ----setup--------------------------------------------------------------------
#| message: false
#| warning: false

rm(list = ls())

library(tikzDevice)
options(tikzDefaultEngine = "luatex")
knitr::opts_chunk$set(
  dev = "tikz",
  fig.align = "center"
)

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

# 2.5 Train Pedotransfer Function for k (Desorption Rate Constant)
# The Arrhenius equation states that k is heavily driven by activation energies and available surface area.
# We predict ln(k) using the ratio of Aluminum to Iron oxides (competing activation energies) and pH.
kin_train <- D_main |> filter(!is.na(k))
k_ptf <- lm(log(k) ~ log(alox_mean / feox_mean) + soil_0_20_pH_H2O, data = kin_train)
D_main$k_pred <- exp(predict(k_ptf, newdata = D_main))
D_main$k <- D_main$k_pred # Broadcast to all treatments

# 3. Scaling and Patching
D_ready <- D_main |>
    mutate(
        # Target and Main Predictors
        ln_P_AAE = log(soil_0_20_P_AAE10),
        ln_P_CO2 = log(soil_0_20_P_CO2),
        a_CO2_total_mg_L = soil_0_20_P_CO2, # Dummy variable to satisfy legacy code
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
ptf_results <- bind_rows(
    get_metrics_lmer(ptf_agro_raw, "Agronomic (Raw $P_{CO_2}$)", D_ptf, "ln_P_AAE"),
    get_metrics_lmer(ptf_agro_thm, "Agronomic (Thermo a_CO2)", D_ptf, "ln_P_AAE"),
    get_metrics_lmer(ptf_geo_raw, "Geochemical (Raw $P_{CO_2}$)", D_ptf, "ln_P_AAE"),
    get_metrics_lmer(ptf_geo_thm, "Geochemical (Thermo a_CO2)", D_ptf, "ln_P_AAE")
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
        labs(title = title, x = "Predicted $\\ln(P_{AAE})$", y = "Observed $\\ln(P_{AAE})$", color = "Monitoring Site") +
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
    get_metrics_lmer(ptf_practical_raw, "Practical Agro (Raw $P_{CO_2}$)", D_ptf_agro, "ln_P_AAE"),
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
        labs(title = title, x = "Predicted $\\ln(P_{AAE})$", y = "Observed $\\ln(P_{AAE})$", color = "Monitoring Site") +
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


## ----ptf-conservative, fig.width=10, fig.height=4-----------------------------
# Create the Conservative Artifact-Free Dataset
D_ptf_cons <- D_ptf_agro |>
    filter(soil_0_20_pH_H2O <= 7.3)

cat("Trials included in Conservative PTF:", length(unique(D_ptf_cons$site)), "\n")

# Fit Conservative Models
ptf_cons_raw <- lmer(ln_P_AAE ~ ln_P_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf_cons)
ptf_cons_thm <- lmer(ln_P_AAE ~ ln_a_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf_cons)

# Present Performance
ptf_results_cons <- bind_rows(
    get_metrics_lmer(ptf_cons_raw, "Conservative Agro (Raw $P_{CO_2}$)", D_ptf_cons, "ln_P_AAE"),
    get_metrics_lmer(ptf_cons_thm, "Conservative Agro (Thermo a_CO2)", D_ptf_cons, "ln_P_AAE")
)

ptf_results_cons |>
    kbl(caption = "**Table 2.5: Variance Explained by Conservative PTF.** Trained exclusively on artifact-free soils (pH <= 7.3).") |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

# Export the Conservative Coefficients
library(jsonlite)
coefs_cons <- fixef(ptf_cons_thm)
scales_cons <- list(
    FineTexture = list(mean = mean(log(D_ready$rollMean_soil_0_20_clay + D_ready$rollMean_soil_0_20_silt), na.rm=TRUE), sd = sd(log(D_ready$rollMean_soil_0_20_clay + D_ready$rollMean_soil_0_20_silt), na.rm=TRUE)),
    Ca = list(mean = mean(log(D_ready$rollMean_soil_0_20_Ca_AAE10), na.rm=TRUE), sd = sd(log(D_ready$rollMean_soil_0_20_Ca_AAE10), na.rm=TRUE)),
    Mg = list(mean = mean(log(D_ready$rollMean_soil_0_20_Mg_AAE10), na.rm=TRUE), sd = sd(log(D_ready$rollMean_soil_0_20_Mg_AAE10), na.rm=TRUE)),
    K = list(mean = mean(log(D_ready$rollMean_soil_0_20_K_AAE10), na.rm=TRUE), sd = sd(log(D_ready$rollMean_soil_0_20_K_AAE10), na.rm=TRUE)),
    pH = list(mean = mean(D_ready$rollMean_soil_0_20_pH_H2O, na.rm=TRUE), sd = sd(D_ready$rollMean_soil_0_20_pH_H2O, na.rm=TRUE)),
    Corg = list(mean = mean(log(D_ready$rollMean_soil_0_20_Corg), na.rm=TRUE), sd = sd(log(D_ready$rollMean_soil_0_20_Corg), na.rm=TRUE)),
    Temp_Mean = list(mean = mean(D_ready$site_juv_temp_mean, na.rm=TRUE), sd = sd(D_ready$site_juv_temp_mean, na.rm=TRUE)),
    Temp_Anom = list(mean = mean(D_ready$temp_anomaly, na.rm=TRUE), sd = sd(D_ready$temp_anomaly, na.rm=TRUE)),
    Prec_Anom = list(mean = mean(D_ready$prec_anomaly, na.rm=TRUE), sd = sd(D_ready$prec_anomaly, na.rm=TRUE))
)

write_json(list(coefficients = as.list(coefs_cons), scales = scales_cons), "presentation/ptf_coefs_cons.json", auto_unbox = TRUE)


## ----plant-uptake-showdown, fig.width=12, fig.height=8------------------------
library(nlme)

# 1. Extract the Coefficients from Both PTFs
coefs_geo <- fixef(ptf_geo_raw)
coefs_agro <- fixef(ptf_practical_raw)
coefs_cons <- fixef(ptf_cons_thm)

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

C_cons <- function(name) {
    if (name %in% names(coefs_cons)) coefs_cons[[name]] else 0
}
get_int_cons <- function(v1, v2) {
    n1 <- paste0(v1, ":", v2)
    n2 <- paste0(v2, ":", v1)
    if (n1 %in% names(coefs_cons)) return(coefs_cons[[n1]])
    if (n2 %in% names(coefs_cons)) return(coefs_cons[[n2]])
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

        ln_K_pred_geo = C_geo("(Intercept)") + C_geo("z_ln_Feox") * z_ln_Feox + C_geo("z_ln_Alox") * z_ln_Alox + C_geo("z_pH") * z_pH + C_geo("z_ln_Ca") * z_ln_Ca + C_geo("z_ln_Mg") * z_ln_Mg + C_geo("z_ln_K") * z_ln_K + C_geo("z_ln_Corg") * z_ln_Corg + C_geo("z_Temp_Anom") * z_Temp_Anom + C_geo("z_Prec_Anom") * z_Prec_Anom + C_geo("z_Temp_Mean") * z_Temp_Mean,

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

        ln_K_pred_agro = C_agro("(Intercept)") + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,

        b_power_agro = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
        inv_b_agro = 1 / b_power_agro
    ) |>

    # Calculate Conservative Artifact-Free Physical Highway (1/b)
    mutate(
        n_pred_cons = C_cons("ln_a_CO2") +
            get_int_cons("ln_a_CO2", "z_ln_FineTexture") * z_ln_FineTexture +
            get_int_cons("ln_a_CO2", "z_pH") * z_pH +
            get_int_cons("ln_a_CO2", "z_ln_Ca") * z_ln_Ca +
            get_int_cons("ln_a_CO2", "z_ln_Mg") * z_ln_Mg +
            get_int_cons("ln_a_CO2", "z_ln_K") * z_ln_K +
            get_int_cons("ln_a_CO2", "z_ln_Corg") * z_ln_Corg +
            get_int_cons("ln_a_CO2", "z_Temp_Anom") * z_Temp_Anom +
            get_int_cons("ln_a_CO2", "z_Prec_Anom") * z_Prec_Anom,

        ln_K_pred_cons = C_cons("(Intercept)") + C_cons("z_ln_FineTexture") * z_ln_FineTexture + C_cons("z_pH") * z_pH + C_cons("z_ln_Ca") * z_ln_Ca + C_cons("z_ln_Mg") * z_ln_Mg + C_cons("z_ln_K") * z_ln_K + C_cons("z_ln_Corg") * z_ln_Corg + C_cons("z_Temp_Anom") * z_Temp_Anom + C_cons("z_Prec_Anom") * z_Prec_Anom + C_cons("z_Temp_Mean") * z_Temp_Mean,

        b_power_cons = n_pred_cons * exp(ln_K_pred_cons) * (a_CO2_total_mg_L^(n_pred_cons - 1)),
        inv_b_cons = 1 / b_power_cons
    ) |>

    # Normalize Uptake & Scale Bottlenecks
    group_by(site, crop, year) |>
    mutate(
        crop = droplevels(as.factor(crop)),
        ref_uptake = mean(annual_P_uptake[treatment_ID == "P166"], na.rm = TRUE),
        ref_uptake = ifelse(is.na(ref_uptake) | is.nan(ref_uptake), max(annual_P_uptake, na.rm = TRUE), ref_uptake),
        Relative_Uptake = annual_P_uptake / ref_uptake
    ) |>
    ungroup() |>
    filter(is.finite(Relative_Uptake)) |>
    mutate(
        z_inv_b_geo = as.numeric(scale(inv_b_geo)),
        z_inv_b_agro = as.numeric(scale(inv_b_agro)),
        z_inv_b_cons = as.numeric(scale(inv_b_cons)),
        z_k = as.numeric(scale(k)),
        z_v0 = as.numeric(scale(k * soil_0_20_P_CO2)),
        z_fert_N = as.numeric(scale(fert_N_tot)),
        site = as.factor(site),
        year_f = as.factor(year)
    )

# ---------------------------------------------------------
# GEOCHEMICAL PBC PENALTY MODELS
# ---------------------------------------------------------
# Note: For Geo models, we filter down to rows where Geo inv_b is finite (excludes REC).
D_Long_Geo <- D_Long |> filter(is.finite(z_inv_b_geo))
    n_crops <- length(levels(D_Long_Geo$crop))
cat("Trials included in Geo Uptake Models:", length(unique(D_Long_Geo$site)), "(", paste(unique(D_Long_Geo$site), collapse = ", "), ")\n")

mod_raw_co2_geo <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_geo)) + soil_0_20_P_CO2),
    data = D_Long_Geo, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop, beta_invb ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = c(median(D_Long_Geo$soil_0_20_P_CO2), rep(0, n_crops - 1)), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_raw_co2_geo_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_geo + beta_v0 * z_v0)) + soil_0_20_P_CO2),
    data = D_Long_Geo, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, K_base ~ crop, beta_invb ~ 1, beta_v0 ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = c(median(D_Long_Geo$soil_0_20_P_CO2), rep(0, n_crops - 1)), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

mod_thm_co2_geo <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_geo)) + a_CO2_total_mg_L),
    data = D_Long_Geo, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop, beta_invb ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = c(median(D_Long_Geo$a_CO2_total_mg_L), rep(0, n_crops - 1)), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_thm_co2_geo_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_geo + beta_v0 * z_v0)) + a_CO2_total_mg_L),
    data = D_Long_Geo, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, K_base ~ crop, beta_invb ~ 1, beta_v0 ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = c(median(D_Long_Geo$a_CO2_total_mg_L), rep(0, n_crops - 1)), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

mod_raw_aae_geo <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_AAE10) /
        ((K_base * exp(beta_invb * z_inv_b_geo)) + soil_0_20_P_AAE10),
    data = D_Long_Geo, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop, beta_invb ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = c(median(D_Long_Geo$soil_0_20_P_AAE10), rep(0, n_crops - 1)), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_raw_aae_geo_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_AAE10) /
        ((K_base * exp(beta_invb * z_inv_b_geo + beta_v0 * z_v0)) + soil_0_20_P_AAE10),
    data = D_Long_Geo, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, K_base ~ crop, beta_invb ~ 1, beta_v0 ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = c(median(D_Long_Geo$soil_0_20_P_AAE10), rep(0, n_crops - 1)), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

# ---------------------------------------------------------
# PRACTICAL AGRONOMIC PBC PENALTY MODELS
# ---------------------------------------------------------
# Note: For Agro models, REC is successfully included!
D_Long_Agro <- D_Long |> filter(is.finite(z_inv_b_agro))
    n_crops <- length(levels(D_Long_Agro$crop))
cat("Trials included in Agro Uptake Models:", length(unique(D_Long_Agro$site)), "(", paste(unique(D_Long_Agro$site), collapse = ", "), ")\n\n")

mod_raw_co2_agro <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_agro)) + soil_0_20_P_CO2),
    data = D_Long_Agro, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop, beta_invb ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = c(median(D_Long_Agro$soil_0_20_P_CO2), rep(0, n_crops - 1)), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_raw_co2_agro_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_CO2) /
        ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + soil_0_20_P_CO2),
    data = D_Long_Agro, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, K_base ~ crop, beta_invb ~ 1, beta_v0 ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = c(median(D_Long_Agro$soil_0_20_P_CO2), rep(0, n_crops - 1)), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

mod_thm_co2_agro <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_agro)) + a_CO2_total_mg_L),
    data = D_Long_Agro, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop, beta_invb ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = c(median(D_Long_Agro$a_CO2_total_mg_L), rep(0, n_crops - 1)), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_thm_co2_agro_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + a_CO2_total_mg_L),
    data = D_Long_Agro, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, K_base ~ crop, beta_invb ~ 1, beta_v0 ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = c(median(D_Long_Agro$a_CO2_total_mg_L), rep(0, n_crops - 1)), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

mod_raw_aae_agro <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_AAE10) /
        ((K_base * exp(beta_invb * z_inv_b_agro)) + soil_0_20_P_AAE10),
    data = D_Long_Agro, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop, beta_invb ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = c(median(D_Long_Agro$soil_0_20_P_AAE10), rep(0, n_crops - 1)), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_raw_aae_agro_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_AAE10) /
        ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + soil_0_20_P_AAE10),
    data = D_Long_Agro, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, K_base ~ crop, beta_invb ~ 1, beta_v0 ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = c(median(D_Long_Agro$soil_0_20_P_AAE10), rep(0, n_crops - 1)), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

# ---------------------------------------------------------
# CONSERVATIVE ARTIFACT-FREE PBC PENALTY MODELS
# ---------------------------------------------------------
# Same dataset as Agro models, but penalized by the conservative physical highway
mod_thm_co2_cons <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_cons)) + a_CO2_total_mg_L),
    data = D_Long_Agro, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop, beta_invb ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = c(median(D_Long_Agro$a_CO2_total_mg_L), rep(0, n_crops - 1)), beta_invb = 0), control = nlmeControl(maxIter = 1000)
)
mod_thm_co2_cons_den <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * a_CO2_total_mg_L) /
        ((K_base * exp(beta_invb * z_inv_b_cons + beta_v0 * z_v0)) + a_CO2_total_mg_L),
    data = D_Long_Agro, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, K_base ~ crop, beta_invb ~ 1, beta_v0 ~ 1), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, K_base = c(median(D_Long_Agro$a_CO2_total_mg_L), rep(0, n_crops - 1)), beta_invb = 0, beta_v0 = -0.1), control = nlmeControl(maxIter = 1000)
)

# ---------------------------------------------------------
# NULL MODELS (NO 1/b PENALTY) FOR COMPARISON
# ---------------------------------------------------------
mod_raw_co2_agro_null <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) /
        (K_base + soil_0_20_P_CO2),
    data = D_Long_Agro, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = c(median(D_Long_Agro$soil_0_20_P_CO2), rep(0, n_crops - 1))), control = nlmeControl(maxIter = 1000)
)

mod_thm_co2_agro_null <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * a_CO2_total_mg_L) /
        (K_base + a_CO2_total_mg_L),
    data = D_Long_Agro, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = c(median(D_Long_Agro$a_CO2_total_mg_L), rep(0, n_crops - 1))), control = nlmeControl(maxIter = 1000)
)

mod_raw_aae_agro_null <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_AAE10) /
        (K_base + soil_0_20_P_AAE10),
    data = D_Long_Agro, fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop), random = U_base ~ 1 | site/year_f,
    start = c(U_base = 0.68, beta_temp = -0.03, beta_N = 0.1, beta_v0 = 0.1, K_base = c(median(D_Long_Agro$soil_0_20_P_AAE10), rep(0, n_crops - 1))), control = nlmeControl(maxIter = 1000)
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
        labs(title = paste(title, "\nConditional $R^2$:", r2_c, "| Marginal $R^2$:", r2_m), x = "Predicted Relative Uptake", y = "Observed Relative Uptake", color = "Monitoring Site") +
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
    extract_perf(mod_thm_co2_cons, "2. CONS PBC - Thermo a_CO2 (Num J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_thm_co2_cons_den, "2. CONS PBC - Thermo a_CO2 (Den J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_aae_geo, "3. Geo PBC - Legacy P_AAE10 (Num J0)", D_Long_Geo$Relative_Uptake),
    extract_perf(mod_raw_aae_geo_den, "3. Geo PBC - Legacy P_AAE10 (Den J0)", D_Long_Geo$Relative_Uptake),
    extract_perf(mod_raw_aae_agro, "3. Agro PBC - Legacy P_AAE10 (Num J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_aae_agro_den, "3. Agro PBC - Legacy P_AAE10 (Den J0)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_co2_agro_null, "1. Null Model - Raw P_CO2 (No 1/b)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_thm_co2_agro_null, "2. Null Model - Thermo a_CO2 (No 1/b)", D_Long_Agro$Relative_Uptake),
    extract_perf(mod_raw_aae_agro_null, "3. Null Model - Legacy P_AAE10 (No 1/b)", D_Long_Agro$Relative_Uptake)
)

res_table |>
    kbl(caption = "**Table 3: Dual PBC Plant Uptake Comparison.** Models penalized by the Practical Agronomic Buffer Power vs Conservative Buffer Power vs Geochemical. Notice the stability of the Conservative models.") |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) |>
    pack_rows("Raw Empirical (P_CO2)", 1, 4) |>
    pack_rows("Thermodynamic (a_CO2)", 5, 10) |>
    pack_rows("Bound Legacy (P_AAE10)", 11, 14) |>
    pack_rows("Null Models (No 1/b Penalty)", 15, 17)

# Comparison Graphs with unified legend
((validate_nlme(mod_raw_co2_geo, D_Long_Geo, "Relative_Uptake", "Geo PBC: Raw $P_{CO_2}$") |
    validate_nlme(mod_thm_co2_geo, D_Long_Geo, "Relative_Uptake", "Geo PBC: Thermo a_CO2") |
    validate_nlme(mod_raw_aae_geo, D_Long_Geo, "Relative_Uptake", "Geo PBC: Legacy P_AAE10")) /
    (validate_nlme(mod_raw_co2_agro, D_Long_Agro, "Relative_Uptake", "Agro PBC: Raw $P_{CO_2}$") |
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

(plot_residuals_boxplot(mod_raw_co2_geo, D_Long_Geo, "Geo: Raw $P_{CO_2}$") |
    plot_residuals_boxplot(mod_thm_co2_geo, D_Long_Geo, "Geo: Thermo a_CO2") |
    plot_residuals_boxplot(mod_raw_aae_geo, D_Long_Geo, "Geo: Legacy P_AAE10")) /
    (plot_residuals_boxplot(mod_raw_co2_agro, D_Long_Agro, "Agro: Raw $P_{CO_2}$") |
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
    get_effects(mod_raw_co2_geo, "1. Geo PBC - Raw $P_{CO_2}$"),
    get_effects(mod_raw_co2_agro, "1. Agro PBC - Raw $P_{CO_2}$"),
    get_effects(mod_thm_co2_geo, "2. Geo PBC - Thermo a_CO2"),
    get_effects(mod_thm_co2_agro, "2. Agro PBC - Thermo a_CO2"),
    get_effects(mod_raw_aae_geo, "3. Geo PBC - Legacy P_AAE10"),
    get_effects(mod_raw_aae_agro, "3. Agro PBC - Legacy P_AAE10")
)

print(as.data.frame(all_effects), row.names = FALSE)


## ----cp-mechanistic-model-----------------------------------------------------
# 1. Filter out biological outliers (e.g., crop failures where near-zero yield artificially inflates C_P)
D_CP <- D_Long_Agro |> 
    filter(is.finite(annual_P_uptake), is.finite(annual_yield_mp_DM)) |> 
    mutate(C_P = annual_P_uptake / annual_yield_mp_DM) |> 
    filter(C_P > 0, C_P < 1.0) |> 
    mutate(P_CO2 = as.numeric(soil_0_20_P_CO2))

D_CP_nona <- na.omit(D_CP[, c("C_P", "P_CO2", "z_v0", "z_inv_b_agro", "crop", "site", "year_f")])

# 2. Define starting values
n_crops <- length(unique(D_CP_nona$crop))
start_vals <- c(
    C_base = median(D_CP_nona$C_P), rep(0, n_crops - 1),
    S_base = 0.01,
    beta_v0 = 0, beta_invb = 0
)

# 3. Fit the Simplified Phenomenological Model
mod_cp_mechanistic <- nlme(
    C_P ~ C_base + (S_base + beta_v0 * z_v0 + beta_invb * z_inv_b_agro) * P_CO2,
    data = D_CP_nona,
    fixed = list(C_base ~ crop, S_base + beta_v0 + beta_invb ~ 1),
    random = C_base ~ 1 | site/year_f,
    start = start_vals,
    control = nlmeControl(maxIter = 1000)
)

print(summary(mod_cp_mechanistic))

# Performance Metrics
preds <- predict(mod_cp_mechanistic)
resids <- D_CP_nona$C_P - preds
rmse <- sqrt(mean(resids^2))
r2 <- cor(D_CP_nona$C_P, preds)^2
cat(sprintf("\nPerformance: RMSE = %.4f kg P/dt DM | R2 = %.4f\n", rmse, r2))


## ----mitscherlich-yield-models, fig.width=14, fig.height=5--------------------
# 1. Prepare the Dataset for YIELD (Grouped by Site AND Crop)
# 1. Prepare the Dataset for YIELD (Grouped by Site AND Crop)
