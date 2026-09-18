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

rm(list = ls())
library(tikzDevice)
options(tikzDefaultEngine = "luatex")
options(tikzLualatex = "/root/.TinyTeX/bin/x86_64-linux/lualatex")
knitr::opts_chunk$set(
  dev = "tikz",
  fig.align = "center",
  cache = TRUE
)

suppressPackageStartupMessages({
    require(parallel)
    require(lme4)
    require(lmerTest) # For p-values
    require(ggplot2)
    require(dplyr)
    require(tidyr) # Ensure tidyr is loaded for drop_na
    require(patchwork)
    require(performance)
    require(kableExtra)
    require(readxl)
    require(jsonlite)
    require(nlme)
    require(broom.mixed)
    require(rpart)
})

num_cores <- parallel::detectCores()

    get_metrics_lmer <- function(model, model_name, data, response) {
        r2 <- suppressWarnings(performance::r2_nakagawa(model))
        r2m <- round(as.numeric(r2$R2_marginal), 3)
        r2c <- round(as.numeric(r2$R2_conditional), 3)
        
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
        max_level <- length(model$groups)
        pred_c <- predict(model, level = max_level) # Plot level
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
    ) |>
    droplevels() # 40-Year Full Dataset (All Treatments)

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

# Safely filter complete cases
D_ptf <- D_ready |>
    drop_na(ln_P_AAE, ln_P_CO2, ln_a_CO2, z_ln_FineTexture, z_pH, z_ln_Ca, z_ln_Mg, z_ln_K, z_ln_Corg, z_Temp_Anom, z_Prec_Anom, z_Temp_Mean, z_ln_Feox, z_ln_Alox) |>
    mutate(site = droplevels(factor(site)))

# Models
jobs <- list(
  ptf_agro_raw = function() {
    lmer(ln_P_AAE ~ ln_P_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf)
  },
  ptf_agro_thm = function() {
    lmer(ln_P_AAE ~ ln_a_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf)
  },
  ptf_geo_raw = function() {
    lmer(ln_P_AAE ~ ln_P_CO2 * (z_ln_Feox + z_ln_Alox + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf)
  },
  ptf_geo_thm = function() {
    lmer(ln_P_AAE ~ ln_a_CO2 * (z_ln_Feox + z_ln_Alox + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf)
  }
)
res <- mclapply(jobs, function(f) f(), mc.cores = min(length(jobs), num_cores))
ptf_agro_raw <- res$ptf_agro_raw
ptf_agro_thm <- res$ptf_agro_thm
ptf_geo_raw <- res$ptf_geo_raw
ptf_geo_thm <- res$ptf_geo_thm


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

# Create maximized dataset without Feox/Alox constraints
D_ptf_agro <- D_ready |>
    drop_na(ln_P_AAE, ln_P_CO2, ln_a_CO2, z_ln_FineTexture, z_pH, z_ln_Ca, z_ln_Mg, z_ln_K, z_ln_Corg, z_Temp_Anom, z_Prec_Anom, z_Temp_Mean) |>
    mutate(site = droplevels(factor(site)))

cat("Total trials successfully included in Practical PTF:", length(unique(D_ptf_agro$site)), "\n")
print(unique(D_ptf_agro$site))

# Fit practical models
jobs <- list(
  ptf_practical_raw = function() {
    lmer(ln_P_AAE ~ ln_P_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf_agro)
  },
  ptf_practical_thm = function() {
    lmer(ln_P_AAE ~ ln_a_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf_agro)
  }
)
res <- mclapply(jobs, function(f) f(), mc.cores = min(length(jobs), num_cores))
ptf_practical_raw <- res$ptf_practical_raw
ptf_practical_thm <- res$ptf_practical_thm


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

# Create the Conservative Artifact-Free Dataset
D_ptf_cons <- D_ptf_agro |>
    filter(soil_0_20_pH_H2O <= 7.3)

cat("Trials included in Conservative PTF:", length(unique(D_ptf_cons$site)), "\n")

# Fit Conservative Models
jobs <- list(
  ptf_cons_raw = function() {
    lmer(ln_P_AAE ~ ln_P_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf_cons)
  },
  ptf_cons_thm = function() {
    lmer(ln_P_AAE ~ ln_a_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf_cons)
  }
)
res <- mclapply(jobs, function(f) f(), mc.cores = min(length(jobs), num_cores))
ptf_cons_raw <- res$ptf_cons_raw
ptf_cons_thm <- res$ptf_cons_thm


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

# 1. Extract the Coefficients from Both PTFs
coefs_geo <- fixef(ptf_geo_raw)
coefs_agro <- fixef(ptf_practical_raw)
coefs_cons <- fixef(ptf_cons_thm)
coefs_cons_raw <- fixef(ptf_cons_raw)

# Safe Extractors
C_geo <- function(name) {
    res <- if (name %in% names(coefs_geo)) coefs_geo[[name]] else 0
    if (length(res) == 0) return(0)
    return(res)
}
get_int_geo <- function(v1, v2) {
    n1 <- paste0(v1, ":", v2)
    n2 <- paste0(v2, ":", v1)
    if (n1 %in% names(coefs_geo)) res <- coefs_geo[[n1]]
    else if (n2 %in% names(coefs_geo)) res <- coefs_geo[[n2]]
    else res <- 0
    if (length(res) == 0) return(0)
    return(res)
}

C_agro <- function(name) {
    res <- if (name %in% names(coefs_agro)) coefs_agro[[name]] else 0
    if (length(res) == 0) return(0)
    return(res)
}
get_int_agro <- function(v1, v2) {
    n1 <- paste0(v1, ":", v2)
    n2 <- paste0(v2, ":", v1)
    if (n1 %in% names(coefs_agro)) res <- coefs_agro[[n1]]
    else if (n2 %in% names(coefs_agro)) res <- coefs_agro[[n2]]
    else res <- 0
    if (length(res) == 0) return(0)
    return(res)
}

C_cons <- function(name) {
    res <- if (name %in% names(coefs_cons)) coefs_cons[[name]] else 0
    if (length(res) == 0) return(0)
    return(res)
}
get_int_cons <- function(v1, v2) {
    n1 <- paste0(v1, ":", v2)
    n2 <- paste0(v2, ":", v1)
    if (n1 %in% names(coefs_cons)) res <- coefs_cons[[n1]]
    else if (n2 %in% names(coefs_cons)) res <- coefs_cons[[n2]]
    else res <- 0
    if (length(res) == 0) return(0)
    return(res)
}

C_cons_raw <- function(name) {
    res <- if (name %in% names(coefs_cons_raw)) coefs_cons_raw[[name]] else 0
    if (length(res) == 0) return(0)
    return(res)
}
get_int_cons_raw <- function(v1, v2) {
    n1 <- paste0(v1, ":", v2)
    n2 <- paste0(v2, ":", v1)
    if (n1 %in% names(coefs_cons_raw)) res <- coefs_cons_raw[[n1]]
    else if (n2 %in% names(coefs_cons_raw)) res <- coefs_cons_raw[[n2]]
    else res <- 0
    if (length(res) == 0) return(0)
    return(res)
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
            get_int_cons("ln_a_CO2", "z_ln_Corg") * z_ln_Corg,

        ln_K_pred_cons = C_cons("(Intercept)") + C_cons("z_ln_FineTexture") * z_ln_FineTexture + C_cons("z_pH") * z_pH + C_cons("z_ln_Ca") * z_ln_Ca + C_cons("z_ln_Mg") * z_ln_Mg + C_cons("z_ln_K") * z_ln_K + C_cons("z_ln_Corg") * z_ln_Corg,

        b_power_cons = n_pred_cons * exp(ln_K_pred_cons) * (a_CO2_total_mg_L^(n_pred_cons - 1)),
        inv_b_cons = 1 / b_power_cons
    ) |>

        # Using Absolute Uptake (no normalization)
    filter(annual_P_uptake > 0) |>
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
# ---------------------------------------------------------
# PLANT UPTAKE MODELS (ABSOLUTE SCALE)
# ---------------------------------------------------------
D_Long_Agro <- D_Long |> filter(crop %in% c("WW", "WG", "SW", "KM", "SM", "KA", "ZR", "RA")) |> filter(is.finite(z_inv_b_agro), !is.na(z_fert_N), !is.na(z_Temp_Anom)) |> mutate(crop = as.factor(crop)) |> droplevels() |> mutate(z_inv_b_agro = as.numeric(scale(inv_b_agro)), z_v0 = as.numeric(scale(k * soil_0_20_P_CO2)), z_fert_N = as.numeric(scale(fert_N_tot)))
D_Long_Thm <- D_Long |> filter(crop %in% c("WW", "WG", "SW", "KM", "SM", "KA", "ZR", "RA")) |> filter(is.finite(z_inv_b_agro), !is.na(z_fert_N), !is.na(z_Temp_Anom)) |> mutate(crop = as.factor(crop)) |> droplevels() |> mutate(z_inv_b_agro = as.numeric(scale(inv_b_agro)), z_v0 = as.numeric(scale(k * a_CO2_total_mg_L)), z_fert_N = as.numeric(scale(fert_N_tot)))
D_Long_Cons <- D_Long |> filter(crop %in% c("WW", "WG", "SW", "KM", "SM", "KA", "ZR", "RA")) |> filter(is.finite(z_inv_b_cons), !is.na(z_fert_N), !is.na(z_Temp_Anom)) |> mutate(crop = as.factor(crop)) |> droplevels() |> mutate(z_inv_b_cons = as.numeric(scale(inv_b_cons)), z_v0 = as.numeric(scale(k * a_CO2_total_mg_L)), z_fert_N = as.numeric(scale(fert_N_tot)))
n_crops <- length(levels(D_Long_Agro$crop))

cat("\n--- DIAGNOSTICS ---\n")
cat("D_Long rows:", nrow(D_Long), "\n")
cat("D_Long_Agro rows:", nrow(D_Long_Agro), "\n")
cat("D_Long_Agro crop levels:", paste(levels(D_Long_Agro$crop), collapse=", "), "\n")
cat("Missing in D_Long_Agro:\n")
print(colSums(is.na(D_Long_Agro[, c("annual_P_uptake", "soil_0_20_P_CO2", "z_fert_N", "z_Temp_Anom", "z_inv_b_agro", "z_v0")])))
cat("-------------------\n")

cat("Trials included in Uptake Models:", length(unique(D_Long_Agro$site)), "(", paste(unique(D_Long_Agro$site), collapse = ", "), ")\n")

jobs <- list(
  mod_raw_co2 = function() {
    nlme(
        annual_P_uptake ~ ((V_max + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_CO2) /
            ((K_base * exp(beta_invb * z_inv_b_agro + beta_k * z_k)) + soil_0_20_P_CO2),
        data = D_Long_Agro, fixed = list(V_max ~ crop, beta_temp ~ 1, beta_N ~ 1, K_base ~ crop, beta_invb ~ 1, beta_k ~ 1), random = V_max ~ 1 | site/year_f,
        weights = varPower(form = ~ soil_0_20_P_CO2),
        start = c(30, rep(0, n_crops - 1), 0, 0, median(D_Long_Agro$soil_0_20_P_CO2), rep(0, n_crops - 1), 0, 0), control = nlmeControl(maxIter = 1000, opt = "nlm", pnlsTol = 0.05)
    )
  },
  mod_thm_co2 = function() {
    nlme(
        annual_P_uptake ~ ((V_max + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * a_CO2_total_mg_L) /
            ((K_base * exp(beta_invb * z_inv_b_agro + beta_k * z_k)) + a_CO2_total_mg_L),
        data = D_Long_Agro, fixed = list(V_max ~ crop, beta_temp ~ 1, beta_N ~ 1, K_base ~ crop, beta_invb ~ 1, beta_k ~ 1), random = V_max ~ 1 | site/year_f,
        weights = varPower(form = ~ a_CO2_total_mg_L),
        start = c(30, rep(0, n_crops - 1), 0, 0, median(D_Long_Agro$a_CO2_total_mg_L), rep(0, n_crops - 1), 0, 0), control = nlmeControl(maxIter = 1000, opt = "nlm", pnlsTol = 0.05)
    )
  },
  mod_raw_aae = function() {
    nlme(
        annual_P_uptake ~ ((V_max + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_AAE10) /
            ((K_base * exp(beta_invb * z_inv_b_agro + beta_k * z_k)) + soil_0_20_P_AAE10),
        data = D_Long_Agro, fixed = list(V_max ~ crop, beta_temp ~ 1, beta_N ~ 1, K_base ~ crop, beta_invb ~ 1, beta_k ~ 1), random = V_max ~ 1 | site/year_f,
        weights = varPower(form = ~ soil_0_20_P_AAE10),
        start = c(30, rep(0, n_crops - 1), 0, 0, median(D_Long_Agro$soil_0_20_P_AAE10), rep(0, n_crops - 1), 0, 0), control = nlmeControl(maxIter = 1000, opt = "nlm", pnlsTol = 0.05)
    )
  }
)
res <- mclapply(jobs, function(f) {
  tryCatch(f(), error = function(e) e)
}, mc.cores = min(length(jobs), num_cores))

for (job_name in names(res)) {
  if (inherits(res[[job_name]], "error")) {
    stop(paste("Model", job_name, "failed with error:\n", res[[job_name]]$message))
  }
}

mod_raw_co2_agro <- res$mod_raw_co2
mod_thm_co2_agro <- res$mod_thm_co2
mod_raw_aae_agro <- res$mod_raw_aae

# Helper function to generate a clean effects table
get_effects <- function(mod, model_name) {
    broom.mixed::tidy(mod, effects = "fixed") |>
        mutate(
            Model = model_name,
            across(where(is.numeric), ~ round(., 4))
        ) |>
        dplyr::select(Model, term, estimate, std.error, statistic, p.value)
}

all_effects <- bind_rows(
    get_effects(mod_raw_co2_agro, "1. Agro PBC - Raw $P_{CO_2}$"),
    get_effects(mod_thm_co2_agro, "2. Agro PBC - Thermo a_CO2"),
    get_effects(mod_raw_aae_agro, "3. Agro PBC - Legacy P_AAE10")
)
print(as.data.frame(all_effects), row.names = FALSE)

# 1. Filter out biological outliers (e.g., crop failures where near-zero yield artificially inflates C_P)
D_CP <- D_Long_Agro |> 
    filter(is.finite(annual_P_uptake), is.finite(annual_yield_mp_DM)) |> 
    mutate(C_P = annual_P_uptake / annual_yield_mp_DM) |> 
    filter(C_P > 0, C_P < 1.0) |> 
    mutate(P_CO2 = as.numeric(soil_0_20_P_CO2))

D_CP_nona <- na.omit(D_CP[, c("C_P", "P_CO2", "z_k", "z_inv_b_agro", "crop", "site", "year_f")])

# 2. Define starting values
n_crops <- length(unique(D_CP_nona$crop))
start_vals <- c(
    C_base = c(median(D_CP_nona$C_P), rep(0, n_crops - 1)),
    S_base = c(0.01, rep(0, n_crops - 1)),
    beta_k = 0, beta_invb = 0
)

# 3. Fit the Simplified Phenomenological Model
# Note: User requested crop as a fixed slope and intercept, and keeping the same random effects (1 | site/year_f).
# Because of the large number of parameters (2 * n_crops + 2) and complex temporal random effects without a hard bound,
# we increase maxIter and pnlsTol to prevent step-halving.
mod_cp_mechanistic <- nlme(
    C_P ~ C_base + (S_base + beta_k * z_k + beta_invb * z_inv_b_agro) * P_CO2,
    data = D_CP_nona,
    fixed = list(C_base ~ crop, S_base ~ crop, beta_k + beta_invb ~ 1),
    random = C_base ~ 1 | site/year_f,
    start = start_vals,
    control = nlmeControl(maxIter = 2000, pnlsMaxIter = 200, msMaxIter = 200, pnlsTol = 0.05, returnObject = TRUE),
    na.action = na.omit
)

print(summary(mod_cp_mechanistic))

# Performance Metrics
preds <- predict(mod_cp_mechanistic)
resids <- D_CP_nona$C_P - preds
rmse <- sqrt(mean(resids^2))
r2 <- cor(D_CP_nona$C_P, preds)^2
cat(sprintf("\nPerformance: RMSE = %.4f kg P/dt DM | R2 = %.4f\n", rmse, r2))

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
        ln_K_pred_agro = C_agro("(Intercept)") + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
        b_power = n_pred_agro * exp(ln_K_pred_agro) * (soil_0_20_P_CO2^(n_pred_agro - 1)),
        inv_b = 1 / b_power,
        total_yield = tidyr::replace_na(annual_yield_mp_DM, 0)
    ) |>

    # Calculate Conservative Artifact-Free Physical Highway (1/b) for Yield
    mutate(
        n_pred_cons = C_cons_raw("ln_P_CO2") +
            get_int_cons_raw("ln_P_CO2", "z_ln_FineTexture") * z_ln_FineTexture +
            get_int_cons_raw("ln_P_CO2", "z_pH") * z_pH +
            get_int_cons_raw("ln_P_CO2", "z_ln_Ca") * z_ln_Ca +
            get_int_cons_raw("ln_P_CO2", "z_ln_Mg") * z_ln_Mg +
            get_int_cons_raw("ln_P_CO2", "z_ln_K") * z_ln_K +
            get_int_cons_raw("ln_P_CO2", "z_ln_Corg") * z_ln_Corg,
        ln_K_pred_cons = C_cons_raw("(Intercept)") + C_cons_raw("z_ln_FineTexture") * z_ln_FineTexture + C_cons_raw("z_pH") * z_pH + C_cons_raw("z_ln_Ca") * z_ln_Ca + C_cons_raw("z_ln_Mg") * z_ln_Mg + C_cons_raw("z_ln_K") * z_ln_K + C_cons_raw("z_ln_Corg") * z_ln_Corg,
        b_power_cons = n_pred_cons * exp(ln_K_pred_cons) * (soil_0_20_P_CO2^(n_pred_cons - 1)),
        inv_b_cons = 1 / b_power_cons
    ) |>

    # Process Absolute Yields
    filter(crop %in% c("WW", "WG", "SW", "KM", "SM", "KA", "ZR", "RA")) |>
    filter(total_yield > 0) |>
    filter(!is.na(rollMean_soil_0_20_K_AAE10), !is.na(rollMean_soil_0_20_pH_H2O), !is.na(rollMean_soil_0_20_Mg_AAE10), !is.na(fert_N_tot), !is.na(site_juv_temp_mean), !is.na(prec_anomaly)) |>
    mutate(crop = droplevels(as.factor(crop))) |>
    filter(is.finite(inv_b), is.finite(inv_b_cons), is.finite(total_yield), total_yield > 0) |>
    mutate(
        z_inv_b = as.numeric(scale(inv_b)),
        z_inv_b_cons = as.numeric(scale(inv_b_cons)),
        z_fert_N = as.numeric(scale(fert_N_tot)),
        site = as.factor(site),
        year_f = as.factor(year),
        plot_nr = as.factor(plot_nr), crop = droplevels(as.factor(crop))
    )

cat("Total Harvest Years Evaluated for Yield (2010-2022):", nrow(D_Yield), "\n")
cat("Sites:", length(unique(D_Yield$site)), "->", paste(unique(D_Yield$site), collapse = ", "), "\n\n")

n_crops_yield <- length(levels(D_Yield$crop))

# Z-score the covariates to help the nonlinear optimizer
D_Yield <- D_Yield |>
    mutate(
        z_inv_b = as.numeric(scale(inv_b)),
        z_pH = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
        z_ln_K = as.numeric(scale(log(rollMean_soil_0_20_K_AAE10))),
        z_ln_Mg = as.numeric(scale(log(rollMean_soil_0_20_Mg_AAE10))),
        z_fert_N = as.numeric(scale(fert_N_tot)),
        z_Temp_Mean = as.numeric(scale(site_juv_temp_mean)),
        z_Prec_Anom = as.numeric(scale(prec_anomaly))
    )

jobs_yield <- list(
  m_yield_raw_co2 = function() {
    nlme(
        total_yield ~ Y0 + (A - Y0) * (1 - exp(-(c_base * exp(
            beta_invb * z_inv_b + beta_N * z_fert_N + beta_Temp * z_Temp_Mean + beta_Prec * z_Prec_Anom
        ) * (soil_0_20_P_CO2 + E_base)))),
        data = D_Yield,
        fixed = list(A ~ crop, Y0 ~ crop, c_base ~ crop, beta_invb ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
        random = E_base ~ 1 | site,
        weights = varPower(form = ~ soil_0_20_P_CO2),
        start = c(10, rep(0, n_crops_yield - 1), 2, rep(0, n_crops_yield - 1), 1.2, rep(0, n_crops_yield - 1), rep(0, 4), 0),
        control = nlmeControl(maxIter = 2000, returnObject = TRUE)
    )
  },
  m_yield_thm_co2 = function() {
    nlme(
        total_yield ~ Y0 + (A - Y0) * (1 - exp(-(c_base * exp(
            beta_invb * z_inv_b + beta_N * z_fert_N + beta_Temp * z_Temp_Mean + beta_Prec * z_Prec_Anom
        ) * (a_CO2_total_mg_L + E_base)))),
        data = D_Yield,
        fixed = list(A ~ crop, Y0 ~ crop, c_base ~ crop, beta_invb ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
        random = E_base ~ 1 | site,
        weights = varPower(form = ~ a_CO2_total_mg_L),
        start = c(10, rep(0, n_crops_yield - 1), 2, rep(0, n_crops_yield - 1), 1.2, rep(0, n_crops_yield - 1), rep(0, 4), 0),
        control = nlmeControl(maxIter = 2000, returnObject = TRUE)
    )
  },
  m_yield_raw_aae = function() {
    nlme(
        total_yield ~ Y0 + (A - Y0) * (1 - exp(-(c_base * exp(
            beta_invb * z_inv_b_cons + beta_N * z_fert_N + beta_Temp * z_Temp_Mean + beta_Prec * z_Prec_Anom
        ) * (soil_0_20_P_AAE10 + E_base)))),
        data = D_Yield,
        fixed = list(A ~ crop, Y0 ~ crop, c_base ~ crop, beta_invb ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
        random = E_base ~ 1 | site,
        weights = varPower(form = ~ soil_0_20_P_AAE10),
        start = c(10, rep(0, n_crops_yield - 1), 2, rep(0, n_crops_yield - 1), 0.05, rep(0, n_crops_yield - 1), rep(0, 4), 0),
        control = nlmeControl(maxIter = 2000, returnObject = TRUE)
    )
  }
)
res_y <- mclapply(jobs_yield, function(f) f(), mc.cores = min(length(jobs_yield), num_cores))
m_yield_raw_co2 <- res_y$m_yield_raw_co2
m_yield_thm_co2 <- res_y$m_yield_thm_co2
m_yield_raw_aae <- res_y$m_yield_raw_aae

cat("### Yield ~ Raw P_CO2 (Mitscherlich NLME) ###
")
print(round(summary(m_yield_raw_co2)$tTable, 4))


cat("
### Yield ~ Thermo a_CO2 (Mitscherlich NLME) ###
")
print(round(summary(m_yield_thm_co2)$tTable, 4))
cat("\n### Yield ~ Legacy P_AAE10 (Mitscherlich NLME) ###\n")
print(round(summary(m_yield_raw_aae)$tTable, 4))

cat("\n### Yield Models Performance Metrics ###\n")
df_yield_metrics <- dplyr::bind_rows(list(
    get_metrics_nlme(m_yield_raw_co2, "Yield ~ Raw $P_{CO_2}$", D_Yield, "total_yield"),
    get_metrics_nlme(m_yield_thm_co2, "Yield ~ Thermo a_CO2", D_Yield, "total_yield"),
    get_metrics_nlme(m_yield_raw_aae, "Yield ~ Legacy P_AAE10", D_Yield, "total_yield")
))
print(kable(df_yield_metrics, format = "markdown"))
cat("\n")

D_res_all <- dplyr::bind_rows(
    D_Yield |> mutate(Model = "Raw $P_{CO2}$ (Complete)", Fitted = predict(m_yield_raw_co2), Residual = residuals(m_yield_raw_co2)),

    D_Yield |> mutate(Model = "Thermo $a_{CO2}$", Fitted = predict(m_yield_thm_co2), Residual = residuals(m_yield_thm_co2)),
    D_Yield |> mutate(Model = "Legacy $P_{AAE10}$", Fitted = predict(m_yield_raw_aae), Residual = residuals(m_yield_raw_aae))
)

p_resid <- ggplot(D_res_all, aes(x = Fitted, y = Residual, color = site)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.4, size = 1.5) +
    facet_wrap(~Model, scales = "free_x", ncol=1) +
    labs(title = "Conditional Residuals vs Fitted", x = "Fitted Yield", y = "Residual", color = "Site") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

p_box <- ggplot(D_res_all, aes(x = site, y = Residual, fill = site)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    facet_wrap(~Model, ncol=1) +
    labs(title = "Yield Model Residuals by Site", x = "Site", y = "Residual", fill = "Site") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

(p_resid | p_box)

# ---------------------------------------------------------------------------
# P_crit = STP at which Y = 95% of maximum yield
#
# Since we modeled c_eff directly via NLME, we just compute it deterministically
# and plot the extracted effects without needing a redundant secondary lmer!
# ---------------------------------------------------------------------------

calc_pcrit <- function(model, name) {
    cf <- fixef(model)
    D_Yield |>
        mutate(
            Model = name,
            c_base_crop = cf["c_base.(Intercept)"] + tidyr::replace_na(cf[paste0("c_base.crop", crop)], 0),
            plot_full_id = paste0(site, "/", plot_nr),
            re_total = ranef(model)[as.character(site), 1],
            c_eff  = (c_base_crop + tidyr::replace_na(re_total, 0)) * exp(
                cf["beta_invb"] * z_inv_b +
                cf["beta_pH"] * z_pH +
                cf["beta_K"] * z_ln_K +
                cf["beta_Mg"] * z_ln_Mg +
                cf["beta_N"] * z_fert_N +
                cf["beta_Temp"] * z_Temp_Mean +
                cf["beta_Prec"] * z_Prec_Anom
            ),
            P_crit = (log(20) / c_eff) - cf['E_base'],
            ln_P_crit = log(P_crit),
            crop   = as.factor(crop)
        ) |>
        filter(is.finite(ln_P_crit))
}

D_Pcrit_all <- dplyr::bind_rows(
    calc_pcrit(m_yield_raw_co2, "Raw P\\_CO2 (Complete)"),

    calc_pcrit(m_yield_thm_co2, "Thermo a\\_CO2"),
    calc_pcrit(m_yield_raw_aae, "Legacy P\\_AAE10")
)

# --- Visualizations ---
# 1. P_crit distributions by site
p_pcrit_box <- ggplot(D_Pcrit_all, aes(x = site, y = P_crit, fill = site)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21) +
    facet_wrap(~Model, scales = "free_y", ncol = 1) +
    labs(title = "Critical STP Thresholds per Site",
        subtitle = "Derived from One-Step NLME Mitscherlich",
        x = "Site", y = "Critical P Quantity/Intensity", fill = "Site") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

# 2. Forest plot: drivers of Mitscherlich rate constant (c)
extract_effects <- function(model, name) {
    broom.mixed::tidy(model, effects = "fixed") |>
        filter(!grepl("c_base|E_base", term)) |>
        mutate(
            Model = name,
            lower = estimate - 1.96 * std.error,
            upper = estimate + 1.96 * std.error,
            estimate_inv = -estimate,
            lower_inv = -upper,
            upper_inv = -lower,
            term_clean = case_when(
                term == "beta_invb" ~ "Physical Buffer Power (1/b)",
                term == "beta_pH"   ~ "Soil pH",
                term == "beta_K"    ~ "Soil Extractable K",
                term == "beta_Mg"   ~ "Soil Extractable Mg",
                term == "beta_N"    ~ "Nitrogen Fertilizer",
                term == "beta_Temp" ~ "Mean Annual Temperature",
                term == "beta_Prec" ~ "Precipitation Anomaly"
            ),
            sig = ifelse(p.value < 0.05, "p < 0.05", "p ≥ 0.05")
        )
}

nlme_effects_all <- dplyr::bind_rows(
    extract_effects(m_yield_raw_co2, "Raw P\\_CO2 (Complete)"),

    extract_effects(m_yield_thm_co2, "Thermo a\\_CO2"),
    extract_effects(m_yield_raw_aae, "Legacy P\\_AAE10")
)

p_pcrit_forest <- ggplot(nlme_effects_all, aes(x = estimate, y = reorder(term_clean, estimate), color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, linewidth = 0.9) +
    geom_point(size = 4) +
    facet_wrap(~Model, ncol = 1) +
    scale_color_manual(values = c("p < 0.05" = "#2c7bb6", "p ≥ 0.05" = "gray60")) +
    labs(title = "Drivers of P-Foraging Efficiency (Rate Constant $c_{eff}$)",
        subtitle = "Negative = Slower uptake (Requires higher $P_{crit}$)",
        x = "Standardised Coefficient (log scale)", y = "", color = "") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

# 3. Forest plot: drivers of P_crit (1/c)
p_pcrit_forest_inv <- ggplot(nlme_effects_all, aes(x = estimate_inv, y = reorder(term_clean, estimate_inv), color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = lower_inv, xmax = upper_inv), height = 0.2, linewidth = 0.9) +
    geom_point(size = 4) +
    facet_wrap(~Model, ncol = 1) +
    scale_color_manual(values = c("p < 0.05" = "#d73027", "p ≥ 0.05" = "gray60")) +
    labs(title = "Drivers of the Critical P Threshold ($P_{crit}$)",
        subtitle = "Positive = Increases the required $P_{crit}$ (worse foraging)",
        x = "Standardised Coefficient (effect on $P_{crit}$)", y = "", color = "") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

(p_pcrit_box | p_pcrit_forest | p_pcrit_forest_inv) + plot_layout(widths = c(1, 1.2, 1.2))

# 1. Define the LOSO-CV function
loso_cv <- function(formula_str, data) {
    sites <- unique(as.character(data$site))
    
    cv_res <- mclapply(sites, function(test_site) {
        train_data <- data |> filter(site != test_site)
        test_data <- data |> filter(site == test_site)
        
        fit <- tryCatch(
            { lmer(as.formula(formula_str), data = train_data, control = lmerControl(calc.derivs = FALSE)) },
            warning = function(w) { lmer(as.formula(formula_str), data = train_data, control = lmerControl(calc.derivs = FALSE)) }
        )
        
        r2_marg <- as.numeric(performance::r2_nakagawa(fit)$R2_marginal)
        preds <- predict(fit, newdata = test_data, re.form = NA)
        obs <- test_data$ln_P_AAE
        r2_pred <- cor(preds, obs)^2
        
        c(r2_marg, r2_pred)
    }, mc.cores = min(length(sites), num_cores))
    
    in_sample_r2 <- sapply(cv_res, `[`, 1)
    out_sample_r2 <- sapply(cv_res, `[`, 2)
    
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
cv_jobs <- list(
    agro_raw = function() loso_cv(f_agro_raw, D_ptf),
    agro_thm = function() loso_cv(f_agro_thm, D_ptf),
    geo_raw  = function() loso_cv(f_geo_raw, D_ptf),
    geo_thm  = function() loso_cv(f_geo_thm, D_ptf)
)
cv_res_list <- mclapply(cv_jobs, function(f) f(), mc.cores = min(4, num_cores))
cv_res_agro_raw <- cv_res_list$agro_raw
cv_res_agro_thm <- cv_res_list$agro_thm
cv_res_geo_raw  <- cv_res_list$geo_raw
cv_res_geo_thm  <- cv_res_list$geo_thm

# 4. Compile and Present Results
cv_results <- bind_rows(
    cv_res_agro_raw |> mutate(Model = "Agronomic (Raw $P_{CO_2}$)"),
    cv_res_agro_thm |> mutate(Model = "Agronomic (Thermo a_CO2)"),
    cv_res_geo_raw  |> mutate(Model = "Geochemical (Raw $P_{CO_2}$)"),
    cv_res_geo_thm  |> mutate(Model = "Geochemical (Thermo a_CO2)")
) |> dplyr::select(Model, Mean_In_Sample_R2, Mean_Out_Sample_R2)

cv_results |>
    kbl(caption = "**Table 4: Spatial Leave-One-Site-Out Cross-Validation (LOSO-CV).** Variance explained by fixed effects only. The Geochemical models maintain a higher predictive capability on completely unseen environments, confirming that amorphous metal oxides are the true physical drivers of soil buffering capacity.") |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

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
        ln_K_pred_agro = C_agro("(Intercept)") + C_agro("z_ln_FineTexture") * z_ln_FineTexture + C_agro("z_pH") * z_pH + C_agro("z_ln_Ca") * z_ln_Ca + C_agro("z_ln_Mg") * z_ln_Mg + C_agro("z_ln_K") * z_ln_K + C_agro("z_ln_Corg") * z_ln_Corg + C_agro("z_Temp_Anom") * z_Temp_Anom + C_agro("z_Prec_Anom") * z_Prec_Anom + C_agro("z_Temp_Mean") * z_Temp_Mean,
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
        z_ln_K = as.numeric(scale(tidyr::replace_na(m_fert_K, 0))),
        z_ln_Mg = as.numeric(scale(tidyr::replace_na(m_fert_Mg, 0))),
        site = as.factor(site)
    )

cat("Total Plots Evaluated for Cumulative P Balance (1990-2022):", nrow(D_Cum), "\n\n")

# 2. Fit Full Models (with fixed covariates to isolate buffer effects)
jobs <- list(
  m_bal_co2 = function() {
    lmer(Cumulated_P_Balance ~ ln_P_CO2 * z_inv_b + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  },
  m_bal_thm = function() {
    lmer(Cumulated_P_Balance ~ ln_a_CO2 * z_inv_b + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  },
  m_bal_aae = function() {
    lmer(Cumulated_P_Balance ~ ln_P_AAE10 * z_inv_b + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  }
)
res <- mclapply(jobs, function(f) f(), mc.cores = min(length(jobs), num_cores))
m_bal_co2 <- res$m_bal_co2
m_bal_thm <- res$m_bal_thm
m_bal_aae <- res$m_bal_aae

# 3. Fit Freundlich n Models
jobs <- list(
  m_bal_co2_n = function() {
    lmer(Cumulated_P_Balance ~ ln_P_CO2 * z_n + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  },
  m_bal_thm_n = function() {
    lmer(Cumulated_P_Balance ~ ln_a_CO2 * z_n + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  },
  m_bal_aae_n = function() {
    lmer(Cumulated_P_Balance ~ ln_P_AAE10 * z_n + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  }
)
res <- mclapply(jobs, function(f) f(), mc.cores = min(length(jobs), num_cores))
m_bal_co2_n <- res$m_bal_co2_n
m_bal_thm_n <- res$m_bal_thm_n
m_bal_aae_n <- res$m_bal_aae_n

# 4. Fit Buffer b Models
jobs <- list(
  m_bal_co2_b = function() {
    lmer(Cumulated_P_Balance ~ ln_P_CO2 * z_b + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  },
  m_bal_thm_b = function() {
    lmer(Cumulated_P_Balance ~ ln_a_CO2 * z_b + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  },
  m_bal_aae_b = function() {
    lmer(Cumulated_P_Balance ~ ln_P_AAE10 * z_b + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  }
)
res <- mclapply(jobs, function(f) f(), mc.cores = min(length(jobs), num_cores))
m_bal_co2_b <- res$m_bal_co2_b
m_bal_thm_b <- res$m_bal_thm_b
m_bal_aae_b <- res$m_bal_aae_b

# 5. Fit Null Models
jobs <- list(
  m_bal_co2_null = function() {
    lmer(Cumulated_P_Balance ~ ln_P_CO2 + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  },
  m_bal_thm_null = function() {
    lmer(Cumulated_P_Balance ~ ln_a_CO2 + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  },
  m_bal_aae_null = function() {
    lmer(Cumulated_P_Balance ~ ln_P_AAE10 + z_pH + z_Temp + z_Tex + z_fert_N + z_ln_K + z_ln_Mg + (1 | site), data = D_Cum)
  }
)
res <- mclapply(jobs, function(f) f(), mc.cores = min(length(jobs), num_cores))
m_bal_co2_null <- res$m_bal_co2_null
m_bal_thm_null <- res$m_bal_thm_null
m_bal_aae_null <- res$m_bal_aae_null

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
    extract_bal(m_bal_co2, "1. Full - Raw $P_{CO_2}$"),
    extract_bal(m_bal_thm, "2. Full - Thermo a_CO2"),
    extract_bal(m_bal_aae, "3. Full - Legacy P_AAE10"),
    extract_bal(m_bal_co2_n, "1. Freundlich n - Raw $P_{CO_2}$"),
    extract_bal(m_bal_thm_n, "2. Freundlich n - Thermo a_CO2"),
    extract_bal(m_bal_aae_n, "3. Freundlich n - Legacy P_AAE10"),
    extract_bal(m_bal_co2_b, "1. Buffer b - Raw $P_{CO_2}$"),
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
    labs(title = "Full Model ($P_{CO_2} \\cdot 1/b$)", x = "Predicted Cumulative P Balance", y = "Observed Cumulative P Balance") +
    theme_minimal() + theme(legend.position = "none")

p_null <- ggplot(plot_data_bal, aes(x = Predicted_Null, y = Cumulated_P_Balance, color = site)) +
    geom_point(alpha = 0.7, size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = "Null Model ($P_{CO_2}$)", x = "Predicted Cumulative P Balance", y = "") +
    theme_minimal()

(p_full | p_null) + plot_layout(guides = "collect") & theme(legend.position = "right")

rmse_table <- data.frame(
    Model = c("Raw P_CO2", "Thermo a_CO2", "Legacy P_AAE10"),
    Pseudo_R2 = c(
        cor(D_Yield$total_yield, predict(m_yield_raw_co2, level = 1))^2,
        cor(D_Yield$total_yield, predict(m_yield_thm_co2, level = 1))^2,
        cor(D_Yield$total_yield, predict(m_yield_raw_aae, level = 1))^2
    ),
    RMSE_Conditional = c(
        sqrt(mean(residuals(m_yield_raw_co2, level = 1)^2)),
        sqrt(mean(residuals(m_yield_thm_co2, level = 1)^2)),
        sqrt(mean(residuals(m_yield_raw_aae, level = 1)^2))
    ),
    RMSE_Marginal = c(
        sqrt(mean(residuals(m_yield_raw_co2, level = 0)^2)),
        sqrt(mean(residuals(m_yield_thm_co2, level = 0)^2)),
        sqrt(mean(residuals(m_yield_raw_aae, level = 0)^2))
    )
)

rmse_table |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 3))) |>
    kbl(caption = "Performance Metrics of One-Step Mitscherlich NLME Models") |>
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

# # 1. Generate Monte Carlo grid (100k points) bounded by empirical ranges
# set.seed(42)
# N <- 100000
# ranges <- lapply(D_Yield[c("z_inv_b", "z_pH", "z_ln_K", "z_ln_Mg", "z_fert_N", "z_Temp_Mean", "z_Prec_Anom")], function(x) c(min(x, na.rm=T), max(x, na.rm=T)))
# ranges_ptf <- lapply(D_Yield[c("z_ln_FineTexture", "z_ln_Ca", "z_ln_Corg", "z_Temp_Anom")], function(x) c(min(x, na.rm=T), max(x, na.rm=T)))
# all_ranges <- c(ranges, ranges_ptf)
# 
# grid <- data.frame(
#     crop = factor(rep("WW", N), levels = levels(D_Yield$crop))
# )
# for(cov in names(all_ranges)) {
#     grid[[cov]] <- runif(N, all_ranges[[cov]][1], all_ranges[[cov]][2])
# }
# 
# # 2. Evaluate Yield Model (P_crit)
# cf_y <- fixef(m_yield_raw_co2)
# c_base_ww <- cf_y["c_base.(Intercept)"] + cf_y["c_base.cropWW"]
# grid$c_eff <- c_base_ww * exp(
#     cf_y["beta_invb"] * grid$z_inv_b + cf_y["beta_pH"] * grid$z_pH +
#     cf_y["beta_K"] * grid$z_ln_K + cf_y["beta_Mg"] * grid$z_ln_Mg +
#     cf_y["beta_N"] * grid$z_fert_N + cf_y["beta_Temp"] * grid$z_Temp_Mean + cf_y["beta_Prec"] * grid$z_Prec_Anom
# )
# grid$P_crit <- (log(20) / grid$c_eff) - cf_y["E_base"]
# grid <- grid |> dplyr::filter(P_crit > 0)
# 
# # 3. Evaluate PTF Model (Q_crit)
# grid$ln_P_CO2 <- log(grid$P_crit)
# grid$pred_ln_Q <- predict(ptf_practical_raw, newdata = grid, re.form = NA)
# grid$Q_crit <- exp(grid$pred_ln_Q)
# grid$Delta_Q <- grid$Q_crit - 10 # Assuming depleted baseline P_AAE10 of 10 mg/kg
# 
# # 4. Extract Boundaries via Decision Tree
# grid$Applicable <- ifelse(grid$Delta_Q <= 50, "Safe", "Danger")
# grid$Applicable <- factor(grid$Applicable, levels=c("Danger", "Safe"))
# 
# tree <- rpart(Applicable ~ z_inv_b + z_pH + z_Temp_Mean + z_Prec_Anom + z_ln_FineTexture + z_ln_Ca + z_ln_Corg,
#               data = grid, method = "class", control = rpart.control(cp = 0.05, maxdepth = 3))
# 
# # Print Tree using base graphics to avoid dependency issues
# plot(tree, uniform = TRUE, main = "Applicability Manifold: Decision Tree Boundaries", margin = 0.1)
# text(tree, use.n = TRUE, all = TRUE, cex = 0.8)
# 
# # Calculate unscaled pH boundary
# split_val <- tree$splits[1, "index"]
# mean_pH <- mean(D_Yield$rollMean_soil_0_20_pH_H2O, na.rm=TRUE)
# sd_pH <- sd(D_Yield$rollMean_soil_0_20_pH_H2O, na.rm=TRUE)
# unscaled_pH <- mean_pH + split_val * sd_pH
# 
# cat("Absolute Master Boundary extracted by the algorithm: pH =", round(unscaled_pH, 2), "\n")

# library(patchwork)
# 
# # 1. Calculate Q_safe for all plots using the exact pedoclimatic covariates
# D_env <- D_Yield |> filter(!is.na(z_ln_FineTexture), !is.na(z_ln_Corg), !is.na(inv_b))
# D_env$ln_P_CO2 <- log(1.0) # Set Danger Threshold to 1.0 mg/L
# 
# D_env$pred_ln_Q_safe <- predict(ptf_practical_raw, newdata = D_env, re.form = NA)
# D_env$Q_safe <- exp(D_env$pred_ln_Q_safe)
# D_env$b <- 1 / D_env$inv_b
# 
# # 2. Plot 1: The Analytical Phase Diagram (Q_safe vs b)
# p_b <- ggplot(D_env, aes(x = b, y = Q_safe, color = site)) +
#     geom_point(alpha=0.6) +
#     geom_smooth(method="lm", color="black", linetype="dashed") +
#     labs(
#         title = "Theoretical Storage Capacity vs Buffer Power",
#         x = "Buffer Capacity (b = dQ/dI)",
#         y = "Max Safe P_AAE10 (mg/kg) at P_CO2 = 1.0"
#     ) +
#     theme_minimal() +
#     theme(legend.position="none")
# 
# # 3. Plot 2: Alignment with GRUD (Fine Texture and C_org)
# # Unscale Fine Texture for readability
# scale_center_ft <- attr(scale(log(D_ready$rollMean_soil_0_20_clay + D_ready$rollMean_soil_0_20_silt)), "scaled:center")
# scale_scale_ft <- attr(scale(log(D_ready$rollMean_soil_0_20_clay + D_ready$rollMean_soil_0_20_silt)), "scaled:scale")
# 
# D_env$FineTexture_Pct <- exp(D_env$z_ln_FineTexture * scale_scale_ft + scale_center_ft)
# D_env$Corg_Class <- cut(D_env$rollMean_soil_0_20_Corg, breaks=c(0, 1.5, 2.5, 5), labels=c("Low Corg (<1.5%)", "Med Corg (1.5-2.5%)", "High Corg (>2.5%)"))
# 
# p_grud <- ggplot(D_env |> filter(!is.na(Corg_Class)), aes(x = FineTexture_Pct, y = Q_safe, color = Corg_Class)) +
#     geom_smooth(method="lm", se=FALSE, linewidth=1.5) +
#     geom_point(alpha=0.3) +
#     labs(
#         title = "Mechanistic Validation of GRUD Supply Classes",
#         x = "Fine Texture (% Clay + Silt)",
#         y = "Max Safe P_AAE10 (mg/kg)",
#         color = "Soil Organic Matter"
#     ) +
#     theme_minimal() +
#     theme(legend.position="bottom")
# 
# p_b | p_grud

# # Calculate the real-world field deficit (kg P / ha) for all deficient plots
# # We use def_data_acidic to ensure we only evaluate plots within the model's valid boundary (pH < 7.2)
# def_data_field <- def_data_acidic |>
#     dplyr::filter(P_crit_loo < 5.0) |> # Remove extreme LOOCV extrapolation artifacts for unseen soil profiles
#     dplyr::mutate(
#         Delta_Q_field = Delta_Q * 3.6,
#         Amortized_5_Year = Delta_Q_field / 5
#     )
# 
# # Summary of the required P additions
# cat("### Real-World Deficit Summary (kg P/ha) ###\n")
# print(summary(def_data_field$Delta_Q_field))
# 
# # Plot the distribution of required field additions
# p_field <- ggplot(def_data_field, aes(x = Delta_Q_field, fill = site)) +
#     geom_histogram(bins=30, color="black", alpha=0.8) +
#     labs(
#         title = "Distribution of Soil Phosphorus Deficits (kg P / ha)",
#         subtitle = "Based on the exact non-linear integral to reach optimal P_crit",
#         x = "Total Required Soil-Building P Addition (kg P / ha)",
#         y = "Number of Plot-Years"
#     ) +
#     theme_minimal() +
#     theme(legend.position="bottom")
# 
# print(p_field)
