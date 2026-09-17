## ----setup, message=FALSE, warning=FALSE--------------------------------------
# Auto-install and load required packages
required_pkgs <- c("tidyverse", "brms", "loo", "cmdstanr")
for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    if (pkg == "cmdstanr") {
      install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
      library(cmdstanr)
      cmdstanr::install_cmdstan()
    } else {
      install.packages(pkg, repos = "https://cloud.r-project.org")
      library(pkg, character.only = TRUE)
    }
  }
}

if (!dir.exists("../models")) dir.create("../models")

# Load pre-cleaned data
d_brms <- readRDS("../data/D_Yield_clean.rds")

# Recreate GRUD Supply Class logic natively
grud_co2_matrix <- matrix(c(
  1.5, 1.5, 1.5, 1.4, 1.2, 1.4, 1.4, 1.3, 1.2, 1.1, 1.2, 1.2, 1.1, 1.0, 1.0,
  1.0, 1.0, 1.0, 1.0, 0.8, 1.0, 1.0, 1.0, 0.8, 0.6, 1.0, 1.0, 0.8, 0.6, 0.0,
  1.0, 0.8, 0.6, 0.0, 0.0, 0.8, 0.8, 0.4, 0.0, 0.0, 0.8, 0.6, 0.0, 0.0, 0.0,
  0.6, 0.4, 0.0, 0.0, 0.0, 0.6, 0.4, 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0.0, 0.0,
  0.4, 0.0, 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0
), ncol = 5, byrow = TRUE)

get_grud_co2 <- function(p_val, clay) {
  if (is.na(p_val) || is.na(clay)) return(NA)
  clay_idx <- min(5, max(1, floor(clay / 10) + 1))
  thresholds <- c(0.15, 0.46, 0.77, 1.08, 1.39, 1.70, 2.01, 2.32, 2.63, 2.94, 3.25, 3.56, 3.88, 4.19, 4.50)
  row_idx <- which(p_val <= thresholds)[1]
  if (is.na(row_idx)) row_idx <- 16
  return(grud_co2_matrix[row_idx, clay_idx])
}
get_grud_co2_vec <- Vectorize(get_grud_co2)

# Recreate GRUD Supply Class logic natively for AAE10
grud_aae_matrix <- matrix(c(
  1.5, 1.5, 1.5, 1.4, 1.4,
  1.5, 1.5, 1.4, 1.4, 1.2,
  1.5, 1.4, 1.4, 1.2, 1.2,
  1.4, 1.4, 1.2, 1.2, 1.2,
  1.4, 1.2, 1.2, 1.2, 1.0,
  1.2, 1.2, 1.2, 1.0, 1.0,
  1.2, 1.2, 1.0, 1.0, 1.0,
  1.2, 1.0, 1.0, 1.0, 1.0,
  1.0, 1.0, 1.0, 1.0, 1.0,
  1.0, 1.0, 1.0, 1.0, 1.0,
  1.0, 1.0, 1.0, 1.0, 0.8,
  1.0, 1.0, 1.0, 0.8, 0.8,
  1.0, 1.0, 0.8, 0.8, 0.8,
  1.0, 0.8, 0.8, 0.8, 0.6,
  0.8, 0.8, 0.8, 0.6, 0.6,
  0.8, 0.8, 0.6, 0.6, 0.6,
  0.8, 0.6, 0.6, 0.6, 0.4,
  0.6, 0.6, 0.6, 0.4, 0.4,
  0.6, 0.6, 0.4, 0.4, 0.4,
  0.6, 0.4, 0.4, 0.4, 0.0,
  0.4, 0.4, 0.4, 0.0, 0.0,
  0.4, 0.4, 0.0, 0.0, 0.0,
  0.4, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0
), ncol = 5, byrow = TRUE)

get_grud_aae <- function(p_val, clay) {
  if (is.na(p_val) || is.na(clay)) return(NA)
  clay_idx <- min(5, max(1, floor(clay / 10) + 1))
  thresholds <- seq(4.9, 124.9, by=5)
  row_idx <- which(p_val <= thresholds)[1]
  if (is.na(row_idx)) row_idx <- 26
  return(grud_aae_matrix[row_idx, clay_idx])
}
get_grud_aae_vec <- Vectorize(get_grud_aae)

# Inject GRUD Reference Targets
crop_refs <- data.frame(
  crop = c("Wheat", "KA", "KM", "RA", "SJ", "WG", "ZR", "FR"),
  Y_ref = c(60, 450, 100, 30, 30, 60, 900, 175),
  P_up_ref = c(27, 30, 38, 24, 30, 28, 41, 52)
) |> mutate(P_conc_ref = (P_up_ref * 10) / Y_ref)

d_brms <- d_brms |>
    # Harmonize Wheat codes to match the GRUD reference
    mutate(crop = case_when(
      crop %in% c("WW", "SW", "WS") ~ "Wheat",
      TRUE ~ as.character(crop)
    )) |>
    left_join(crop_refs, by = "crop") |>
    mutate(
      C_P = annual_P_uptake / annual_yield_mp_DM,
      z_k_pred = scale(ln_K_pred_agro)[, 1],
      Supply_class_CO2 = get_grud_co2_vec(soil_0_20_P_CO2, rollMean_soil_0_20_clay),
      Supply_class_AAE10 = get_grud_aae_vec(soil_0_20_P_AAE10, rollMean_soil_0_20_clay),
      crop = as.factor(crop),
      site = as.factor(site),
      year = as.factor(year)
    ) |>
    # CRITICAL: Drop any rows missing GRUD targets so ALL models train on the exact same rows!
    filter(
      !is.na(annual_yield_mp_DM), 
      !is.na(z_inv_b),
      !is.na(z_k_pred),
      !is.na(Y_ref),
      !is.na(Supply_class_CO2),
      !is.na(juvdev_temp),
      !is.na(juvdev_prec),
      !is.na(z_ln_Corg),
      !is.na(z_ln_Ca),
      !is.na(soil_0_20_P_AAE10),
      !is.na(Supply_class_AAE10)
    )

# Standard Priors
priors_linear <- c(set_prior("normal(0, 1000)", class = "b"))

bprior_uptake <- c(
  prior(normal(30, 40), nlpar = "Vmax", lb = 0),     
  prior(lognormal(1, 1), nlpar = "Kbase", lb = 0),     
  prior(normal(0, 0.5), nlpar = "betainvb"),
  prior(normal(0, 0.5), nlpar = "betak"),
  prior(normal(0, 0.5), nlpar = "betaN"),
  prior(normal(0, 0.5), nlpar = "betaTemp"),
  prior(normal(0, 0.5), nlpar = "betaPrec"),
  prior(normal(0, 0.5), nlpar = "betapH"),
  prior(normal(0, 0.5), nlpar = "betaClay"),
  prior(normal(0, 0.5), nlpar = "betaCorg"),
  prior(normal(0, 0.5), nlpar = "betaCa")
)

# Tight Data-Driven Yield Priors to completely resolve unidentifiability
bprior_yield <- c(
  prior(normal(107, 20), nlpar = "Y0", coef = "cropFR"),
  prior(normal(290, 40), nlpar = "A", coef = "cropFR"),
  prior(normal(13, 10), nlpar = "Y0", coef = "cropKA"),
  prior(normal(152, 20), nlpar = "A", coef = "cropKA"),
  prior(normal(20, 10), nlpar = "Y0", coef = "cropKM"),
  prior(normal(142, 20), nlpar = "A", coef = "cropKM"),
  prior(normal(3, 5), nlpar = "Y0", coef = "cropRA"),
  prior(normal(49, 10), nlpar = "A", coef = "cropRA"),
  prior(normal(10, 5), nlpar = "Y0", coef = "cropSJ"),
  prior(normal(44, 10), nlpar = "A", coef = "cropSJ"),
  prior(normal(21, 10), nlpar = "Y0", coef = "cropWG"),
  prior(normal(92, 15), nlpar = "A", coef = "cropWG"),
  prior(normal(3, 5), nlpar = "Y0", coef = "cropWheat"),
  prior(normal(73, 15), nlpar = "A", coef = "cropWheat"),
  prior(normal(36, 10), nlpar = "Y0", coef = "cropZR"),
  prior(normal(271, 30), nlpar = "A", coef = "cropZR"),
  prior(lognormal(1, 1), nlpar = "Kbase", lb = 0),
  prior(normal(0.010, 0.05), nlpar = "betainvb"),
  prior(normal(0.165, 0.05), nlpar = "betak"),
  prior(normal(0.776, 0.10), nlpar = "betaN"),
  prior(normal(-0.130, 0.10), nlpar = "betaTemp"),
  prior(normal(0.000, 0.05), nlpar = "betaPrec"),
  prior(normal(0, 0.5), nlpar = "betapH"),
  prior(normal(0, 0.5), nlpar = "betaClay"),
  prior(normal(0, 0.5), nlpar = "betaCorg"),
  prior(normal(0, 0.5), nlpar = "betaCa")
)

# HMC config
cores_n <- 16
chains_n <- 4
threads_n <- 4
iter_n <- 4000

get_rmse <- function(mod, y) {
  preds <- fitted(mod)[, "Estimate"]
  sqrt(mean((y - preds)^2, na.rm = TRUE))
}

## ----fit-base-----------------------------------------------------------------
# Yield
mod_base_Y <- brm(annual_yield_mp_DM ~ Y_ref * Supply_class_CO2 + (1 | site/year),
    data = d_brms, prior = priors_linear, backend = "cmdstanr",
    cores = cores_n, chains = chains_n, threads = threading(threads_n), 
    iter = iter_n, file = "../models/base_yield", file_refit = "on_change")

loo_base <- loo(mod_base_Y, cores = 1)
r2_base <- list(conditional = bayes_R2(mod_base_Y), marginal = bayes_R2(mod_base_Y, re_formula = NA))
rmse_base <- get_rmse(mod_base_Y, d_brms$annual_yield_mp_DM)
rm(mod_base_Y); gc()

# Uptake
mod_base_U <- brm(annual_P_uptake ~ P_up_ref * Supply_class_CO2 + (1 | site/year),
    data = d_brms, prior = priors_linear, backend = "cmdstanr",
    cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, file = "../models/base_uptake", file_refit = "on_change")
loo_base_U <- loo(mod_base_U, cores = 1)
r2_base_U <- list(conditional = bayes_R2(mod_base_U), marginal = bayes_R2(mod_base_U, re_formula = NA))
rmse_base_U <- get_rmse(mod_base_U, d_brms$annual_P_uptake)
rm(mod_base_U); gc()

## ----fit-base-AAE10-----------------------------------------------------------
# Yield
mod_base_Y_aae <- brm(annual_yield_mp_DM ~ Y_ref * Supply_class_AAE10 + (1 | site/year),
    data = d_brms, prior = priors_linear, backend = "cmdstanr",
    cores = cores_n, chains = chains_n, threads = threading(threads_n), 
    iter = iter_n, file = "../models/base_yield_aae", file_refit = "on_change")

loo_base_aae <- loo(mod_base_Y_aae, cores = 1)
r2_base_aae <- list(conditional = bayes_R2(mod_base_Y_aae), marginal = bayes_R2(mod_base_Y_aae, re_formula = NA))
rmse_base_aae <- get_rmse(mod_base_Y_aae, d_brms$annual_yield_mp_DM)
rm(mod_base_Y_aae); gc()

# Uptake
mod_base_U_aae <- brm(annual_P_uptake ~ P_up_ref * Supply_class_AAE10 + (1 | site/year),
    data = d_brms, prior = priors_linear, backend = "cmdstanr",
    cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, file = "../models/base_uptake_aae", file_refit = "on_change")
loo_base_U_aae <- loo(mod_base_U_aae, cores = 1)
r2_base_U_aae <- list(conditional = bayes_R2(mod_base_U_aae), marginal = bayes_R2(mod_base_U_aae, re_formula = NA))
rmse_base_U_aae <- get_rmse(mod_base_U_aae, d_brms$annual_P_uptake)
rm(mod_base_U_aae); gc()


## ----fit-null-----------------------------------------------------------------
# Yield Null: Michaelis-Menten
bform_Y_null <- bf(
  annual_yield_mp_DM ~ Y0 + (A - Y0) * soil_0_20_P_CO2 / (Kbase + soil_0_20_P_CO2),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  Kbase ~ crop - 1,
  nl = TRUE
)
mod_null_Y <- brm(bform_Y_null, data = d_brms, prior = bprior_yield[1:17, ], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, control = list(adapt_delta = 0.95, max_treedepth = 12), file = "../models/null_yield", file_refit = "on_change")
loo_null <- loo(mod_null_Y, cores = 1)
ce_null <- conditional_effects(mod_null_Y, effects = "soil_0_20_P_CO2:crop")
r2_null <- list(conditional = bayes_R2(mod_null_Y), marginal = bayes_R2(mod_null_Y, re_formula = NA))
rmse_null <- get_rmse(mod_null_Y, d_brms$annual_yield_mp_DM)
rm(mod_null_Y); gc()

# Uptake Null: Michaelis-Menten
bform_U_null <- bf(
  annual_P_uptake ~ (Vmax * soil_0_20_P_CO2) / (Kbase + soil_0_20_P_CO2),
  Vmax ~ crop - 1 + (1 | site/year),
  Kbase ~ crop - 1,
  nl = TRUE
)
mod_null_U <- brm(bform_U_null, data = d_brms, prior = bprior_uptake[1:2, ], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, control = list(adapt_delta = 0.95, max_treedepth = 12), file = "../models/null_uptake", file_refit = "on_change")
loo_null_U <- loo(mod_null_U, cores = 1)
ce_null_U <- conditional_effects(mod_null_U, effects = "soil_0_20_P_CO2:crop")
r2_null_U <- list(conditional = bayes_R2(mod_null_U), marginal = bayes_R2(mod_null_U, re_formula = NA))
rmse_null_U <- get_rmse(mod_null_U, d_brms$annual_P_uptake)
rm(mod_null_U); gc()


## ----fit-heuristic------------------------------------------------------------
# Yield Heuristic: Michaelis-Menten with 6 pedoclimatic covariates on Kbase
bform_Y_heur <- bf(
  annual_yield_mp_DM ~ Y0 + (A - Y0) * soil_0_20_P_CO2 / ((Kbase * exp(betapH * soil_0_20_pH_H2O + betaClay * rollMean_soil_0_20_clay + betaTemp * juvdev_temp + betaPrec * juvdev_prec + betaCorg * z_ln_Corg + betaCa * z_ln_Ca)) + soil_0_20_P_CO2),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  Kbase ~ crop - 1,
  betapH + betaClay + betaTemp + betaPrec + betaCorg + betaCa ~ 1,
  nl = TRUE
)
mod_heur_Y <- brm(bform_Y_heur, data = d_brms, prior = bprior_yield[c(1:17, 21:26), ], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, control = list(adapt_delta = 0.95, max_treedepth = 12), file = "../models/heur_yield", file_refit = "on_change")
loo_heur <- loo(mod_heur_Y, cores = 1)
ce_heur <- conditional_effects(mod_heur_Y, effects = "soil_0_20_P_CO2:crop")
params_heur <- summary(mod_heur_Y)$fixed
r2_heur <- list(conditional = bayes_R2(mod_heur_Y), marginal = bayes_R2(mod_heur_Y, re_formula = NA))
rmse_heur <- get_rmse(mod_heur_Y, d_brms$annual_yield_mp_DM)
rm(mod_heur_Y); gc()

# Uptake Heuristic: Michaelis-Menten with 6 pedoclimatic covariates on Kbase
bform_U_heur <- bf(
  annual_P_uptake ~ (Vmax * soil_0_20_P_CO2) / ((Kbase * exp(betaClay * rollMean_soil_0_20_clay + betapH * soil_0_20_pH_H2O + betaTemp * juvdev_temp + betaPrec * juvdev_prec + betaCorg * z_ln_Corg + betaCa * z_ln_Ca)) + soil_0_20_P_CO2),
  Vmax ~ crop - 1 + (1 | site/year),
  Kbase ~ crop - 1,
  betaClay + betapH + betaTemp + betaPrec + betaCorg + betaCa ~ 1,
  nl = TRUE
)
mod_heur_U <- brm(bform_U_heur, data = d_brms, prior = bprior_uptake[c(1:2, 6:11), ], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, control = list(adapt_delta = 0.95, max_treedepth = 12), file = "../models/heur_uptake", file_refit = "on_change")
loo_heur_U <- loo(mod_heur_U, cores = 1)
ce_heur_U <- conditional_effects(mod_heur_U, effects = "soil_0_20_P_CO2:crop")
params_heur_U <- summary(mod_heur_U)$fixed
r2_heur_U <- list(conditional = bayes_R2(mod_heur_U), marginal = bayes_R2(mod_heur_U, re_formula = NA))
rmse_heur_U <- get_rmse(mod_heur_U, d_brms$annual_P_uptake)
rm(mod_heur_U); gc()

## ----fit-heuristic-AAE10------------------------------------------------------
# Yield Heuristic AAE10: Michaelis-Menten with 6 pedoclimatic covariates on Kbase
bform_Y_heur_aae <- bf(
  annual_yield_mp_DM ~ Y0 + (A - Y0) * soil_0_20_P_AAE10 / ((Kbase * exp(betapH * soil_0_20_pH_H2O + betaClay * rollMean_soil_0_20_clay + betaTemp * juvdev_temp + betaPrec * juvdev_prec + betaCorg * z_ln_Corg + betaCa * z_ln_Ca)) + soil_0_20_P_AAE10),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  Kbase ~ crop - 1,
  betapH + betaClay + betaTemp + betaPrec + betaCorg + betaCa ~ 1,
  nl = TRUE
)
mod_heur_Y_aae <- brm(bform_Y_heur_aae, data = d_brms, prior = bprior_yield[c(1:17, 21:26), ], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, control = list(adapt_delta = 0.95, max_treedepth = 12), file = "../models/heur_yield_aae", file_refit = "on_change")
loo_heur_aae <- loo(mod_heur_Y_aae, cores = 1)
ce_heur_aae <- conditional_effects(mod_heur_Y_aae, effects = "soil_0_20_P_AAE10:crop")
params_heur_aae <- summary(mod_heur_Y_aae)$fixed
r2_heur_aae <- list(conditional = bayes_R2(mod_heur_Y_aae), marginal = bayes_R2(mod_heur_Y_aae, re_formula = NA))
rmse_heur_aae <- get_rmse(mod_heur_Y_aae, d_brms$annual_yield_mp_DM)
rm(mod_heur_Y_aae); gc()

# Uptake Heuristic AAE10
bform_U_heur_aae <- bf(
  annual_P_uptake ~ (Vmax * soil_0_20_P_AAE10) / ((Kbase * exp(betaClay * rollMean_soil_0_20_clay + betapH * soil_0_20_pH_H2O + betaTemp * juvdev_temp + betaPrec * juvdev_prec + betaCorg * z_ln_Corg + betaCa * z_ln_Ca)) + soil_0_20_P_AAE10),
  Vmax ~ crop - 1 + (1 | site/year),
  Kbase ~ crop - 1,
  betaClay + betapH + betaTemp + betaPrec + betaCorg + betaCa ~ 1,
  nl = TRUE
)
mod_heur_U_aae <- brm(bform_U_heur_aae, data = d_brms, prior = bprior_uptake[c(1:2, 6:11), ], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, control = list(adapt_delta = 0.95, max_treedepth = 12), file = "../models/heur_uptake_aae", file_refit = "on_change")
loo_heur_U_aae <- loo(mod_heur_U_aae, cores = 1)
ce_heur_U_aae <- conditional_effects(mod_heur_U_aae, effects = "soil_0_20_P_AAE10:crop")
params_heur_U_aae <- summary(mod_heur_U_aae)$fixed
r2_heur_U_aae <- list(conditional = bayes_R2(mod_heur_U_aae), marginal = bayes_R2(mod_heur_U_aae, re_formula = NA))
rmse_heur_U_aae <- get_rmse(mod_heur_U_aae, d_brms$annual_P_uptake)
rm(mod_heur_U_aae); gc()


## ----fit-mechanistic----------------------------------------------------------
# Yield Mechanistic: Michaelis-Menten with 1/b, Temp, Prec on Kbase
bform_Y_mech <- bf(
  annual_yield_mp_DM ~ Y0 + (A - Y0) * soil_0_20_P_CO2 / ((Kbase * exp(betainvb * z_inv_b + betak * z_k_pred + betaN * z_fert_N + betaTemp * juvdev_temp + betaPrec * juvdev_prec)) + soil_0_20_P_CO2),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  Kbase ~ crop - 1,
  betainvb + betak + betaN + betaTemp + betaPrec ~ 1,
  nl = TRUE
)
mod_mech_Y <- brm(bform_Y_mech, data = d_brms, prior = bprior_yield[1:22, ], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, control = list(adapt_delta = 0.95, max_treedepth = 12), file = "../models/mech_yield", file_refit = "on_change")
loo_mech <- loo(mod_mech_Y, cores = 1)
ce_mech <- conditional_effects(mod_mech_Y, effects = "soil_0_20_P_CO2:crop")
params_mech <- summary(mod_mech_Y)$fixed
r2_mech <- list(conditional = bayes_R2(mod_mech_Y), marginal = bayes_R2(mod_mech_Y, re_formula = NA))
rmse_mech <- get_rmse(mod_mech_Y, d_brms$annual_yield_mp_DM)
rm(mod_mech_Y); gc()

# Uptake Mechanistic: Michaelis-Menten with 1/b, Temp, Prec on Kbase
bform_U_mech <- bf(
  annual_P_uptake ~ (Vmax * soil_0_20_P_CO2) / ((Kbase * exp(betainvb * z_inv_b + betak * z_k_pred + betaTemp * juvdev_temp + betaPrec * juvdev_prec)) + soil_0_20_P_CO2),
  Vmax ~ crop - 1 + (1 | site/year),
  Kbase ~ crop - 1,
  betainvb + betak + betaTemp + betaPrec ~ 1,
  nl = TRUE
)
mod_mech_U <- brm(bform_U_mech, data = d_brms, prior = bprior_uptake[c(1:4, 6:7), ], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, control = list(adapt_delta = 0.95, max_treedepth = 12), file = "../models/mech_uptake", file_refit = "on_change")
loo_mech_U <- loo(mod_mech_U, cores = 1)
ce_mech_U <- conditional_effects(mod_mech_U, effects = "soil_0_20_P_CO2:crop")
params_mech_U <- summary(mod_mech_U)$fixed
r2_mech_U <- list(conditional = bayes_R2(mod_mech_U), marginal = bayes_R2(mod_mech_U, re_formula = NA))
rmse_mech_U <- get_rmse(mod_mech_U, d_brms$annual_P_uptake)
rm(mod_mech_U); gc()


## ----extract-metrics----------------------------------------------------------
cat("Extracting LOO metrics for direct comparison...\n")

# Direct Stacked Predictive Comparison
comp_yield <- loo_compare(loo_base, loo_base_aae, loo_null, loo_heur, loo_heur_aae, loo_mech)
comp_uptake <- loo_compare(loo_base_U, loo_base_U_aae, loo_null_U, loo_heur_U, loo_heur_U_aae, loo_mech_U)

# Export exactly what we need for the paper
export_payload <- list(
    yield = list(
        comparison = comp_yield,
        plot_data = list(null = ce_null[[1]], heur = ce_heur[[1]], heur_aae = ce_heur_aae[[1]], mech = ce_mech[[1]]),
        parameters = list(mech = params_mech, heur = params_heur, heur_aae = params_heur_aae),
        r2_base = r2_base,
        r2_base_aae = r2_base_aae,
        r2_null = r2_null,
        r2_mech = r2_mech,
        r2_heur = r2_heur,
        r2_heur_aae = r2_heur_aae,
        rmse = list(base = rmse_base, base_aae = rmse_base_aae, null = rmse_null, heur = rmse_heur, heur_aae = rmse_heur_aae, mech = rmse_mech)
    ),
    uptake = list(
        comparison = comp_uptake,
        plot_data = list(null = ce_null_U[[1]], heur = ce_heur_U[[1]], heur_aae = ce_heur_U_aae[[1]], mech = ce_mech_U[[1]]),
        parameters = list(mech = params_mech_U, heur = params_heur_U, heur_aae = params_heur_U_aae),
        r2_base = r2_base_U,
        r2_base_aae = r2_base_U_aae,
        r2_null = r2_null_U,
        r2_mech = r2_mech_U,
        r2_heur = r2_heur_U,
        r2_heur_aae = r2_heur_U_aae,
        rmse = list(base = rmse_base_U, base_aae = rmse_base_U_aae, null = rmse_null_U, heur = rmse_heur_U, heur_aae = rmse_heur_U_aae, mech = rmse_mech_U)
    )
)

saveRDS(export_payload, "../data/cluster_results.rds")
cat("SUCCESS. cluster_results.rds has been generated.\n")
