## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(tidyverse)
library(brms)
library(cmdstanr)
library(loo)

if (!dir.exists("../models")) dir.create("../models")

# Load pre-cleaned data
d_brms <- readRDS("../data/D_Yield_clean.rds")

# Inject GRUD Reference Targets
crop_refs <- data.frame(
  crop = c("Wheat", "KA", "KM", "RA", "SJ", "WG", "ZR", "FR"),
  Y_ref = c(60, 450, 100, 30, 30, 60, 900, 175),
  P_up_ref = c(27, 30, 38, 24, 30, 28, 41, 52)
) |> mutate(P_conc_ref = (P_up_ref * 10) / Y_ref)

d_brms <- d_brms |>
    left_join(crop_refs, by = "crop") |>
    mutate(
      C_P = annual_P_uptake / annual_yield_mp_DM,
      crop = as.factor(crop),
      site = as.factor(site),
      year = as.factor(year)
    ) |>
    filter(is.finite(C_P), C_P > 0, !is.na(annual_yield_mp_DM), !is.na(z_inv_b_cons))

# Standard Priors
priors_linear <- c(set_prior("normal(0, 1000)", class = "b"))

bprior_yield <- c(
  prior(normal(50, 50), nlpar = "Y0", lb = 0),     
  prior(normal(100, 50), nlpar = "A", lb = 0),     
  prior(lognormal(-2, 2), nlpar = "cbase", lb = 0), 
  prior(normal(0, 1), nlpar = "betainvb"),
  prior(normal(0, 1), nlpar = "betaN"),
  prior(normal(0, 1), nlpar = "betaTemp"),
  prior(normal(0, 1), nlpar = "betaPrec"),
  prior(normal(0, 1), nlpar = "betapH"),
  prior(normal(0, 1), nlpar = "betaClay")
)

# HMC config
cores_n <- 8
chains_n <- 4
iter_n <- 2000


## ----fit-base-----------------------------------------------------------------
# Yield
mod_base_Y <- brm(annual_yield_mp_DM ~ Y_ref * Supply_class_CO2 + (1 | site/year),
    data = d_brms, prior = priors_linear, backend = "cmdstanr",
    cores = cores_n, chains = chains_n, iter = iter_n, file = "../models/base_yield")

# Uptake
mod_base_U <- brm(annual_P_uptake ~ P_up_ref * Supply_class_CO2 + (1 | site/year),
    data = d_brms, prior = priors_linear, backend = "cmdstanr",
    cores = cores_n, chains = chains_n, iter = iter_n, file = "../models/base_uptake")


## ----fit-null-----------------------------------------------------------------
# Yield Null: No pedoclimatic modifiers on the rate constant
bform_Y_null <- bf(
  annual_yield_mp_DM ~ Y0 + (A - Y0) * (1 - exp(-(cbase) * soil_0_20_P_CO2)),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  cbase ~ crop - 1,
  nl = TRUE
)

mod_null_Y <- brm(bform_Y_null, data = d_brms, prior = bprior_yield[1:3], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, iter = iter_n, 
    control = list(adapt_delta = 0.95), file = "../models/null_yield")


## ----fit-heuristic------------------------------------------------------------
# Yield Heuristic: Blindly add pH and Clay to the rate exponent
bform_Y_heur <- bf(
  annual_yield_mp_DM ~ Y0 + (A - Y0) * (1 - exp(-(cbase * exp(betapH * z_pH + betaClay * z_ln_FineTexture)) * soil_0_20_P_CO2)),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  cbase ~ crop - 1,
  betapH + betaClay ~ 1,
  nl = TRUE
)

mod_heur_Y <- brm(bform_Y_heur, data = d_brms, prior = bprior_yield[c(1:3, 8:9)], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, iter = iter_n, 
    control = list(adapt_delta = 0.95), file = "../models/heur_yield")


## ----fit-mechanistic----------------------------------------------------------
# Yield Mechanistic: The exact model from bayesian_modelling.qmd using 1/b and Temp/Prec
bform_Y_mech <- bf(
  annual_yield_mp_DM ~ Y0 + (A - Y0) * (1 - exp(-(cbase * exp(betainvb * z_inv_b_cons + betaN * z_fert_N + betaTemp * z_Temp_Mean + betaPrec * z_Prec_Anom)) * soil_0_20_P_CO2)),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  cbase ~ crop - 1,
  betainvb + betaN + betaTemp + betaPrec ~ 1,
  nl = TRUE
)

mod_mech_Y <- brm(bform_Y_mech, data = d_brms, prior = bprior_yield[1:7], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, iter = iter_n, 
    control = list(adapt_delta = 0.95), file = "../models/mech_yield")


## ----extract-metrics----------------------------------------------------------
cat("Extracting LOO metrics for direct comparison...\n")

# Run exact LOO
mod_base_Y <- add_criterion(mod_base_Y, "loo")
mod_null_Y <- add_criterion(mod_null_Y, "loo")
mod_heur_Y <- add_criterion(mod_heur_Y, "loo")
mod_mech_Y <- add_criterion(mod_mech_Y, "loo")

# Direct Stacked Predictive Comparison
comp_yield <- loo_compare(mod_base_Y, mod_null_Y, mod_heur_Y, mod_mech_Y)

# Generate lightweight Prediction Dataframes for Plotting (Marginal Effects)
cat("Generating prediction coordinates for local plotting...\n")
ce_null <- conditional_effects(mod_null_Y, effects = "soil_0_20_P_CO2")
ce_heur <- conditional_effects(mod_heur_Y, effects = "soil_0_20_P_CO2")
ce_mech <- conditional_effects(mod_mech_Y, effects = "soil_0_20_P_CO2")

# Export exactly what we need for the paper
export_payload <- list(
    yield_comparison = comp_yield,
    plot_data = list(
        null = ce_null[[1]],
        heur = ce_heur[[1]],
        mech = ce_mech[[1]]
    )
)

saveRDS(export_payload, "../data/cluster_results.rds")
cat("SUCCESS. cluster_results.rds has been generated.\n")

