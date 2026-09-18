import re

with open("notebooks/cluster_bayesian_offload.R", "r") as f:
    content = f.read()

# 1. Update the filter block to include annual_P_uptake
content = content.replace(
    "!is.na(annual_yield_mp_DM),",
    "!is.na(annual_yield_mp_DM), \n      !is.na(annual_P_uptake),"
)

# 2. Add bprior_uptake below bprior_yield
bprior_uptake_str = """
bprior_uptake <- c(
  prior(normal(10, 10), nlpar = "Y0", lb = 0),     
  prior(normal(40, 20), nlpar = "A", lb = 0),     
  prior(lognormal(-2, 2), nlpar = "cbase", lb = 0), 
  prior(normal(0, 1), nlpar = "betainvb"),
  prior(normal(0, 1), nlpar = "betaN"),
  prior(normal(0, 1), nlpar = "betaTemp"),
  prior(normal(0, 1), nlpar = "betaPrec"),
  prior(normal(0, 1), nlpar = "betapH"),
  prior(normal(0, 1), nlpar = "betaClay")
)
"""
content = content.replace(
    "bprior_yield <- c(",
    bprior_uptake_str + "\nbprior_yield <- c("
)

# 3. Add Uptake models
# NULL
null_uptake = """
# Uptake Null: No pedoclimatic modifiers on the rate constant
bform_U_null <- bf(
  annual_P_uptake ~ Y0 + (A - Y0) * (1 - exp(-(cbase) * soil_0_20_P_CO2)),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  cbase ~ 1,
  nl = TRUE
)
mod_null_U <- brm(bform_U_null, data = d_brms, prior = bprior_uptake[1:3, ], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, control = list(adapt_delta = 0.95), file = "../models/null_uptake")

loo_null_U <- loo(mod_null_U, cores = 1)
ce_null_U <- conditional_effects(mod_null_U, effects = "soil_0_20_P_CO2")
rm(mod_null_U); gc()
"""

# HEUR
heur_uptake = """
# Uptake Heuristic: Blindly add pH and Clay to the rate exponent
bform_U_heur <- bf(
  annual_P_uptake ~ Y0 + (A - Y0) * (1 - exp(-(cbase * exp(betaClay * rollMean_soil_0_20_clay + betapH * soil_0_20_pH_H2O)) * soil_0_20_P_CO2)),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  cbase + betaClay + betapH ~ 1,
  nl = TRUE
)
mod_heur_U <- brm(bform_U_heur, data = d_brms, prior = bprior_uptake[c(1:3, 8:9), ], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, control = list(adapt_delta = 0.95), file = "../models/heur_uptake")

loo_heur_U <- loo(mod_heur_U, cores = 1)
ce_heur_U <- conditional_effects(mod_heur_U, effects = "soil_0_20_P_CO2")
params_heur_U <- fixef(mod_heur_U)
rm(mod_heur_U); gc()
"""

# MECH
mech_uptake = """
# Uptake Mechanistic: The exact model from bayesian_modelling.qmd using 1/b and Temp/Prec
bform_U_mech <- bf(
  annual_P_uptake ~ Y0 + (A - Y0) * (1 - exp(-(cbase * exp(betainvb * z_inv_b + betaTemp * rollMean_Temp + betaPrec * rollMean_Prec)) * soil_0_20_P_CO2)),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  cbase + betainvb + betaTemp + betaPrec ~ 1,
  nl = TRUE
)
mod_mech_U <- brm(bform_U_mech, data = d_brms, prior = bprior_uptake[1:7, ], 
    backend = "cmdstanr", cores = cores_n, chains = chains_n, threads = threading(threads_n),
    iter = iter_n, control = list(adapt_delta = 0.95), file = "../models/mech_uptake")

loo_mech_U <- loo(mod_mech_U, cores = 1)
ce_mech_U <- conditional_effects(mod_mech_U, effects = "soil_0_20_P_CO2")
params_mech_U <- fixef(mod_mech_U)
r2_mech_U <- bayes_R2(mod_mech_U)
rm(mod_mech_U); gc()
"""

# Inject after each yield model's garbage collection
content = content.replace("rm(mod_null_Y); gc()", "rm(mod_null_Y); gc()\n" + null_uptake)
content = content.replace("rm(mod_heur_Y); gc()", "rm(mod_heur_Y); gc()\n" + heur_uptake)
content = content.replace("rm(mod_mech_Y); gc()", "rm(mod_mech_Y); gc()\n" + mech_uptake)


# Update extraction pipeline
extraction_old = """
# Direct Stacked Predictive Comparison
comp_yield <- loo_compare(loo_base, loo_null, loo_heur, loo_mech)

# Export exactly what we need for the paper
export_payload <- list(
    yield_comparison = comp_yield,
    plot_data = list(
        null = ce_null[[1]],
        heur = ce_heur[[1]],
        mech = ce_mech[[1]]
    ),
    parameters = list(
        mech = params_mech,
        heur = params_heur
    ),
    r2_mech = r2_mech
)
"""

extraction_new = """
# Add loo() for base Uptake (we skipped it previously)
loo_base_U <- loo(readRDS("../models/base_uptake.rds"), cores = 1)

# Direct Stacked Predictive Comparison
comp_yield <- loo_compare(loo_base, loo_null, loo_heur, loo_mech)
comp_uptake <- loo_compare(loo_base_U, loo_null_U, loo_heur_U, loo_mech_U)

# Export exactly what we need for the paper
export_payload <- list(
    yield = list(
        comparison = comp_yield,
        plot_data = list(null = ce_null[[1]], heur = ce_heur[[1]], mech = ce_mech[[1]]),
        parameters = list(mech = params_mech, heur = params_heur),
        r2_mech = r2_mech
    ),
    uptake = list(
        comparison = comp_uptake,
        plot_data = list(null = ce_null_U[[1]], heur = ce_heur_U[[1]], mech = ce_mech_U[[1]]),
        parameters = list(mech = params_mech_U, heur = params_heur_U),
        r2_mech = r2_mech_U
    )
)
"""
content = content.replace(extraction_old.strip(), extraction_new.strip())

with open("notebooks/cluster_bayesian_offload.R", "w") as f:
    f.write(content)
