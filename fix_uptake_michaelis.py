import re

files = ["notebooks/cluster_bayesian_offload.R", "notebooks/cluster_bayesian_offload.qmd"]

for file in files:
    with open(file, "r") as f:
        content = f.read()

    # 1. Update Priors
    old_prior = """
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
    new_prior = """
bprior_uptake <- c(
  prior(normal(30, 15), nlpar = "Vmax", lb = 0),     
  prior(lognormal(1, 1), nlpar = "Kbase", lb = 0),     
  prior(normal(0, 1), nlpar = "betainvb"),
  prior(normal(0, 1), nlpar = "betaN"),
  prior(normal(0, 1), nlpar = "betaTemp"),
  prior(normal(0, 1), nlpar = "betaPrec"),
  prior(normal(0, 1), nlpar = "betapH"),
  prior(normal(0, 1), nlpar = "betaClay")
)
"""
    content = content.replace(old_prior.strip(), new_prior.strip())

    # 2. Null Uptake
    old_null = """
# Uptake Null: No pedoclimatic modifiers on the rate constant
bform_U_null <- bf(
  annual_P_uptake ~ Y0 + (A - Y0) * (1 - exp(-(cbase) * soil_0_20_P_CO2)),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  cbase ~ 1,
  nl = TRUE
)
"""
    new_null = """
# Uptake Null: Michaelis-Menten (No pedoclimatic modifiers)
bform_U_null <- bf(
  annual_P_uptake ~ (Vmax * soil_0_20_P_CO2) / (Kbase + soil_0_20_P_CO2),
  Vmax ~ crop - 1 + (1 | site/year),
  Kbase ~ crop - 1,
  nl = TRUE
)
"""
    # Fix the prior index in the brm call for null! (We only need Vmax and Kbase, which are [1:2])
    content = content.replace("bprior_uptake[1:3, ]", "bprior_uptake[1:2, ]")
    content = content.replace(old_null.strip(), new_null.strip())

    # 3. Heuristic Uptake
    old_heur = """
# Uptake Heuristic: Blindly add pH and Clay to the rate exponent
bform_U_heur <- bf(
  annual_P_uptake ~ Y0 + (A - Y0) * (1 - exp(-(cbase * exp(betaClay * rollMean_soil_0_20_clay + betapH * soil_0_20_pH_H2O)) * soil_0_20_P_CO2)),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  cbase + betaClay + betapH ~ 1,
  nl = TRUE
)
"""
    new_heur = """
# Uptake Heuristic: Michaelis-Menten with pH and Clay on Kbase
bform_U_heur <- bf(
  annual_P_uptake ~ (Vmax * soil_0_20_P_CO2) / ((Kbase * exp(betaClay * rollMean_soil_0_20_clay + betapH * soil_0_20_pH_H2O)) + soil_0_20_P_CO2),
  Vmax ~ crop - 1 + (1 | site/year),
  Kbase ~ crop - 1,
  betaClay + betapH ~ 1,
  nl = TRUE
)
"""
    # Fix the prior index for heur! (Vmax, Kbase, betapH, betaClay -> indices 1:2, 7:8)
    content = content.replace("bprior_uptake[c(1:3, 8:9), ]", "bprior_uptake[c(1:2, 7:8), ]")
    content = content.replace(old_heur.strip(), new_heur.strip())

    # 4. Mech Uptake
    old_mech = """
# Uptake Mechanistic: The exact model from bayesian_modelling.qmd using 1/b and Temp/Prec
bform_U_mech <- bf(
  annual_P_uptake ~ Y0 + (A - Y0) * (1 - exp(-(cbase * exp(betainvb * z_inv_b + betaTemp * rollMean_Temp + betaPrec * rollMean_Prec)) * soil_0_20_P_CO2)),
  Y0 ~ crop - 1 + (1 | site/year),
  A ~ crop - 1 + (1 | site/year),
  cbase + betainvb + betaTemp + betaPrec ~ 1,
  nl = TRUE
)
"""
    new_mech = """
# Uptake Mechanistic: Michaelis-Menten with 1/b, Temp, Prec on Kbase
bform_U_mech <- bf(
  annual_P_uptake ~ (Vmax * soil_0_20_P_CO2) / ((Kbase * exp(betainvb * z_inv_b + betaTemp * rollMean_Temp + betaPrec * rollMean_Prec)) + soil_0_20_P_CO2),
  Vmax ~ crop - 1 + (1 | site/year),
  Kbase ~ crop - 1,
  betainvb + betaTemp + betaPrec ~ 1,
  nl = TRUE
)
"""
    # Fix the prior index for mech! (Vmax, Kbase, betainvb, betaTemp, betaPrec -> indices 1:3, 5:6)
    # Wait, in the bprior_uptake definition above, the indices are:
    # 1: Vmax
    # 2: Kbase
    # 3: betainvb
    # 4: betaN
    # 5: betaTemp
    # 6: betaPrec
    # 7: betapH
    # 8: betaClay
    # Mech needs 1:3 and 5:6 (excluding betaN since it's not in the formula)
    content = content.replace("bprior_uptake[1:7, ]", "bprior_uptake[c(1:3, 5:6), ]")
    content = content.replace(old_mech.strip(), new_mech.strip())

    with open(file, "w") as f:
        f.write(content)
