# Sandbox script to test Absolute Yield vs Normalized Yield (max)
# Ensure you have your 'D_Yield' or 'D_ready' object loaded in your environment before running this.

library(nlme)
library(dplyr)

# 1. Prepare the dataset (assuming D_ready is in your environment)
# We calculate a NEW normalization column: Y_rel_max using the absolute max of P166
D_Test <- D_ready %>%
  group_by(site_ID, crop, year) %>%
  mutate(
    # The new robust baseline: The true physical ceiling achieved that year
    ref_yield_max = max(total_yield[treatment_ID == "P166"], na.rm = TRUE),
    
    # Normalized Yield
    Y_rel_max = total_yield / ref_yield_max
  ) %>%
  ungroup() %>%
  # Filter out NA kinetics just like in the main script
  filter(!is.na(k), !is.na(soil_0_20_P_CO2))

n_crops <- length(levels(D_Test$crop))

# ==============================================================================
# MODEL 1: The Raw Absolute Yield Model (No Normalization)
# We must estimate BOTH the absolute ceiling (Y_max) and the slope (c_base) for EVERY crop.
# WARNING: This will likely crash or fail to converge because it introduces 
# >30 fixed effect parameters into a non-linear optimization space.
# ==============================================================================
cat("\nFitting Model 1: Absolute Yield (No Normalization)...\n")
tryCatch({
  m_yield_absolute <- nlme(
      total_yield ~ Y_max * (1 - exp(-(c_base * exp(
          beta_invb * z_inv_b +
          beta_pH * z_pH +
          beta_Temp * z_Temp_Mean
      ) * (soil_0_20_P_CO2 + E_base)))),
      data = D_Test,
      # Y_max AND c_base must vary by crop!
      fixed = list(Y_max ~ crop, c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_Temp ~ 1, E_base ~ 1),
      random = Y_max ~ 1 | site/year_f,
      # Starting values are extremely difficult to guess for raw yield across 15 crops
      start = c(rep(10, n_crops), rep(0.1, n_crops), 0, 0, 0, 0),
      control = nlmeControl(maxIter = 2000, returnObject = TRUE)
  )
  print(summary(m_yield_absolute))
}, error = function(e) { cat("Model 1 crashed:\n", e$message, "\n") })


# ==============================================================================
# MODEL 2: The Max-Normalized Yield Model
# The ceiling is mathematically collapsed to 1.0. We only estimate the slope (c_base).
# ==============================================================================
cat("\nFitting Model 2: Max-Normalized Yield (Y_rel_max)...\n")
tryCatch({
  m_yield_norm_max <- nlme(
      Y_rel_max ~ 1 - exp(-(c_base * exp(
          beta_invb * z_inv_b +
          beta_pH * z_pH +
          beta_Temp * z_Temp_Mean
      ) * (soil_0_20_P_CO2 + E_base))),
      data = D_Test,
      # Only c_base varies by crop! Massive reduction in parameters.
      fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_Temp ~ 1, E_base ~ 1),
      random = c_base ~ 1 | site/year_f,
      start = c(rep(0.1, n_crops), 0, 0, 0, 0),
      control = nlmeControl(maxIter = 2000, returnObject = TRUE)
  )
  print(summary(m_yield_norm_max))
}, error = function(e) { cat("Model 2 crashed:\n", e$message, "\n") })
