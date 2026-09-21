# Calculate 30-year cumulative P-Balance metrics
library(tidyverse)
library(brms)

# 1. Load Data
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

grud_aae_matrix <- matrix(c(
  1.5, 1.5, 1.5, 1.4, 1.4,
  1.5, 1.5, 1.4, 1.4, 1.2,
  1.5, 1.4, 1.4, 1.2, 1.2,
  1.4, 1.4, 1.2, 1.2, 1.2,
  1.4, 1.2, 1.2, 1.2, 1.0,
  1.2, 1.2, 1.2, 1.0, 1.0,
  1.2, 1.2, 1.0, 1.0, 1.0,
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

crop_refs <- data.frame(
  crop = c("Wheat", "KA", "KM", "RA", "SJ", "WG", "ZR", "FR"),
  Y_ref = c(60, 450, 100, 30, 30, 60, 900, 175),
  P_up_ref = c(27, 30, 38, 24, 30, 28, 41, 52)
) |> mutate(P_conc_ref = (P_up_ref * 10) / Y_ref)

d_brms <- d_brms |>
    mutate(crop = case_when(
      crop %in% c("WW", "SW", "WS") ~ "Wheat",
      TRUE ~ as.character(crop)
    )) |>
    left_join(crop_refs, by = "crop") |>
    mutate(
      C_P = annual_P_uptake / annual_yield_mp_DM,
      z_k_pred = scale(ln_K_pred_agro)[, 1],
      Class_CO2 = get_grud_co2_vec(soil_0_20_P_CO2, rollMean_soil_0_20_clay),
      Class_AAE = get_grud_aae_vec(soil_0_20_P_AAE10, rollMean_soil_0_20_clay),
      crop = as.factor(crop),
      site = as.factor(site),
      year = as.factor(year)
    )

# 2. Convert normative P2O5 requirement to P (kg/ha)
d_brms$P_up_norm <- d_brms$P_up_ref # Use the hardcoded GRUD reference uptake (0 NAs) instead of fert_P2O5_NORM

# True annual P balance
d_brms$annual_P_bal_true <- d_brms$fert_P_tot - d_brms$annual_P_uptake

# GRUD Base Models (Deterministic, based on norms and numeric multipliers from get_grud_vec)
d_brms$annual_P_bal_pred_CO2 <- d_brms$P_up_norm * (d_brms$Class_CO2 - 1)
d_brms$annual_P_bal_pred_AAE10 <- d_brms$P_up_norm * (d_brms$Class_AAE - 1)


# 3. Predict from Bayesian models
mod_heur_U <- readRDS("../models/heur_uptake.rds")
mod_heur_U_aae <- readRDS("../models/heur_uptake_aae.rds")
mod_mech_U <- readRDS("../models/mech_uptake.rds")

# SAFEGUARD 1: Force-drop any crops that were not in the training data
allowed_crops <- unique(as.character(mod_heur_U$data$crop))
d_brms <- d_brms[as.character(d_brms$crop) %in% allowed_crops, ]

# SAFEGUARD 2: Completely strip unused factor levels from ALL factors in the dataset
d_brms <- droplevels(d_brms)

# SAFEGUARD 3: allow_new_levels = TRUE to handle any remaining site/year mismatch in random effects
pred_heur <- predict(mod_heur_U, newdata = d_brms, allow_new_levels = TRUE)
pred_heur_aae <- predict(mod_heur_U_aae, newdata = d_brms, allow_new_levels = TRUE)
pred_mech <- predict(mod_mech_U, newdata = d_brms, allow_new_levels = TRUE)

d_brms$annual_P_bal_pred_heur <- d_brms$fert_P_tot - pred_heur[, 'Estimate']
d_brms$annual_P_bal_pred_heur_aae <- d_brms$fert_P_tot - pred_heur_aae[, 'Estimate']
d_brms$annual_P_bal_pred_mech <- d_brms$fert_P_tot - pred_mech[, 'Estimate']

rm(mod_heur_U, mod_heur_U_aae, mod_mech_U); gc()

# 4. Aggregate cumulatively to 30-year balances per plot
pbal_agg <- d_brms %>%
  group_by(site, plot_nr) %>%
  summarise(
    Cum_P_bal_true = sum(annual_P_bal_true, na.rm = TRUE),
    Cum_P_bal_CO2 = sum(annual_P_bal_pred_CO2, na.rm = TRUE),
    Cum_P_bal_AAE10 = sum(annual_P_bal_pred_AAE10, na.rm = TRUE),
    Cum_P_bal_Heur = sum(annual_P_bal_pred_heur, na.rm = TRUE),
    Cum_P_bal_Heur_AAE = sum(annual_P_bal_pred_heur_aae, na.rm = TRUE),
    Cum_P_bal_Mech = sum(annual_P_bal_pred_mech, na.rm = TRUE),
    .groups = 'drop'
  )

# 5. Compute R2 and RMSE for each approach
calc_metrics <- function(true, pred) {
  valid <- !is.na(true) & !is.na(pred)
  t_val <- true[valid]
  p_val <- pred[valid]
  rmse <- sqrt(mean((t_val - p_val)^2))
  r2 <- 1 - sum((t_val - p_val)^2) / sum((t_val - mean(t_val))^2)
  return(list(RMSE = rmse, R2 = r2))
}

res_pbal <- list(
  CO2 = calc_metrics(pbal_agg$Cum_P_bal_true, pbal_agg$Cum_P_bal_CO2),
  AAE10 = calc_metrics(pbal_agg$Cum_P_bal_true, pbal_agg$Cum_P_bal_AAE10),
  Heur = calc_metrics(pbal_agg$Cum_P_bal_true, pbal_agg$Cum_P_bal_Heur),
  Heur_AAE = calc_metrics(pbal_agg$Cum_P_bal_true, pbal_agg$Cum_P_bal_Heur_AAE),
  Mech = calc_metrics(pbal_agg$Cum_P_bal_true, pbal_agg$Cum_P_bal_Mech),
  data = pbal_agg
)

saveRDS(res_pbal, '../data/pbalance_results.rds')
print('P-Balance evaluation completed and saved to pbalance_results.rds')
