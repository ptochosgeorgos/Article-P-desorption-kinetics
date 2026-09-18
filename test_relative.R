suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
  library(performance)
})
load('gapon_aparK.RData')

molar_mass <- c(K  = 39.10, Ca = 40.08, Mg = 24.31)
valence <- c(K  = 1, Ca = 2, Mg = 2)
dilution_factor <- 10

calc_equivalents <- function(conc_mgkg, molar_mass, valence) {
  (conc_mgkg / molar_mass) * valence
}

D_main_Q <- D_main %>%
  mutate(
    Q_K = calc_equivalents(soil_0_20_K_AAE10, molar_mass["K"], valence["K"]),
    Q_Ca = calc_equivalents(soil_0_20_Ca_AAE10, molar_mass["Ca"], valence["Ca"]),
    Q_Mg = calc_equivalents(soil_0_20_Mg_AAE10, molar_mass["Mg"], valence["Mg"]),
    CEC_rest = (soil_0_20_Ca_AAE10 / 20.04) + (soil_0_20_Mg_AAE10 / 12.15),
    z_clay = as.numeric(scale(soil_0_20_clay)),
    z_pH = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
    z_Corg = as.numeric(scale(log(rollMean_soil_0_20_Corg))),
    z_CEC_rest = as.numeric(scale(log(CEC_rest)))
  )

calibration_data <- calibrate_KG(D_main_Q) %>%
  filter(!is.na(z_clay), !is.na(z_pH), !is.na(z_Corg), !is.na(z_CEC_rest))

m_KG <- lmer(log(K_G_sample) ~ z_clay + z_pH + z_Corg + z_CEC_rest + (1|site/plot_nr) + (1|year), data = calibration_data)

full_predictions <- D_main_Q %>%
  add_measured_AR_K() %>%
  filter(!is.na(z_clay), !is.na(z_pH), !is.na(z_Corg), !is.na(z_CEC_rest))

full_predictions$log_KG_pred <- predict(m_KG, newdata = full_predictions, re.form = NA, allow.new.levels = TRUE)
full_predictions$KG_pred <- exp(full_predictions$log_KG_pred)

full_predictions <- full_predictions %>%
  mutate(
    exchange_ratio = Q_K / (Q_Ca + Q_Mg),
    AR_K_gapon = (1 / KG_pred) * exchange_ratio,
    AR_K_gapon = if_else(!is.na(AR_K_gapon) & is.finite(AR_K_gapon) & AR_K_gapon > 0, AR_K_gapon, NA_real_),
    AR_K_final = coalesce(AR_K_measured, AR_K_gapon)
  )

modelling_data <- full_predictions %>%
  filter(year >= 1990, !is.na(soil_0_20_K_AAE10), soil_0_20_K_AAE10 > 0, !is.na(AR_K_final), AR_K_final > 0) %>%
  mutate(
    K_buffer_capacity = KG_pred * (Q_Ca + Q_Mg),
    z_b = as.numeric(scale(log(K_buffer_capacity))),
    z_I = as.numeric(scale(log(AR_K_final)))
  ) %>%
  filter(!is.na(K_buffer_capacity), K_buffer_capacity > 0)

biological_data <- modelling_data %>%
  group_by(site, year, crop) %>%
  mutate(
    max_yield = max(annual_yield_mp_DM, na.rm = TRUE),
    relative_yield = if_else(max_yield > 0, annual_yield_mp_DM / max_yield, NA_real_),
    max_uptake = max(annual_K_uptake, na.rm = TRUE),
    relative_uptake = if_else(max_uptake > 0, annual_K_uptake / max_uptake, NA_real_)
  ) %>%
  ungroup() %>%
  filter(!is.na(crop))

yield_data <- biological_data %>% filter(!is.na(relative_yield), is.finite(relative_yield))

# Models avoiding AR_K * b
m_ry_base <- lmer(relative_yield ~ z_I + (1|site/year), data = yield_data)
# Additive main effects, with crop varying slopes
m_ry_buffer <- lmer(relative_yield ~ z_I + z_b + (0 + z_I + z_b | crop) + (1|site/year), data = yield_data)
# Alternative: simple additive without crop random effects (in case the above is singular)
m_ry_simple <- lmer(relative_yield ~ z_I + z_b + (1|site/year), data = yield_data)

cat("RELATIVE YIELD MODELS:\n")
print(compare_performance(m_ry_base, m_ry_buffer, m_ry_simple))

uptake_data <- biological_data %>% filter(!is.na(relative_uptake), is.finite(relative_uptake))
m_ru_base <- lmer(relative_uptake ~ z_I + (1|site/year), data = uptake_data)
m_ru_buffer <- lmer(relative_uptake ~ z_I + z_b + (0 + z_I + z_b | crop) + (1|site/year), data = uptake_data)
m_ru_simple <- lmer(relative_uptake ~ z_I + z_b + (1|site/year), data = uptake_data)

cat("\nRELATIVE UPTAKE MODELS:\n")
print(compare_performance(m_ru_base, m_ru_buffer, m_ru_simple))
