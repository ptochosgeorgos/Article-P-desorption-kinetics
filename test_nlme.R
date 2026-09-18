suppressPackageStartupMessages({
  library(dplyr)
  library(nlme)
  library(lme4)
})
load('gapon_aparK.RData')

molar_mass <- c(K  = 39.10, Ca = 40.08, Mg = 24.31)
valence <- c(K  = 1, Ca = 2, Mg = 2)

calc_equivalents <- function(conc, mw, z) { (conc / mw) * z }

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
  filter(!is.na(z_clay), !is.na(z_pH), !is.na(z_Corg), !is.na(z_CEC_rest), !is.na(AR_K_measured), AR_K_measured > 0) %>%
  mutate(exchange_ratio = Q_K / (Q_Ca + Q_Mg))

m_gapon_one_step <- lmer(
  exchange_ratio ~ 0 + AR_K_measured + AR_K_measured:(z_clay + z_pH + z_Corg + z_CEC_rest) + 
  (0 + AR_K_measured | site/year), 
  data = calibration_data
)

full_predictions <- D_main_Q %>%
  add_measured_AR_K() %>%
  filter(!is.na(z_clay), !is.na(z_pH), !is.na(z_Corg), !is.na(z_CEC_rest)) %>%
  mutate(exchange_ratio = Q_K / (Q_Ca + Q_Mg))

coefs <- fixef(m_gapon_one_step)

full_predictions <- full_predictions %>%
  mutate(
    KG_pred = coefs["AR_K_measured"] +
              coefs["AR_K_measured:z_clay"] * z_clay +
              coefs["AR_K_measured:z_pH"] * z_pH +
              coefs["AR_K_measured:z_Corg"] * z_Corg +
              coefs["AR_K_measured:z_CEC_rest"] * z_CEC_rest,
              
    AR_K_gapon = (1 / KG_pred) * exchange_ratio,
    AR_K_gapon = if_else(!is.na(AR_K_gapon) & is.finite(AR_K_gapon) & AR_K_gapon > 0, AR_K_gapon, NA_real_),
    AR_K_final = coalesce(AR_K_measured, AR_K_gapon)
  )

modelling_data <- full_predictions %>%
  filter(year >= 1990, !is.na(soil_0_20_K_AAE10), soil_0_20_K_AAE10 > 0, !is.na(AR_K_final), AR_K_final > 0) %>%
  mutate(
    K_buffer_capacity = KG_pred * (Q_Ca + Q_Mg),
    z_b = as.numeric(scale(log(K_buffer_capacity))),
    z_AR_K = as.numeric(scale(log(AR_K_final)))
  ) %>%
  filter(!is.na(K_buffer_capacity), K_buffer_capacity > 0)

biological_data <- modelling_data %>%
  group_by(site, year, crop) %>%
  mutate(
    max_yield = max(annual_yield_mp_DM, na.rm = TRUE),
    relative_yield = if_else(max_yield > 0, annual_yield_mp_DM / max_yield, NA_real_)
  ) %>%
  ungroup() %>%
  filter(!is.na(crop))

yield_data <- biological_data %>% filter(!is.na(relative_yield), is.finite(relative_yield))

cat("Fitting NLME Mitscherlich model...\n")
tryCatch({
  m_nlme <- nlme(
    relative_yield ~ 1 - exp(-(c_I * z_AR_K + c_b * z_b + E)),
    data = yield_data,
    fixed = c_I + c_b + E ~ 1,
    random = E ~ 1 | site/year,
    start = c(c_I = 0.1, c_b = 0.1, E = 1)
  )
  print(summary(m_nlme))
}, error = function(e) {
  cat("Error in nested NLME:", conditionMessage(e), "\n")
})
