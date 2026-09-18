suppressMessages(library(dplyr))
suppressMessages(library(lme4))
suppressMessages(library(performance))
load('gapon_aparK.RData')

calibration_data <- calibration_data %>%
  mutate(
    CEC_rest = (soil_0_20_Ca_AAE10 / 20.04) + (soil_0_20_Mg_AAE10 / 12.15),
    z_clay = as.numeric(scale(soil_0_20_clay)),
    z_pH = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
    z_Corg = as.numeric(scale(log(rollMean_soil_0_20_Corg))),
    z_CEC_rest = as.numeric(scale(log(CEC_rest)))
  ) %>% filter(!is.na(z_clay), !is.na(z_pH), !is.na(z_Corg), !is.na(z_CEC_rest))

m_KG <- lmer(log(K_G_sample) ~ z_clay + z_pH + z_Corg + z_CEC_rest + (1|site/plot_nr) + (1|year), data = calibration_data)
cat("K_G Model Performance:\n")
print(r2(m_KG))
print(summary(m_KG)$coefficients)

D_pred <- D_main_Q %>%
  mutate(
    CEC_rest = (soil_0_20_Ca_AAE10 / 20.04) + (soil_0_20_Mg_AAE10 / 12.15),
    z_clay = as.numeric(scale(soil_0_20_clay)),
    z_pH = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
    z_Corg = as.numeric(scale(log(rollMean_soil_0_20_Corg))),
    z_CEC_rest = as.numeric(scale(log(CEC_rest)))
  ) %>%
  add_measured_AR_K() %>%
  filter(!is.na(z_clay), !is.na(z_pH), !is.na(z_Corg), !is.na(z_CEC_rest))

D_pred$log_KG_pred <- predict(m_KG, newdata = D_pred, re.form = NA, allow.new.levels = TRUE)
D_pred$KG_pred <- exp(D_pred$log_KG_pred)

D_pred <- D_pred %>%
  mutate(
    exchange_ratio = Q_K / (Q_Ca + Q_Mg),
    AR_K_gapon_dyn = (1 / KG_pred) * exchange_ratio,
    AR_K_final_dyn = coalesce(AR_K_measured, AR_K_gapon_dyn)
  ) %>%
  filter(year >= 1990, !is.na(soil_0_20_K_AAE10), soil_0_20_K_AAE10 > 0, !is.na(AR_K_final_dyn), AR_K_final_dyn > 0)

m_K_clay_dyn <- lmer(log(soil_0_20_K_AAE10) ~ log(AR_K_final_dyn) * z_clay + (1|site/plot_nr) + (1|year), data = D_pred)
cat("\nNew m_K_clay Performance (Dynamic K_G from fixed effects only):\n")
print(r2(m_K_clay_dyn))

native_data <- D_pred %>% filter(!is.na(AR_K_measured))
m_K_clay_native <- lmer(log(soil_0_20_K_AAE10) ~ log(AR_K_measured) * z_clay + (1|site/plot_nr) + (1|year), data = native_data)
cat("\nNative m_K_clay Performance (Measured AR_K only):\n")
print(r2(m_K_clay_native))

