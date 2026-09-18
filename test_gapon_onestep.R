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
  filter(!is.na(z_clay), !is.na(z_pH), !is.na(z_Corg), !is.na(z_CEC_rest), !is.na(AR_K_measured), AR_K_measured > 0) %>%
  mutate(exchange_ratio = Q_K / (Q_Ca + Q_Mg))

cat("FITTING ONE-STEP GAPON MODEL...\n")
m_gapon_one_step <- lmer(
  exchange_ratio ~ 0 + AR_K_measured + AR_K_measured:(z_clay + z_pH + z_Corg + z_CEC_rest) + 
  (0 + AR_K_measured | site/year), 
  data = calibration_data
)

print(summary(m_gapon_one_step))
print(performance(m_gapon_one_step))
