library(lme4)
library(dplyr)
df <- readRDS("data/Final_Models_Data.rds")$data$D_Long_Agro

df <- df |>
  mutate(
    v_co2 = case_when(
      soil_0_20_P_CO2 < 0.6 ~ 1.5,
      soil_0_20_P_CO2 >= 0.6 & soil_0_20_P_CO2 <= 1.0 ~ 1.2,
      soil_0_20_P_CO2 > 1.0 & soil_0_20_P_CO2 <= 1.5 ~ 1.0,
      soil_0_20_P_CO2 > 1.5 & soil_0_20_P_CO2 <= 2.5 ~ 0.8,
      soil_0_20_P_CO2 > 2.5 ~ 0.0
    ),
    v_aae = case_when(
      rollMean_soil_0_20_P_AAE10 < 15 ~ 1.5,
      rollMean_soil_0_20_P_AAE10 >= 15 & rollMean_soil_0_20_P_AAE10 <= 25 ~ 1.2,
      rollMean_soil_0_20_P_AAE10 > 25 & rollMean_soil_0_20_P_AAE10 <= 35 ~ 1.0,
      rollMean_soil_0_20_P_AAE10 > 35 & rollMean_soil_0_20_P_AAE10 <= 55 ~ 0.8,
      rollMean_soil_0_20_P_AAE10 > 55 ~ 0.0
    )
  )

df <- df[!is.na(df$Relative_Uptake) & !is.na(df$v_co2) & !is.na(df$v_aae), ]

cat("--- Genuine GRUD Baseline (Actual Pre-Normalized Data) ---\n")

lm_co2 <- lm(Relative_Uptake ~ factor(v_co2), data=df)
lm_aae <- lm(Relative_Uptake ~ factor(v_aae), data=df)

cat(sprintf("CO2 Classes R-squared: %.3f\n", summary(lm_co2)$r.squared))
cat(sprintf("AAE10 Classes R-squared: %.3f\n", summary(lm_aae)$r.squared))

# Let's check interaction
lm_co2_int <- lm(Relative_Uptake ~ factor(v_co2) * crop, data=df)
cat(sprintf("CO2 Classes * Crop Interaction R-squared: %.3f\n", summary(lm_co2_int)$r.squared))
