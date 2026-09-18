library(dplyr)
library(lme4)

# Load data
df <- readRDS("data/Final_Models_Data.rds")$data$D_Long_Agro

# Filter complete cases
df <- df %>% 
  filter(!is.na(yield_t_ha), !is.na(P_uptake_kg_ha), !is.na(v_co2), !is.na(v_aae10))

# Create Proxy Relative Metrics (using crop mean as a stand-in for GRUD reference norm)
df <- df %>%
  group_by(crop) %>%
  mutate(
    Relative_Yield = yield_t_ha / mean(yield_t_ha, na.rm=TRUE),
    Relative_Uptake = P_uptake_kg_ha / mean(P_uptake_kg_ha, na.rm=TRUE)
  ) %>%
  ungroup()

# Fit Genuine GRUD Baseline Models
fit_ru_co2 <- lm(Relative_Uptake ~ factor(v_co2), data=df)
fit_ry_co2 <- lm(Relative_Yield ~ factor(v_co2), data=df)

cat("--- Genuine GRUD Baseline (Proxy Normalization) ---\n")
cat(sprintf("Relative Uptake (CO2) R-squared: %.3f\n", summary(fit_ru_co2)$r.squared))
cat(sprintf("Relative Yield (CO2) R-squared: %.3f\n", summary(fit_ry_co2)$r.squared))

# Also fit with interaction to see if it improves
fit_ru_co2_int <- lm(Relative_Uptake ~ factor(v_co2) * crop, data=df)
cat(sprintf("Relative Uptake (CO2 * Crop) R-squared: %.3f\n", summary(fit_ru_co2_int)$r.squared))

# Print summary of the strict model
print(summary(fit_ru_co2))
