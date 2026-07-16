library(dplyr)
D <- readRDS("data/RES.rds")$D
# Filter out missing k and select predictors
kin_data <- D |> filter(!is.na(k)) |> 
    select(site, treatment_ID, k, Feox, Alox, soil_0_20_pH_H2O, soil_0_20_Corg, rollMean_soil_0_20_clay, rollMean_soil_0_20_silt) |>
    na.omit()

# Scale predictors for comparable coefficients
kin_data <- kin_data |> mutate(
    Feox_s = scale(Feox),
    Alox_s = scale(Alox),
    pH_s = scale(soil_0_20_pH_H2O),
    Corg_s = scale(soil_0_20_Corg),
    Clay_s = scale(rollMean_soil_0_20_clay),
    Silt_s = scale(rollMean_soil_0_20_silt)
)

# Model 1: Feox and Alox
m1 <- lm(k ~ Feox_s + Alox_s, data = kin_data)
print("--- Geochemical Predictors ---")
print(summary(m1))

# Model 2: Basic Agronomic
m2 <- lm(k ~ pH_s + Corg_s + Clay_s, data = kin_data)
print("--- Agronomic Predictors ---")
print(summary(m2))

# Model 3: Combined
m3 <- lm(k ~ Feox_s + Alox_s + pH_s + Corg_s + Clay_s, data = kin_data)
print("--- Combined Predictors ---")
print(summary(m3))

# Relative importance / R-squared
cat("M1 (Geo) R2:", summary(m1)$adj.r.squared, "\n")
cat("M2 (Agro) R2:", summary(m2)$adj.r.squared, "\n")
cat("M3 (Combined) R2:", summary(m3)$adj.r.squared, "\n")

