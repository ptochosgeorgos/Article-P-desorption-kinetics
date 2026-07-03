source("notebooks/qi_modelling1.R")
library(ggplot2)

D_emp <- D_ready |> filter(!is.na(rollMean_soil_0_20_FineTexture), !is.na(soil_0_20_P_AAE10), !is.na(soil_0_20_P_CO2)) |>
    mutate(
        Danger = ifelse(soil_0_20_P_CO2 >= 1.0, "Danger (>= 1.0 mg/L)", "Safe (< 1.0 mg/L)")
    )

cat("Number of Danger plots:", sum(D_emp$Danger == "Danger (>= 1.0 mg/L)"), "\n")
cat("Number of Safe plots:", sum(D_emp$Danger == "Safe (< 1.0 mg/L)"), "\n")

# Check correlation of P_AAE10 vs FineTexture for the Danger plots
D_danger <- D_emp |> filter(soil_0_20_P_CO2 >= 1.0)
if(nrow(D_danger) > 0) {
    cat("Correlation of P_AAE10 vs FineTexture in Danger plots:", cor(D_danger$soil_0_20_P_AAE10, D_danger$rollMean_soil_0_20_FineTexture), "\n")
}

