source("notebooks/qi_modelling1.R")
library(dplyr)

cat("\nTotal plots in D_ptf_agro (used for PTF):\n")
print(table(D_ptf_agro$site))

cat("\nTotal plots in D_Yield (used for Yield Models):\n")
print(table(D_Yield$site))

cad_reh <- D_ready |> filter(site %in% c("CAD", "REH"))
cat("\nMissing values in CAD and REH in D_ready:\n")
missing_summary <- cad_reh |> group_by(site) |> summarise(
    missing_P_AAE10 = sum(is.na(soil_0_20_P_AAE10)),
    missing_P_CO2 = sum(is.na(soil_0_20_P_CO2)),
    missing_yield_raw = sum(is.na(yield_t_ha)),
    missing_clay = sum(is.na(rollMean_soil_0_20_clay)),
    missing_Corg = sum(is.na(rollMean_soil_0_20_Corg)),
    missing_pH = sum(is.na(rollMean_soil_0_20_pH_H2O)),
    missing_Feox = sum(is.na(rollMean_soil_0_20_Feox)),
    missing_Alox = sum(is.na(rollMean_soil_0_20_Alox)),
    total = n()
)
print(missing_summary)

