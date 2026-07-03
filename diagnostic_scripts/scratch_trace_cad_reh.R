source("notebooks/qi_modelling1.R")
library(dplyr)

cad_reh <- D_ready |> filter(site %in% c("CAD", "REH"))

cat("Total CAD rows:", sum(cad_reh$site == "CAD"), "\n")
cat("Total REH rows:", sum(cad_reh$site == "REH"), "\n")

cat("\nYears available for CAD:\n")
print(table(cad_reh$year[cad_reh$site == "CAD"]))

cat("\nYears available for REH:\n")
print(table(cad_reh$year[cad_reh$site == "REH"]))

cat("\nMissing fert_N_tot for CAD (from 2010 onwards):\n")
print(sum(is.na(cad_reh$fert_N_tot[cad_reh$site == "CAD" & cad_reh$year >= 2010])))

cat("\nMissing fert_N_tot for REH (from 2010 onwards):\n")
print(sum(is.na(cad_reh$fert_N_tot[cad_reh$site == "REH" & cad_reh$year >= 2010])))

cat("\nMissing annual_yield_mp_DM for CAD (from 2010 onwards):\n")
print(sum(is.na(cad_reh$annual_yield_mp_DM[cad_reh$site == "CAD" & cad_reh$year >= 2010])))

cat("\nMissing annual_yield_mp_DM for REH (from 2010 onwards):\n")
print(sum(is.na(cad_reh$annual_yield_mp_DM[cad_reh$site == "REH" & cad_reh$year >= 2010])))

# What happens if we do the exact filter D_Yield does?
cad_yield_test <- cad_reh |>
    filter(year >= 2010, !is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot))

cat("\nRows surviving the initial filter (year>=2010 & P & N_fert) for CAD/REH:\n")
print(table(cad_yield_test$site))

