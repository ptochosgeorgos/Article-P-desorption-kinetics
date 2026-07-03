source("notebooks/qi_modelling1.R")
library(dplyr)

cat("Total original plots in D_ready:\n")
print(table(D_ready$site))

cat("\nChecking for CAD and REC in D_ready:\n")
cad_rec <- D_ready |> filter(site %in% c("CAD", "REC"))
cat("Number of rows for CAD:", sum(D_ready$site == "CAD"), "\n")
cat("Number of rows for REC:", sum(D_ready$site == "REC"), "\n")

# Check NA values in covariates for CAD and REC
if(nrow(cad_rec) > 0) {
    cat("\nMissing Covariates for CAD and REC:\n")
    missing_summary <- cad_rec |> group_by(site) |> summarise(
        missing_FineTexture = sum(is.na(z_ln_FineTexture)),
        missing_pH = sum(is.na(z_pH)),
        missing_yield = sum(is.na(Relative_Yield)),
        missing_Corg = sum(is.na(z_ln_Corg)),
        missing_Ca = sum(is.na(z_ln_Ca)),
        missing_P_CO2 = sum(is.na(soil_0_20_P_CO2)),
        missing_P_AAE10 = sum(is.na(soil_0_20_P_AAE10)),
        total = n()
    )
    print(missing_summary)
}

# Check where they get dropped in D_ptf_agro
cat("\nTotal plots in D_ptf_agro (used for PTF):\n")
print(table(D_ptf_agro$site))

# Check where they get dropped in D_Yield
cat("\nTotal plots in D_Yield (used for Yield Models):\n")
print(table(D_Yield$site))

# If CAD and REC are in D_ready but get dropped in D_Yield, why?
# D_Yield drops NAs for Yield models: Relative_Yield, z_pH, z_inv_b, etc.

