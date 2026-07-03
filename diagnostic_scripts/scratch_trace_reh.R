source("notebooks/qi_modelling1.R")
library(dplyr)

reh <- D_ready |> filter(site == "REH")
cat("Total REH rows in D_ready:", nrow(reh), "\n")

cat("\nMissing Covariates in REH:\n")
missing <- reh |> summarise(
    P_AAE = sum(is.na(ln_P_AAE)),
    P_CO2 = sum(is.na(ln_P_CO2)),
    a_CO2 = sum(is.na(ln_a_CO2)),
    FineTex = sum(is.na(z_ln_FineTexture)),
    pH = sum(is.na(z_pH)),
    Ca = sum(is.na(z_ln_Ca)),
    Mg = sum(is.na(z_ln_Mg)),
    K = sum(is.na(z_ln_K)),
    Corg = sum(is.na(z_ln_Corg)),
    T_Anom = sum(is.na(z_Temp_Anom)),
    P_Anom = sum(is.na(z_Prec_Anom)),
    T_Mean = sum(is.na(z_Temp_Mean))
)
print(missing)
