source("scratch/purl_trunc.R")
vars <- c("annual_P_uptake", "z_Temp_Anom", "z_fert_N", "soil_0_20_P_CO2", "z_inv_b_agro", "z_v0")
cat("\nInf counts for all numeric formula variables in D_Long_Agro:\n")
print(sapply(D_Long_Agro[, vars], function(x) sum(is.infinite(x))))
