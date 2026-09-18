source("scratch/purl_trunc.R")
vars <- c("annual_P_uptake", "crop", "z_Temp_Anom", "z_fert_N", "soil_0_20_P_CO2", "z_inv_b_agro", "z_v0", "site", "year_f")
cat("\nNA counts for all 9 formula variables in D_Long_Agro:\n")
print(colSums(is.na(D_Long_Agro[, vars])))
