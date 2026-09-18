source("scratch/purl_trunc.R")
mf <- model.frame(
    annual_P_uptake ~ z_Temp_Anom + z_fert_N + soil_0_20_P_CO2 + z_inv_b_agro + z_v0 + crop + site + year_f,
    data = D_Long_Agro,
    na.action = na.fail
)
cat("model.frame SUCCESS!\n")
