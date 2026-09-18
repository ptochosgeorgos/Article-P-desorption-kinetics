suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(readxl))

climate_data <- readRDS("data/all_P.rds") |> dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec) |> dplyr::distinct()
D2 <- read_excel("data/STYCS_data_2023_260511.xlsx") |> rename(rep = replicate) |> mutate(site = gsub("STYCS_", "", LtE_name)) |> left_join(climate_data, by = c("site", "year"))

D_ready <- D2 |> mutate(
    ln_P_AAE = log(soil_0_20_P_AAE10),
    ln_P_CO2 = log(soil_0_20_P_test * 0.155),
    z_ln_FineTexture = as.numeric(scale(log(rollMean_soil_0_20_clay + rollMean_soil_0_20_silt))),
    z_pH = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
    z_ln_Ca = as.numeric(scale(log(rollMean_soil_0_20_Ca_AAE10))),
    z_ln_Mg = as.numeric(scale(log(rollMean_soil_0_20_Mg_AAE10))),
    z_ln_K = as.numeric(scale(log(rollMean_soil_0_20_K_AAE10))),
    z_ln_Corg = as.numeric(scale(log(rollMean_soil_0_20_Corg))),
    z_Temp_Anom = as.numeric(scale(juvdev_temp)),
    z_Prec_Anom = as.numeric(scale(juvdev_prec)),
    z_Temp_Mean = as.numeric(scale(anavg_temp))
)

D_ptf <- D_ready |> tidyr::drop_na(ln_P_AAE, ln_P_CO2, z_ln_FineTexture, z_pH, z_ln_Ca, z_ln_Mg, z_ln_K, z_ln_Corg, z_Temp_Anom, z_Prec_Anom, z_Temp_Mean)
cat("D_ptf rows:", nrow(D_ptf), "\n")
