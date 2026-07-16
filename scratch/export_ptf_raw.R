library(tidyverse)
library(nlme)
library(lme4)
library(jsonlite)

final_artifacts <- readRDS("data/Final_Models_Data.rds")
D_ready <- final_artifacts$data$D_Long_Agro

# Recreate D_ptf_cons
D_ptf <- D_ready |> 
    drop_na(rollMean_soil_0_20_clay, rollMean_soil_0_20_silt, soil_0_20_pH_H2O, 
            rollMean_soil_0_20_Ca_AAE10, rollMean_soil_0_20_Mg_AAE10, 
            rollMean_soil_0_20_K_AAE10, rollMean_soil_0_20_Corg,
            temp_anomaly, prec_anomaly, site_juv_temp_mean, soil_0_20_P_AAE10, soil_0_20_P_CO2) |>
    mutate(
        ln_P_AAE = log(soil_0_20_P_AAE10),
        ln_P_CO2 = log(soil_0_20_P_CO2),
        z_ln_FineTexture = as.numeric(scale(log(rollMean_soil_0_20_clay + rollMean_soil_0_20_silt))),
        z_pH = as.numeric(scale(soil_0_20_pH_H2O)),
        z_ln_Ca = as.numeric(scale(log(rollMean_soil_0_20_Ca_AAE10))),
        z_ln_Mg = as.numeric(scale(log(rollMean_soil_0_20_Mg_AAE10))),
        z_ln_K = as.numeric(scale(log(rollMean_soil_0_20_K_AAE10))),
        z_ln_Corg = as.numeric(scale(log(rollMean_soil_0_20_Corg))),
        z_Temp_Anom = as.numeric(scale(temp_anomaly)),
        z_Prec_Anom = as.numeric(scale(prec_anomaly)),
        z_Temp_Mean = as.numeric(scale(site_juv_temp_mean))
    )
D_ptf_cons <- D_ptf |> filter(soil_0_20_pH_H2O <= 7.3)

# Refit ptf_cons_raw
ptf_cons_raw <- lmer(ln_P_AAE ~ ln_P_CO2 * (z_ln_FineTexture + z_pH + z_ln_Ca + z_ln_Mg + z_ln_K + z_ln_Corg + z_Temp_Anom + z_Prec_Anom) + z_Temp_Mean + (1 | site:plot_nr), data = D_ptf_cons)

# Export to presentation/ptf_coefs.json
coefs_cons <- fixef(ptf_cons_raw)
scales_cons <- list(
    FineTexture = list(mean = mean(log(D_ptf$rollMean_soil_0_20_clay + D_ptf$rollMean_soil_0_20_silt), na.rm=TRUE), sd = sd(log(D_ptf$rollMean_soil_0_20_clay + D_ptf$rollMean_soil_0_20_silt), na.rm=TRUE)),
    Ca = list(mean = mean(log(D_ptf$rollMean_soil_0_20_Ca_AAE10), na.rm=TRUE), sd = sd(log(D_ptf$rollMean_soil_0_20_Ca_AAE10), na.rm=TRUE)),
    Mg = list(mean = mean(log(D_ptf$rollMean_soil_0_20_Mg_AAE10), na.rm=TRUE), sd = sd(log(D_ptf$rollMean_soil_0_20_Mg_AAE10), na.rm=TRUE)),
    K = list(mean = mean(log(D_ptf$rollMean_soil_0_20_K_AAE10), na.rm=TRUE), sd = sd(log(D_ptf$rollMean_soil_0_20_K_AAE10), na.rm=TRUE)),
    pH = list(mean = mean(D_ptf$soil_0_20_pH_H2O, na.rm=TRUE), sd = sd(D_ptf$soil_0_20_pH_H2O, na.rm=TRUE)),
    Corg = list(mean = mean(log(D_ptf$rollMean_soil_0_20_Corg), na.rm=TRUE), sd = sd(log(D_ptf$rollMean_soil_0_20_Corg), na.rm=TRUE)),
    Temp_Mean = list(mean = mean(D_ptf$site_juv_temp_mean, na.rm=TRUE), sd = sd(D_ptf$site_juv_temp_mean, na.rm=TRUE)),
    Temp_Anom = list(mean = mean(D_ptf$temp_anomaly, na.rm=TRUE), sd = sd(D_ptf$temp_anomaly, na.rm=TRUE)),
    Prec_Anom = list(mean = mean(D_ptf$prec_anomaly, na.rm=TRUE), sd = sd(D_ptf$prec_anomaly, na.rm=TRUE))
)

write_json(list(coefficients = as.list(coefs_cons), scales = scales_cons), "presentation/ptf_coefs.json", auto_unbox = TRUE)
cat("Exported ptf_coefs.json successfully!\n")
