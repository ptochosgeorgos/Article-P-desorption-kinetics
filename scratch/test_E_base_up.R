library(nlme)
library(MuMIn)

final_artifacts <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_artifacts$data$D_Long_Agro
D_Long_Agro$crop <- droplevels(as.factor(D_Long_Agro$crop))
n_crops <- length(levels(D_Long_Agro$crop))

cat("Fitting original model...\n")
m_up_old <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) / 
                      ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + soil_0_20_P_CO2),
    data = D_Long_Agro,
    fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop, beta_invb ~ 1),
    random = U_base ~ 1 | site / plot_nr,
    start = c(0.68, -0.03, 0.1, 0.1, median(D_Long_Agro$soil_0_20_P_CO2), rep(0, n_crops - 1), 0),
    control = nlmeControl(maxIter = 1000)
)

cat("Fitting new model with E_base ~ 1...\n")
m_up_new <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * (soil_0_20_P_CO2 + E_base)) / 
                      ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + (soil_0_20_P_CO2 + E_base)),
    data = D_Long_Agro,
    fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop, beta_invb ~ 1, E_base ~ 1),
    random = U_base ~ 1 | site / plot_nr,
    start = c(0.68, -0.03, 0.1, 0.1, median(D_Long_Agro$soil_0_20_P_CO2), rep(0, n_crops - 1), 0, 0.5),
    control = nlmeControl(maxIter = 1000)
)

cat("\n--- COMPARISON ---\n")
cat("Old AIC:", AIC(m_up_old), "\n")
cat("New AIC:", AIC(m_up_new), "\n")

cat("\nOld RMSE:", sqrt(mean(resid(m_up_old)^2)), "\n")
cat("New RMSE:", sqrt(mean(resid(m_up_new)^2)), "\n")

cat("\nNew Model Fixed Effects:\n")
print(fixef(m_up_new))

cat("\nANOVA for new model:\n")
print(anova(m_up_new))
