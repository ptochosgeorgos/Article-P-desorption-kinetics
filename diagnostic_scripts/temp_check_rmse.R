source("notebooks/qi_modelling1.R")
cat("P_AAE10 Pseudo-R2:", round(cor(D_Yield$Relative_Yield, predict(m_yield_raw_aae, level = 2))^2, 3), "\n")
cat("P_AAE10 RMSE_Cond:", round(sqrt(mean(residuals(m_yield_raw_aae, level = 2)^2)), 3), "\n")
cat("P_AAE10 RMSE_Marg:", round(sqrt(mean(residuals(m_yield_raw_aae, level = 0)^2)), 3), "\n")
