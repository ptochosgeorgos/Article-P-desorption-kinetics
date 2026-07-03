source("notebooks/qi_modelling1.R")
cat("\n\n--- MARGINAL RMSE ---\n")
cat(round(sqrt(mean(residuals(m_yield_nlme, level = 0)^2)), 3), "\n")
