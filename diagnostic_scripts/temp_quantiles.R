source("notebooks/qi_modelling1.R")
res <- abs(residuals(m_yield_nlme))
cat("\n\n--- Absolute Error Quantiles ---\n")
print(quantile(res, probs = c(0, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1)))
