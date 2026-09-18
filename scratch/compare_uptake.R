library(dplyr)
library(knitr)

res <- readRDS("data/cluster_results.rds")

# Extract performance metrics for Uptake models
perf_base <- res$uptake$performance$base
perf_heur <- res$uptake$performance$heur
perf_mech <- res$uptake$performance$mech

# Create a comparison table for performance
perf_df <- data.frame(
  Model = c("Base (GRUD)", "Heuristic (pH, Clay)", "Mechanistic (1/b, k, Temp, Prec)"),
  LOOIC = c(perf_base$looic, perf_heur$looic, perf_mech$looic),
  Marginal_R2 = c(perf_base$r2_marginal, perf_heur$r2_marginal, perf_mech$r2_marginal),
  Conditional_R2 = c(perf_base$r2_conditional, perf_heur$r2_conditional, perf_mech$r2_conditional)
)

cat("--- MODEL PERFORMANCE ---\n")
print(knitr::kable(perf_df, digits = 3))

# Extract parameters
param_heur <- as.data.frame(res$uptake$parameters$heur)
param_mech <- as.data.frame(res$uptake$parameters$mech)

# Helper to format effects
format_effects <- function(df) {
  # Keep only fixed effects starting with 'b_'
  eff <- df %>% filter(grepl("^b_", row.names(df)))
  
  res_df <- data.frame(
    Parameter = row.names(eff),
    Estimate = eff$Estimate,
    CI_lower = eff$Q2.5,
    CI_upper = eff$Q97.5
  )
  return(res_df)
}

cat("\n--- HEURISTIC EFFECTS (pH, Clay) ---\n")
print(knitr::kable(format_effects(param_heur), digits = 3))

cat("\n--- MECHANISTIC EFFECTS (1/b, k, Temp, Prec) ---\n")
print(knitr::kable(format_effects(param_mech), digits = 3))

