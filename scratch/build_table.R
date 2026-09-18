library(knitr)
res <- readRDS("data/cluster_results.rds")
get_metric <- function(val, rnd = 2) {
  if (is.null(val)) return("-")
  sprintf(paste0("%.", rnd, "f"), val)
}
comp <- res$yield$comparison
looic_val <- function(m_name) {
  if (is.null(comp) || !(m_name %in% comp$model)) return("-")
  sprintf("%.0f", comp$looic[comp$model == m_name])
}
ploo_val <- function(m_name) {
  if (is.null(comp) || !(m_name %in% comp$model)) return("-")
  sprintf("%.1f", comp$p_loo[comp$model == m_name])
}
r2_cond <- function(r2_obj) {
  if (is.null(r2_obj)) return("-")
  sprintf("%.3f", r2_obj$conditional["R2", "Estimate"])
}
r2_marg <- function(r2_obj) {
  if (is.null(r2_obj)) return("-")
  sprintf("%.3f", r2_obj$marginal["R2", "Estimate"])
}
df_yield <- data.frame(
  Model = c("**Base Model CO2**", "**Base Model AAE10**", "**Null Model**", "**Heuristic CO2**", "**Heuristic AAE10**", "**Mechanistic**"),
  Substrate = c("Supply_class_CO2", "Supply_class_AAE10", "$P_{CO_2}$", "$P_{CO_2}$", "$P_{AAE10}$", "$P_{CO_2}$"),
  `K_base Modulators` = c("*(None)*", "*(None)*", "*(None)*", "pH, Clay, Corg, Ca, Temp, Prec", "pH, Clay, Corg, Ca, Temp, Prec", "1/b, k, N, Temp, Prec"),
  RMSE = c(get_metric(res$yield$rmse$base), get_metric(res$yield$rmse$base_aae), get_metric(res$yield$rmse$null), get_metric(res$yield$rmse$heur), get_metric(res$yield$rmse$heur_aae), get_metric(res$yield$rmse$mech)),
  `R2_Marg` = c(r2_marg(res$yield$r2_base), r2_marg(res$yield$r2_base_aae), r2_marg(res$yield$r2_null), r2_marg(res$yield$r2_heur), r2_marg(res$yield$r2_heur_aae), r2_marg(res$yield$r2_mech)),
  `R2_Cond` = c(r2_cond(res$yield$r2_base), r2_cond(res$yield$r2_base_aae), r2_cond(res$yield$r2_null), r2_cond(res$yield$r2_heur), r2_cond(res$yield$r2_heur_aae), r2_cond(res$yield$r2_mech)),
  LOOIC = c(looic_val("mod_base_Y"), looic_val("mod_base_Y_aae"), looic_val("mod_null_Y"), looic_val("mod_heur_Y"), looic_val("mod_heur_Y_aae"), looic_val("mod_mech_Y")),
  p_loo = c(ploo_val("mod_base_Y"), ploo_val("mod_base_Y_aae"), ploo_val("mod_null_Y"), ploo_val("mod_heur_Y"), ploo_val("mod_heur_Y_aae"), ploo_val("mod_mech_Y")),
  check.names = FALSE
)
cat(kable(df_yield, format = "markdown", col.names = c("Model", "Substrate", "$K_{base}$ Modulators", "RMSE", "$R^2_{Marg}$", "$R^2_{Cond}$", "LOOIC", "$p_{loo}$")))
cat("\n")
