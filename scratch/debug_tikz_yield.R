source("scratch/qi_code_subset_final.R")
library(nlme)
library(ggplot2)
library(patchwork)
library(tikzDevice)
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}","\\usetikzlibrary{calc}"))

# Fit models (quickly with fixed start values)
m_yield_raw_co2 <- tryCatch({nlme(total_yield ~ Y0 + (A - Y0) * (1 - exp(-(c_base * exp(beta_invb * z_inv_b + beta_N * z_fert_N + beta_Temp * z_Temp_Mean + beta_Prec * z_Prec_Anom) * soil_0_20_P_CO2))), data = D_Yield, fixed = list(A ~ crop, Y0 ~ crop, c_base ~ crop, beta_invb ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1), random = Y0 ~ 1 | site, weights = varPower(form = ~ soil_0_20_P_CO2), start = c(100, rep(0, length(unique(D_Yield$crop)) - 1), 10, rep(0, length(unique(D_Yield$crop)) - 1), 1.2, rep(0, length(unique(D_Yield$crop)) - 1), rep(0, 4)), control = nlmeControl(maxIter = 100))}, error=function(e) NULL)
m_yield_thm_co2 <- tryCatch({nlme(total_yield ~ Y0 + (A - Y0) * (1 - exp(-(c_base * exp(beta_invb * z_inv_b + beta_N * z_fert_N + beta_Temp * z_Temp_Mean + beta_Prec * z_Prec_Anom) * a_CO2_total_mg_L))), data = D_Yield, fixed = list(A ~ crop, Y0 ~ crop, c_base ~ crop, beta_invb ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1), random = Y0 ~ 1 | site, weights = varPower(form = ~ a_CO2_total_mg_L), start = c(100, rep(0, length(unique(D_Yield$crop)) - 1), 10, rep(0, length(unique(D_Yield$crop)) - 1), 1.2, rep(0, length(unique(D_Yield$crop)) - 1), rep(0, 4)), control = nlmeControl(maxIter = 100))}, error=function(e) NULL)
m_yield_raw_aae <- tryCatch({nlme(total_yield ~ Y0 + (A - Y0) * (1 - exp(-(c_base * exp(beta_invb * z_inv_b_cons + beta_N * z_fert_N + beta_Temp * z_Temp_Mean + beta_Prec * z_Prec_Anom) * soil_0_20_P_AAE10))), data = D_Yield, fixed = list(A ~ crop, Y0 ~ crop, c_base ~ crop, beta_invb ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1), random = Y0 ~ 1 | site, weights = varPower(form = ~ soil_0_20_P_AAE10), start = c(100, rep(0, length(unique(D_Yield$crop)) - 1), 10, rep(0, length(unique(D_Yield$crop)) - 1), 0.05, rep(0, length(unique(D_Yield$crop)) - 1), rep(0, 4)), control = nlmeControl(maxIter = 100))}, error=function(e) NULL)

D_res_all <- dplyr::bind_rows(
    D_Yield |> mutate(Model = "Raw $P_{CO2}$ (Complete)", Fitted = predict(m_yield_raw_co2), Residual = residuals(m_yield_raw_co2)),
    D_Yield |> mutate(Model = "Thermo $a_{CO2}$", Fitted = predict(m_yield_thm_co2), Residual = residuals(m_yield_thm_co2)),
    D_Yield |> mutate(Model = "Legacy $P_{AAE10}$", Fitted = predict(m_yield_raw_aae), Residual = residuals(m_yield_raw_aae))
)

p_resid <- ggplot(D_res_all, aes(x = Fitted, y = Residual, color = site)) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") + geom_point(alpha = 0.4, size = 1.5) + facet_wrap(~Model, scales = "free_x", ncol=1) + labs(title = "Conditional Residuals vs Fitted", x = "Fitted Yield", y = "Residual", color = "Site") + theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"), legend.position = "none")
p_box <- ggplot(D_res_all, aes(x = site, y = Residual, fill = site)) + geom_boxplot(alpha = 0.7, outlier.shape = 21) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") + facet_wrap(~Model, ncol=1) + labs(title = "Yield Model Residuals by Site", x = "Site", y = "Residual", fill = "Site") + theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"), legend.position = "none")

# Explicitly test tikz
options(tikzMetricsDictionary="scratch/tikzMetrics")
tikz("scratch/debug_yield.tex", width = 12, height = 8)
print(p_resid | p_box)
dev.off()
cat("Tikz compiled successfully!\n")
