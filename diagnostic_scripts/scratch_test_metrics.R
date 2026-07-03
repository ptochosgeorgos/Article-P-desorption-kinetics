source("notebooks/qi_modelling1.R")

library(MuMIn)

get_metrics_lmer <- function(model, model_name, data, response) {
    r2 <- suppressWarnings(MuMIn::r.squaredGLMM(model))
    r2m <- round(r2[1, "R2m"], 3)
    r2c <- round(r2[1, "R2c"], 3)
    
    # RMSE marginal (fixed effects only)
    pred_m <- predict(model, re.form = NA)
    rmse_m <- round(sqrt(mean((data[[response]] - pred_m)^2, na.rm=TRUE)), 3)
    
    # RMSE conditional
    pred_c <- predict(model)
    rmse_c <- round(sqrt(mean((data[[response]] - pred_c)^2, na.rm=TRUE)), 3)
    
    data.frame(
        Model = model_name,
        R2_m = r2m, R2_c = r2c,
        RMSE_m = rmse_m, RMSE_c = rmse_c,
        AIC = round(AIC(model), 1),
        BIC = round(BIC(model), 1)
    )
}

get_metrics_nlme <- function(model, model_name, data, response) {
    # NLME R2 via correlation
    pred_c <- predict(model, level = 2) # Plot level
    pred_m <- predict(model, level = 0) # Fixed effects only
    
    r2_c <- round(cor(data[[response]], pred_c)^2, 3)
    r2_m <- round(cor(data[[response]], pred_m)^2, 3)
    
    rmse_c <- round(sqrt(mean((data[[response]] - pred_c)^2, na.rm=TRUE)), 3)
    rmse_m <- round(sqrt(mean((data[[response]] - pred_m)^2, na.rm=TRUE)), 3)
    
    data.frame(
        Model = model_name,
        R2_m = r2_m, R2_c = r2_c,
        RMSE_m = rmse_m, RMSE_c = rmse_c,
        AIC = round(AIC(model), 1),
        BIC = round(BIC(model), 1)
    )
}

cat("\nUptake Models Metrics:\n")
metrics_list <- list(
    get_metrics_lmer(m_uptake_geo, "1. Geo PBC - Raw P_CO2", D_Uptake, "annual_P_uptake"),
    get_metrics_lmer(m_uptake_geo_thermo, "2. Geo PBC - Thermo a_CO2", D_Uptake, "annual_P_uptake"),
    get_metrics_lmer(m_uptake_geo_legacy, "3. Geo PBC - Legacy P_AAE10", D_Uptake, "annual_P_uptake")
)
df_uptake_metrics <- dplyr::bind_rows(metrics_list)
print(df_uptake_metrics)

cat("\nYield Model Metrics:\n")
df_yield_metrics <- get_metrics_nlme(m_yield_nlme, "Yield ~ P_CO2 (Mitscherlich NLME)", D_Yield, "Relative_Yield")
print(df_yield_metrics)

