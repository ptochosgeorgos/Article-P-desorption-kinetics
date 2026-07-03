cat << 'EOF' > scratch_test_metrics2.R
source("notebooks/qi_modelling1.R")
library(MuMIn)
library(dplyr)

get_metrics_lmer <- function(model, model_name, data, response) {
    r2 <- suppressWarnings(MuMIn::r.squaredGLMM(model))
    r2m <- round(r2[1, "R2m"], 3)
    r2c <- round(r2[1, "R2c"], 3)
    
    pred_m <- predict(model, re.form = NA)
    rmse_m <- round(sqrt(mean((data[[response]] - pred_m)^2, na.rm=TRUE)), 3)
    
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

cat("\nPTF Models Metrics:\n")
metrics_list <- list(
    get_metrics_lmer(ptf_geo_raw, "1. Geo PBC - Raw P_CO2", D_ptf, "ln_P_AAE"),
    get_metrics_lmer(ptf_geo_thm, "2. Geo PBC - Thermo a_CO2", D_ptf, "ln_P_AAE"),
    get_metrics_lmer(ptf_agro_raw, "1. Agro PBC - Raw P_CO2", D_ptf, "ln_P_AAE"),
    get_metrics_lmer(ptf_agro_thm, "2. Agro PBC - Thermo a_CO2", D_ptf, "ln_P_AAE")
)
df_ptf_metrics <- dplyr::bind_rows(metrics_list)
print(df_ptf_metrics)

cat("\nYield Model Metrics:\n")
df_yield_metrics <- get_metrics_nlme(m_yield_nlme, "Yield ~ P_CO2 (Mitscherlich NLME)", D_Yield, "Relative_Yield")
print(df_yield_metrics)

EOF
Rscript scratch_test_metrics2.R
