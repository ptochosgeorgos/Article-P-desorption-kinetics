source("scratch/qi_code_subset_final.R")
library(nlme)
cat("Starting m_yield_raw_aae...\n")
fit <- tryCatch({
    nlme(
        total_yield ~ Y0 + (A - Y0) * (1 - exp(-(c_base * exp(
            beta_invb * z_inv_b_cons + beta_N * z_fert_N + beta_Temp * z_Temp_Mean + beta_Prec * z_Prec_Anom
        ) * soil_0_20_P_AAE10))),
        data = D_Yield,
        fixed = list(A ~ crop, Y0 ~ crop, c_base ~ crop, beta_invb ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1),
        random = Y0 ~ 1 | site,
        weights = varPower(form = ~ soil_0_20_P_AAE10),
        start = c(10, rep(0, length(unique(D_Yield$crop)) - 1), 2, rep(0, length(unique(D_Yield$crop)) - 1), 0.05, rep(0, length(unique(D_Yield$crop)) - 1), rep(0, 4)),
        control = nlmeControl(maxIter = 2000, returnObject = TRUE)
    )
}, error = function(e) {
    cat("Error:", e$message, "\n")
    NULL
})
if (!is.null(fit)) print(summary(fit))
