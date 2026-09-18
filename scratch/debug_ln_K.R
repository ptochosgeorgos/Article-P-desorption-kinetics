source("scratch/purl_data_only.R")
job <- function() {
    nlme(
        annual_P_uptake ~ ((V_max + beta_temp * z_Temp_Anom + beta_N * z_fert_N) * soil_0_20_P_CO2) /
            ((exp(ln_K_base) * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + soil_0_20_P_CO2),
        data = D_Long_Agro, fixed = list(V_max ~ crop, beta_temp ~ 1, beta_N ~ 1, ln_K_base ~ crop, beta_invb ~ 1, beta_v0 ~ 1), random = V_max ~ 1 | site/year_f,
        start = c(30, rep(0, n_crops - 1), 0, 0, log(median(D_Long_Agro$soil_0_20_P_CO2)), rep(0, n_crops - 1), 0, 0), control = nlmeControl(maxIter = 1000)
    )
}
tryCatch({
    res <- job()
    cat("\nSUCCESS with ln_K_base!\n")
    print(summary(res))
}, error = function(e) {
    cat("\nERROR with ln_K_base:", e$message, "\n")
})
