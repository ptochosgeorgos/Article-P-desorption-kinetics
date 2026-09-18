source("scratch/qi_code_subset.R")
library(nlme)

# D_Yield is now defined exactly as in the notebook.
cat("Dimensions of D_Yield:", dim(D_Yield), "\n")
cat("Crops in D_Yield:\n")
print(table(D_Yield$crop))

# Let's try running the model:
n_crops_yield <- length(levels(D_Yield$crop))
start_vals <- c(10, rep(0, n_crops_yield - 1), 2, rep(0, n_crops_yield - 1), 1.2, rep(0, n_crops_yield - 1), rep(0, 4), 0)

cat("Starting model fit...\n")
res <- tryCatch({
    nlme(
        total_yield ~ Y0 + (A - Y0) * (1 - exp(-(c_base * exp(
            beta_invb * z_inv_b + beta_N * z_fert_N + beta_Temp * z_Temp_Mean + beta_Prec * z_Prec_Anom
        ) * (soil_0_20_P_CO2 + E_base)))),
        data = D_Yield,
        fixed = list(A ~ crop, Y0 ~ crop, c_base ~ crop, beta_invb ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
        random = E_base ~ 1 | site,
        weights = varPower(form = ~ soil_0_20_P_CO2),
        start = start_vals,
        control = nlmeControl(maxIter = 2000, returnObject = TRUE, msVerbose=TRUE)
    )
}, error = function(e) e)

if (inherits(res, "error")) {
    cat("Error encountered:\n")
    print(res$message)
    
    # Try finding collinearity in fixed effects structure
    cat("\nChecking linear dependence in fixed effect matrix...\n")
    form <- ~ crop + crop + crop + z_inv_b + z_fert_N + z_Temp_Mean + z_Prec_Anom
    X <- model.matrix(form, data = D_Yield)
    qr_X <- qr(X)
    cat("Rank of X:", qr_X$rank, " out of", ncol(X), "columns\n")
    if (qr_X$rank < ncol(X)) {
        cat("Aliased terms:\n")
        print(alias(lm(rnorm(nrow(D_Yield)) ~ form, data = D_Yield)))
    }
} else {
    cat("Model converged successfully!\n")
}
