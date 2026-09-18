library(knitr)
purl("notebooks/qi_modelling_parallel.qmd", output = "scratch/qi_code.R", documentation = 0)

lines <- readLines("scratch/qi_code.R")
idx <- grep("jobs_yield <- list", lines)
if(length(idx) > 0) {
    cat(lines[1:(idx[1]-1)], sep="\n", file="scratch/qi_code_subset.R")
} else {
    stop("Could not find yield jobs definition")
}

source("scratch/qi_code_subset.R")
library(nlme)

cat("Dimensions of D_Yield:", dim(D_Yield), "\n")
cat("Crops in D_Yield:\n")
print(table(D_Yield$crop))

if(nrow(D_Yield) > 0) {
    n_crops_yield <- length(levels(D_Yield$crop))
    start_vals <- c(10, rep(0, n_crops_yield - 1), 2, rep(0, n_crops_yield - 1), 1.2, rep(0, n_crops_yield - 1), rep(0, 4), 0)

    cat("Starting model fit with crop effects...\n")
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
            control = nlmeControl(maxIter = 2000, returnObject = TRUE, msVerbose=FALSE)
        )
    }, error = function(e) e)

    if (inherits(res, "error")) {
        cat("Error encountered:\n")
        print(res$message)
    } else {
        cat("Model converged successfully!\n")
    }
}
