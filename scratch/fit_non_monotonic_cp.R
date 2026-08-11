library(nlme)
library(dplyr)
library(tidyr)
library(ggplot2)

options(warn = -1)
final_obj <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_obj$data$D_Long_Agro

D_CP <- D_Long_Agro |>
    mutate(
        C_P = annual_P_uptake / annual_yield_mp_DM
    ) |>
    filter(is.finite(C_P), C_P > 0, a_CO2_total_mg_L > 0)

# The Exponential-Linear Hook Model
# C_P = C_int + A_dil * exp(-k_dil * I) + S_acc * I

mod_nls <- try(nls(
    C_P ~ C_int + A_dil * exp(-k_dil * a_CO2_total_mg_L) + S_acc * a_CO2_total_mg_L,
    data = D_CP,
    start = list(C_int = 2, A_dil = 2, k_dil = 5, S_acc = 0.5)
))

if (inherits(mod_nls, "try-error")) {
    cat("NLS failed.\n")
    start_vals <- c(C_int = 2, A_dil = 2, k_dil = 5, S_acc = 0.5)
} else {
    cat("NLS succeeded.\n")
    start_vals <- coef(mod_nls)
    print(start_vals)
}

cat("\n--- Fitting NLME Parametrization ---\n")
mod_nlme <- try(nlme(
    C_P ~ C_int + A_dil * exp(-k_dil * a_CO2_total_mg_L) + S_acc * a_CO2_total_mg_L,
    data = D_CP,
    fixed = C_int + A_dil + k_dil + S_acc ~ 1,
    random = C_int ~ 1 | site/year_f,
    start = start_vals,
    control = nlmeControl(maxIter = 1000)
))

if (!inherits(mod_nlme, "try-error")) {
    print(summary(mod_nlme))
    cf <- fixef(mod_nlme)
    
    # Derivative: -A_dil * k_dil * exp(-k_dil * I) + S_acc = 0
    # exp(-k_dil * I) = S_acc / (A_dil * k_dil)
    # I = - (1 / k_dil) * log( S_acc / (A_dil * k_dil) )
    
    val <- cf["S_acc"] / (cf["A_dil"] * cf["k_dil"])
    if (val > 0 && val < 1) {
        I_crit <- - (1 / cf["k_dil"]) * log(val)
        cat(sprintf("\n*** Analytical Minimum (P_crit): %.5f mg/L ***\n", I_crit))
    } else {
        cat("\nDerivative root is outside physically meaningful bounds.\n")
        I_crit <- NA
    }
} else {
    cat("NLME failed.\n")
}
