library(dplyr)
library(jsonlite)

options(warn=-1)
coefs <- fromJSON("presentation/calculator_coefs.json")

E_base <- coefs$yield_coefs$E_base
c_crops <- coefs$yield_coefs$c_base_crops
K_crops <- coefs$uptake_coefs$K_base_crops
target_crops <- names(coefs$crop_norms)

cat("| Crop | Empirical $P_{crit}$ (mg/L) | Theoretical $P_{crit}$ ($P_{up}/Y$) (mg/L) |\n")
cat("|------|-----------------------------|--------------------------------------------|\n")

final_obj <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_obj$data$D_Long_Agro
D_CP <- D_Long_Agro |>
    mutate(C_P = annual_P_uptake / annual_yield_mp_DM) |>
    filter(is.finite(C_P), C_P > 0, a_CO2_total_mg_L > 0)

for (cr in target_crops) {
    # 1. Empirical
    d_sub <- D_CP |> filter(crop == cr)
    emp_P_crit <- NA
    if(nrow(d_sub) >= 15) {
        mod <- try(nls(
            C_P ~ C_int + A_dil * exp(-k_dil * a_CO2_total_mg_L) + S_acc * a_CO2_total_mg_L,
            data = d_sub,
            start = list(C_int = mean(d_sub$C_P)*0.8, A_dil = 2, k_dil = 10, S_acc = 0.1),
            control = nls.control(warnOnly = TRUE, maxiter = 2000)
        ), silent=TRUE)
        if(!inherits(mod, "try-error")) {
            cf <- coef(mod)
            val <- cf["S_acc"] / (cf["A_dil"] * cf["k_dil"])
            if(val > 0 && val < 1 && !is.na(val)) {
                emp <- as.numeric(- (1 / cf["k_dil"]) * log(val))
                if (emp > 0) emp_P_crit <- emp
            }
        }
    }
    
    # 2. Theoretical
    K_m <- K_crops[[cr]]
    c_val <- c_crops[[cr]]
    
    theo_P_crit <- NA
    if (!is.null(K_m) && !is.null(c_val)) {
        f_trans <- function(I) {
            dpup <- K_m / (I * (K_m + I))
            dy <- ( (1 - E_base) * c_val * exp(-c_val * I) ) / ( E_base + (1 - E_base) * (1 - exp(-c_val * I)) )
            return(dpup - dy)
        }
        res <- try(uniroot(f_trans, lower = 1e-6, upper = 5, extendInt = "yes"), silent=TRUE)
        if (!inherits(res, "try-error")) {
            theo_P_crit <- res$root
        }
    }
    
    emp_str <- if(!is.na(emp_P_crit)) sprintf("%.4f", emp_P_crit) else "N/A"
    theo_str <- if(!is.na(theo_P_crit)) sprintf("%.6f", theo_P_crit) else "N/A"
    
    cat(sprintf("| %s | %s | %s |\n", cr, emp_str, theo_str))
}
