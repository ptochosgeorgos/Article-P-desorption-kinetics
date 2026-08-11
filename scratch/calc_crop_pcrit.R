library(dplyr)
library(jsonlite)
library(nlme)

options(warn=-1)

final_obj <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_obj$data$D_Long_Agro
coefs <- fromJSON("presentation/calculator_coefs.json")

D_CP <- D_Long_Agro |>
    mutate(C_P = annual_P_uptake / annual_yield_mp_DM) |>
    filter(is.finite(C_P), C_P > 0, a_CO2_total_mg_L > 0)

target_crops <- names(coefs$crop_norms)

cat("| Crop | Crop Name | Empirical P_crit (mg/L) | K_m (mg/L) | Multiplier (P_crit / K_m) | P_up_max % |\n")
cat("|------|-----------|-------------------------|------------|---------------------------|------------|\n")

for (cr in target_crops) {
    c_name <- coefs$crop_norms[[cr]]$name
    d_sub <- D_CP |> filter(crop == cr)
    if(nrow(d_sub) < 15) {
        cat(sprintf("| %s | %s | N/A (Too little data) | N/A | N/A | N/A |\n", cr, c_name))
        next
    }
    
    mod <- try(nls(
        C_P ~ C_int + A_dil * exp(-k_dil * a_CO2_total_mg_L) + S_acc * a_CO2_total_mg_L,
        data = d_sub,
        start = list(C_int = mean(d_sub$C_P)*0.8, A_dil = 2, k_dil = 10, S_acc = 0.1),
        control = nls.control(warnOnly = TRUE, maxiter = 2000)
    ), silent=TRUE)
    
    P_crit <- NA
    if(!inherits(mod, "try-error")) {
        cf <- coef(mod)
        val <- cf["S_acc"] / (cf["A_dil"] * cf["k_dil"])
        if(val > 0 && val < 1 && !is.na(val)) {
            P_crit <- as.numeric(- (1 / cf["k_dil"]) * log(val))
            if (P_crit < 0) P_crit <- NA
        }
    }
    
    K_base <- coefs$uptake_coefs$K_base_crops[[cr]]
    if (is.null(K_base)) {
        K_base <- coefs$uptake_coefs$K_base_crops[["WW"]]
    }
    
    if(!is.na(P_crit) && !is.null(K_base)) {
        ratio <- P_crit / K_base
        u_pct <- (P_crit / (K_base + P_crit)) * 100
        cat(sprintf("| **%s** | %s | **%.4f** | %.4f | **%.1f x** | **%.1f%%** |\n", cr, c_name, P_crit, K_base, ratio, u_pct))
    } else {
        cat(sprintf("| **%s** | %s | N/A (No Hook) | %.4f | N/A | N/A |\n", cr, c_name, K_base))
    }
}
