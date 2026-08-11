library(dplyr)
library(jsonlite)

options(warn=-1)

final_obj <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_obj$data$D_Long_Agro

D_CP <- D_Long_Agro |>
    mutate(C_P = annual_P_uptake / annual_yield_mp_DM) |>
    filter(is.finite(C_P), C_P > 0, a_CO2_total_mg_L > 0)

target_crops <- c("WW", "KA", "ZR", "RA", "KM", "SM", "SW")

cat("| Crop | Exponent (k) | $P_{crit}$ (3/k) (mg/L) |\n")
cat("|------|--------------|-------------------------|\n")

for (cr in target_crops) {
    d_sub <- D_CP |> filter(crop == cr)
    if(nrow(d_sub) >= 15) {
        # Individual crop NLS fit
        mod <- try(nls(
            C_P ~ C_int + A_dil * exp(-k_dil * a_CO2_total_mg_L) + S_acc * a_CO2_total_mg_L,
            data = d_sub,
            start = list(C_int = mean(d_sub$C_P)*0.8, A_dil = 2, k_dil = 10, S_acc = 0.1),
            control = nls.control(warnOnly = TRUE, maxiter = 2000)
        ), silent=TRUE)
        
        if(!inherits(mod, "try-error")) {
            cf <- coef(mod)
            k_val <- cf["k_dil"]
            p_crit <- 3.0 / k_val
            
            cat(sprintf("| %s | %12.4f | %23.4f |\n", cr, k_val, p_crit))
        } else {
            cat(sprintf("| %s |          N/A |                     N/A |\n", cr))
        }
    }
}
