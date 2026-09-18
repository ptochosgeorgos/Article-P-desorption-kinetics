source("scratch/qi_code_subset_final.R")
library(nlme)
library(dplyr)

m_yield_raw_co2 <- nlme(total_yield ~ Y0 + (A - Y0) * (1 - exp(-(c_base * exp(beta_invb * z_inv_b + beta_N * z_fert_N + beta_Temp * z_Temp_Mean + beta_Prec * z_Prec_Anom) * soil_0_20_P_CO2))), data = D_Yield, fixed = list(A ~ crop, Y0 ~ crop, c_base ~ crop, beta_invb ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1), random = Y0 ~ 1 | site, weights = varPower(form = ~ soil_0_20_P_CO2), start = c(100, rep(0, length(unique(D_Yield$crop)) - 1), 10, rep(0, length(unique(D_Yield$crop)) - 1), 1.2, rep(0, length(unique(D_Yield$crop)) - 1), rep(0, 4)), control = nlmeControl(maxIter = 2000, returnObject = TRUE))

cf <- fixef(m_yield_raw_co2)
D_test <- D_Yield |>
    mutate(
        c_base_crop = cf["c_base.(Intercept)"] + tidyr::replace_na(cf[paste0("c_base.crop", crop)], 0),
        plot_full_id = paste0(site, "/", plot_nr),
        re_total = ranef(m_yield_raw_co2)[as.character(site), 1],
        c_eff  = (c_base_crop + tidyr::replace_na(re_total, 0)) * exp(
            cf["beta_invb"] * z_inv_b +
            cf["beta_N"] * z_fert_N +
            cf["beta_Temp"] * z_Temp_Mean +
            cf["beta_Prec"] * z_Prec_Anom
        ),
        P_crit = log(20) / c_eff,
        ln_P_crit = log(P_crit)
    )

print(head(D_test))
print(summary(D_test$c_eff))
print(summary(D_test$P_crit))
print(summary(D_test$ln_P_crit))

print(sum(is.finite(D_test$ln_P_crit)))

