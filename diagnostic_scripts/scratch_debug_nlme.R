library(dplyr)
library(nlme)

# Recreate the data prep up to D_Yield
D_ready <- readRDS("data/3_D_ready.rds")
D_Yield <- D_ready |>
    filter(!is.na(soil_0_20_P_CO2), !is.na(soil_0_20_P_AAE10), !is.na(fert_N_tot)) |>
    mutate(
        inv_b = 1, # Dummy value to avoid calculating the PTF
        total_yield = tidyr::replace_na(annual_yield_mp_DM, 0)
    ) |>
    group_by(site, crop, year) |>
    mutate(Relative_Yield = total_yield / max(total_yield, na.rm = TRUE)) |>
    ungroup() |>
    filter(is.finite(inv_b), is.finite(Relative_Yield), Relative_Yield > 0) |>
    mutate(
        z_inv_b = as.numeric(scale(inv_b)),
        z_fert_N = as.numeric(scale(fert_N_tot)),
        z_pH = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
        z_ln_K = as.numeric(scale(log(soil_0_20_K_AAE10))),
        z_ln_Mg = as.numeric(scale(log(soil_0_20_Mg_AAE10))),
        z_Temp_Mean = as.numeric(scale(Temp_Mean)),
        z_Prec_Anom = as.numeric(scale(Prec_Anom)),
        site = as.factor(site),
        year_f = as.factor(year),
        plot_nr = as.factor(plot_nr), 
        crop = droplevels(as.factor(crop))
    )

n_crops <- length(levels(D_Yield$crop))
cat("n_crops based on levels:", n_crops, "\n")
cat("Expected parameter length for nlme:", n_crops + 8, "\n")

# Let's see the model matrix
form <- c_base ~ crop
cat("Model matrix columns:", ncol(model.matrix(form, data = D_Yield)), "\n")

start_vec <- c(1.2, rep(0, n_crops - 1), 0, 0, 0, 0, 0, 0, 0, 0)
cat("Length of start_vec:", length(start_vec), "\n")

# Let's try running nlme now
tryCatch({
    m_yield_raw_co2 <- nlme(
        Relative_Yield ~ 1 - exp(-(c_base * exp(
            beta_invb * z_inv_b +
            beta_pH * z_pH +
            beta_K * z_ln_K +
            beta_Mg * z_ln_Mg +
            beta_N * z_fert_N +
            beta_Temp * z_Temp_Mean +
            beta_Prec * z_Prec_Anom
        )) * (soil_0_20_P_CO2 + E_base)),
        data = D_Yield,
        fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_K ~ 1, beta_Mg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
        random = c_base ~ 1 | site/plot_nr,
        start = start_vec,
        control = nlmeControl(maxIter = 2, returnObject = TRUE)
    )
    cat("Success!\n")
}, error = function(e) {
    cat("Error caught:\n")
    print(e)
})
