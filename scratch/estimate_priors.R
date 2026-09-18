library(tidyverse)
library(lme4)

d_brms <- readRDS("data/D_Yield_clean.rds")

crop_refs <- data.frame(
  crop = c("Wheat", "KA", "KM", "RA", "SJ", "WG", "ZR", "FR"),
  Y_ref = c(60, 450, 100, 30, 30, 60, 900, 175),
  P_up_ref = c(27, 30, 38, 24, 30, 28, 41, 52)
)

# Empirical bounds from our earlier analysis
bounds <- data.frame(
  crop = c("FR", "KA", "KM", "RA", "SJ", "WG", "Wheat", "ZR"),
  Y0 = c(107, 13, 20, 3, 10, 21, 3, 36),
  A = c(290, 152, 142, 49, 44, 92, 73, 271)
)

d_brms <- d_brms |>
    mutate(crop = case_when(
      crop %in% c("WW", "SW", "WS") ~ "Wheat",
      TRUE ~ as.character(crop)
    )) |>
    left_join(bounds, by = "crop") |>
    mutate(
      z_k_pred = scale(ln_K_pred_agro)[, 1],
      crop = as.factor(crop),
      site = as.factor(site),
      year = as.factor(year)
    ) |>
    filter(
      !is.na(annual_yield_mp_DM), 
      !is.na(z_inv_b),
      !is.na(z_k_pred),
      !is.na(juvdev_temp),
      !is.na(juvdev_prec),
      !is.na(z_fert_N)
    ) |>
    # To prevent log(0) and divide by zero, filter strictly between bounds
    filter(annual_yield_mp_DM > Y0 + 0.1 & annual_yield_mp_DM < A - 0.1) |>
    mutate(
      Z = log((A - annual_yield_mp_DM) / (annual_yield_mp_DM - Y0)) + log(soil_0_20_P_CO2)
    )

cat("Fitting robust lmer...\n")
mod_lmer <- lmer(Z ~ z_inv_b + z_k_pred + z_fert_N + juvdev_temp + juvdev_prec + (1 | crop) + (1 | site/year), data = d_brms)

summary(mod_lmer)
