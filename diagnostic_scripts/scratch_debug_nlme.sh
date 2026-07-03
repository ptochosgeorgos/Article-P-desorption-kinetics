cat << 'EOF' > scratch_debug_nlme.R
# Just load data manually without sourcing qi_modelling1.R
library(dplyr)
library(nlme)
library(readxl)

# Use the saved all_P which now has REC renamed to REH!
climate_data <- readRDS("data/all_P.rds") |>
    dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec) |>
    dplyr::distinct()

D2 <- read_excel("data/STYCS_data_2023_260511.xlsx") |>
    rename(rep = replicate) |>
    mutate(site = gsub("STYCS_", "", LtE_name)) |>
    left_join(climate_data, by = c("site", "year")) |>
    mutate(
        soil_0_20_P_CO2 = soil_0_20_P_test * 0.155,
        crop = crop_abr,
        annual_P_uptake = rowSums(across(starts_with("P_harv")), na.rm = TRUE),
        annual_yield_mp_DM = rowSums(across(matches("^harv.*mp_yield_DM$")), na.rm = TRUE),
        annual_yield_bp_DM = rowSums(across(matches("^harv.*bp[1-2]_yield_DM$")), na.rm = TRUE)
    )

D_main <- D2 |>
    filter(year >= 1990) |>
    group_by(site) |>
    mutate(
        site_juv_temp_mean = mean(juvdev_temp, na.rm = TRUE), site_juv_prec_mean = mean(juvdev_prec, na.rm = TRUE),
        temp_anomaly = juvdev_temp - site_juv_temp_mean, prec_anomaly = juvdev_prec - site_juv_prec_mean
    ) |> ungroup()

# ... Just use a dummy subset for the nlme test to save typing out the whole pipeline
# Actually, I can just load the workspace before the loocv!
EOF
Rscript scratch_debug_nlme.R
