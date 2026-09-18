suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readxl))

RES <- readRDS("data/RES.rds")
D_main <- RES$D

climate_data <- readRDS("data/all_P.rds") |> dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec) |> dplyr::distinct()
D2 <- read_excel("data/STYCS_data_2023_260511.xlsx") |> rename(rep = replicate) |> mutate(site = gsub("STYCS_", "", LtE_name)) |> left_join(climate_data, by = c("site", "year")) |> mutate(crop = crop_abr, annual_P_uptake = rowSums(across(starts_with("P_harv")), na.rm = TRUE), fert_P_tot = fert_P2O5_tot / 2.291, annual_P_balance = fert_P_tot - annual_P_uptake)

D_k <- D_main |> filter(!is.na(k)) |> dplyr::select(site, treatment_ID, rep, k, v0_kPS = kPS, Pmax_PS = PS) |> distinct(site, treatment_ID, rep, .keep_all = TRUE)
D_main <- D2 |> left_join(D_k, by = c("site", "treatment_ID", "rep"))

D_main <- D_main |> mutate(
    soil_0_20_P_CO2 = soil_0_20_P_test * 0.155,
    Temp_Anom = juvdev_temp,
    fert_N_tot = ifelse(is.na(fert_N_tot), 0, fert_N_tot)
)

D_ready <- D_main |> mutate(
    z_fert_N = as.numeric(scale(fert_N_tot)),
    z_Temp_Anom = as.numeric(scale(Temp_Anom))
)

cat("annual_P_uptake > 0:", sum(D_ready$annual_P_uptake > 0, na.rm=TRUE), "\n")
D_Long <- D_ready |> filter(annual_P_uptake > 0)
cat("D_Long rows:", nrow(D_Long), "\n")
cat("D_Long missing z_fert_N:", sum(is.na(D_Long$z_fert_N)), "\n")
cat("D_Long missing z_Temp_Anom:", sum(is.na(D_Long$z_Temp_Anom)), "\n")
cat("D_Long missing soil_0_20_P_CO2:", sum(is.na(D_Long$soil_0_20_P_CO2)), "\n")
cat("D_Long missing k:", sum(is.na(D_Long$k)), "\n")

D_Long <- D_Long |> filter(!is.na(k), !is.na(soil_0_20_P_CO2), !is.na(z_fert_N), !is.na(z_Temp_Anom))
cat("D_Long filtered rows:", nrow(D_Long), "\n")
cat("Crops:", length(unique(D_Long$crop)), "\n")
print(table(D_Long$crop))

