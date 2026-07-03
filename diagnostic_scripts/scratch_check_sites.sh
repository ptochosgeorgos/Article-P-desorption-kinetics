cat << 'EOF' > scratch_check_sites.R
source("notebooks/qi_modelling1.R")
library(dplyr)

quad_co2 <- run_loocv_quadrants(D_Yield, "soil_0_20_P_CO2", "z_inv_b")

def_data <- quad_co2 |>
    dplyr::filter(Quadrant == "True Negative (Correct Warning)")

cat("Sites in def_data (True Negatives):\n")
print(table(def_data$site))

def_data_acidic <- def_data |> dplyr::filter(rollMean_soil_0_20_pH_H2O < 7.20)
cat("\nSites in def_data_acidic (pH < 7.2):\n")
print(table(def_data_acidic$site))

def_data_field <- def_data_acidic |> dplyr::filter(P_crit_loo < 5.0)
cat("\nSites in def_data_field (P_crit_loo < 5.0):\n")
print(table(def_data_field$site))

cat("\nSummary of P_crit_loo by Site in def_data_acidic:\n")
def_data_acidic |> group_by(site) |> summarize(
    Min = min(P_crit_loo),
    Mean = mean(P_crit_loo),
    Max = max(P_crit_loo),
    Count = n()
) |> print()

EOF
Rscript scratch_check_sites.R
