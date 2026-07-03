cat << 'EOF' > scratch_check_years.R
climate_data <- readRDS("data/all_P.rds") |>
    dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec) |>
    dplyr::distinct()
print(aggregate(year ~ site, data = climate_data, FUN = function(x) c(min = min(x), max = max(x))))

D2 <- readxl::read_excel("data/STYCS_data_2023_260511.xlsx")
print(aggregate(year ~ LtE_name, data = D2, FUN = function(x) c(min = min(x), max = max(x))))

EOF
Rscript scratch_check_years.R
