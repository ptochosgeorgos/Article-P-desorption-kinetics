cat << 'EOF' > scratch_debug_loocv.R
source("notebooks/qi_modelling1.R")

test_site <- "ALT"
data <- D_Yield
p_col <- "soil_0_20_P_AAE10"
inv_b_col <- "z_inv_b"

train_data <- data |> dplyr::filter(site != test_site) |> dplyr::mutate(P_test = .data[[p_col]], z_inv_b_test = .data[[inv_b_col]])
test_data  <- data |> dplyr::filter(site == test_site) |> dplyr::mutate(P_test = .data[[p_col]], z_inv_b_test = .data[[inv_b_col]])

train_data$crop <- droplevels(train_data$crop)
start_vals <- c(1.2, rep(0, length(levels(train_data$crop)) - 1), 0, 0, 0, 0, 0, 0, 0, 0)

cat("Starting NLME fit...\n")
fit <- tryCatch({
    nlme(
        Relative_Yield ~ 1 - exp(-(c_base * exp(
            beta_invb * z_inv_b_test + 
            beta_pH * z_pH + 
            beta_K * z_ln_K +
            beta_Mg * z_ln_Mg +
            beta_N * z_fert_N +
            beta_Temp * z_Temp_Mean + 
            beta_Prec * z_Prec_Anom
        )) * (P_test + E_base)),
        data = train_data,
        fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_K ~ 1, beta_Mg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
        random = c_base ~ 1 | site/plot_nr,
        start = start_vals,
        control = nlmeControl(maxIter = 1000, returnObject = TRUE)
    )
}, error = function(e) {
    cat("Error caught:\n")
    print(e)
    NULL
})
EOF
Rscript scratch_debug_loocv.R
