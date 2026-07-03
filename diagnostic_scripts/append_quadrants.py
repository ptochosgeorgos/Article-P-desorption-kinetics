import os

# Define the new chunk of code
new_chunk = """

## ----pcrit-quadrant-analysis, fig.width=10, fig.height=6----------------------
# 1. Prepare function to do LOOCV and quadrant classification
run_loocv_quadrants <- function(data, p_col, inv_b_col) {
    sites <- unique(as.character(data$site))
    results <- list()
    
    for (test_site in sites) {
        # Split Data and create standard column names for the formula
        train_data <- data |> dplyr::filter(site != test_site) |> dplyr::mutate(P_test = .data[[p_col]], z_inv_b_test = .data[[inv_b_col]])
        test_data  <- data |> dplyr::filter(site == test_site) |> dplyr::mutate(P_test = .data[[p_col]], z_inv_b_test = .data[[inv_b_col]])
        
        start_vals <- c(1.2, rep(0, length(unique(train_data$crop)) - 1), 0, 0, 0, 0, 0, 0, 0, 0)
        
        # Fit Model on Training Data (n-1)
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
        }, error = function(e) NULL)
        
        if (!is.null(fit)) {
            cf <- fixef(fit)
            
            # Predict P_crit for the TEST SITE
            test_res <- test_data |>
                dplyr::mutate(
                    c_base_crop = cf["c_base.(Intercept)"] + tidyr::replace_na(cf[paste0("c_base.crop", crop)], 0),
                    c_eff_loo = c_base_crop * exp(
                        cf["beta_invb"] * z_inv_b_test + 
                        cf["beta_pH"] * z_pH + 
                        cf["beta_K"] * z_ln_K +
                        cf["beta_Mg"] * z_ln_Mg +
                        cf["beta_N"] * z_fert_N +
                        cf["beta_Temp"] * z_Temp_Mean + 
                        cf["beta_Prec"] * z_Prec_Anom
                    ),
                    P_crit_loo = (log(20) / c_eff_loo) - cf["E_base"],
                    
                    Quadrant = dplyr::case_when(
                        P_test >= P_crit_loo & Relative_Yield >= 0.95 ~ "True Positive (Success)",
                        P_test <  P_crit_loo & Relative_Yield <  0.95 ~ "True Negative (Correct Warning)",
                        P_test >= P_crit_loo & Relative_Yield <  0.95 ~ "False Positive (Failure)",
                        P_test <  P_crit_loo & Relative_Yield >= 0.95 ~ "False Negative (Over-fertilized)",
                        TRUE ~ "NA"
                    )
                )
            results[[test_site]] <- test_res
        }
    }
    
    dplyr::bind_rows(results)
}

# 2. Run Quadrant Analysis for P_CO2 and P_AAE10
cat("Running LOOCV Quadrant Analysis... This may take a minute...\\n")

quad_co2 <- run_loocv_quadrants(D_Yield, "soil_0_20_P_CO2", "z_inv_b")
quad_aae <- run_loocv_quadrants(D_Yield, "soil_0_20_P_AAE10", "z_inv_b")

# 3. Summarize the Results
sum_quads <- function(quad_data, extractant_name) {
    total <- nrow(quad_data)
    quad_data |>
        dplyr::group_by(Quadrant) |>
        dplyr::summarise(Count = n(), .groups = 'drop') |>
        dplyr::mutate(
            Extractant = extractant_name,
            Percentage = round((Count / total) * 100, 1)
        )
}

res_co2 <- sum_quads(quad_co2, "P_CO2 (Intensity)")
res_aae <- sum_quads(quad_aae, "P_AAE10 (Legacy Pool)")

quad_summary <- dplyr::bind_rows(res_co2, res_aae) |>
    dplyr::select(Extractant, Quadrant, Count, Percentage) |>
    dplyr::arrange(Quadrant, Extractant)

quad_summary |>
    kbl(caption = "**Table 5: Predictive Quadrant Analysis (LOOCV).** Evaluating the agronomic safety of dynamic P_crit predictions on fully unseen sites.") |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

# 4. Visualization
p_quad <- ggplot(quad_summary, aes(x = Quadrant, y = Percentage, fill = Extractant)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
    scale_fill_manual(values = c("P_CO2 (Intensity)" = "#2c7bb6", "P_AAE10 (Legacy Pool)" = "#d7191c")) +
    geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5, fontface = "bold") +
    labs(
        title = "Agronomic Safety Check: Out-of-Sample P_crit Quadrants",
        subtitle = "False Positive = Predicted P was sufficient, but yield was actually < 0.95 (Most dangerous error!)",
        x = "Agronomic Outcome Quadrant", y = "Percentage of Left-out Plots (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 15, hjust = 1, face = "bold")
    )

print(p_quad)
"""

def append_to_file(filepath, ext):
    if not os.path.exists(filepath):
        return
        
    with open(filepath, 'r') as f:
        content = f.read()
        
    if "pcrit-quadrant-analysis" in content:
        print(f"Quadrant analysis already exists in {filepath}")
        return
        
    if ext == 'qmd':
        chunk_to_append = f"```{{r}}\n{new_chunk}\n```\n"
    else:
        chunk_to_append = new_chunk + "\n"
        
    with open(filepath, 'a') as f:
        f.write(chunk_to_append)
    print(f"Appended to {filepath}")

append_to_file('notebooks/qi_modelling1.R', 'R')
append_to_file('notebooks/qi_modelling1.qmd', 'qmd')
