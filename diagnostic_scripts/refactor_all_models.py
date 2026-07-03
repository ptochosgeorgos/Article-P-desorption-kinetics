import re
import sys

with open('notebooks/qi_modelling1.qmd', 'r') as f:
    content = f.read()

# 1. mitscherlich-yield-models chunk
yield_chunk_pattern = re.compile(r'```{r mitscherlich-yield-models,.*?```', re.DOTALL)
new_yield_chunk = """```{r mitscherlich-yield-models, fig.width=10, fig.height=5}
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
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1), 0, 0, 0, 0, 0, 0, 0, 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_thm_co2 <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_invb * z_inv_b +
        beta_pH * z_pH +
        beta_K * z_ln_K +
        beta_Mg * z_ln_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean +
        beta_Prec * z_Prec_Anom
    )) * (a_CO2_total_mg_L + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_K ~ 1, beta_Mg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1), 0, 0, 0, 0, 0, 0, 0, 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

m_yield_raw_aae <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_invb * z_inv_b +
        beta_pH * z_pH +
        beta_K * z_ln_K +
        beta_Mg * z_ln_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean +
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_AAE10 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_K ~ 1, beta_Mg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(0.1, rep(0, length(unique(D_Yield$crop)) - 1), 0, 0, 0, 0, 0, 0, 0, 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)

cat("### Yield ~ Raw P_CO2 (Mitscherlich NLME) ###\\n")
print(round(summary(m_yield_raw_co2)$tTable, 4))
cat("\\nPseudo-R2:", round(cor(D_Yield$Relative_Yield, predict(m_yield_raw_co2, level = 2))^2, 3), "\\n")
cat("RMSE (Conditional, includes site/plot):", round(sqrt(mean(residuals(m_yield_raw_co2, level = 2)^2)), 3), "\\n")
cat("RMSE (Marginal, fixed effects only):", round(sqrt(mean(residuals(m_yield_raw_co2, level = 0)^2)), 3), "\\n")

cat("\\n### Yield ~ Thermo a_CO2 (Mitscherlich NLME) ###\\n")
print(round(summary(m_yield_thm_co2)$tTable, 4))
cat("\\nPseudo-R2:", round(cor(D_Yield$Relative_Yield, predict(m_yield_thm_co2, level = 2))^2, 3), "\\n")
cat("RMSE (Conditional, includes site/plot):", round(sqrt(mean(residuals(m_yield_thm_co2, level = 2)^2)), 3), "\\n")
cat("RMSE (Marginal, fixed effects only):", round(sqrt(mean(residuals(m_yield_thm_co2, level = 0)^2)), 3), "\\n")

cat("\\n### Yield ~ Legacy P_AAE10 (Mitscherlich NLME) ###\\n")
print(round(summary(m_yield_raw_aae)$tTable, 4))
cat("\\nPseudo-R2:", round(cor(D_Yield$Relative_Yield, predict(m_yield_raw_aae, level = 2))^2, 3), "\\n")
cat("RMSE (Conditional, includes site/plot):", round(sqrt(mean(residuals(m_yield_raw_aae, level = 2)^2)), 3), "\\n")
cat("RMSE (Marginal, fixed effects only):", round(sqrt(mean(residuals(m_yield_raw_aae, level = 0)^2)), 3), "\\n")
```"""

# 2. residual-diagnostics-yield
resid_chunk_pattern = re.compile(r'```{r residual-diagnostics-yield,.*?```', re.DOTALL)
new_resid_chunk = """```{r residual-diagnostics-yield, fig.width=12, fig.height=8}
D_res_all <- dplyr::bind_rows(
    D_Yield |> mutate(Model = "Raw P_CO2", Fitted = predict(m_yield_raw_co2), Residual = residuals(m_yield_raw_co2)),
    D_Yield |> mutate(Model = "Thermo a_CO2", Fitted = predict(m_yield_thm_co2), Residual = residuals(m_yield_thm_co2)),
    D_Yield |> mutate(Model = "Legacy P_AAE10", Fitted = predict(m_yield_raw_aae), Residual = residuals(m_yield_raw_aae))
)

p_resid <- ggplot(D_res_all, aes(x = Fitted, y = Residual, color = site)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.4, size = 1.5) +
    facet_wrap(~Model, scales = "free_x", ncol=1) +
    labs(title = "Conditional Residuals vs Fitted", x = "Fitted Yield", y = "Residual", color = "Site") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

p_box <- ggplot(D_res_all, aes(x = site, y = Residual, fill = site)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    facet_wrap(~Model, ncol=1) +
    labs(title = "Yield Model Residuals by Site", x = "Site", y = "Residual", fill = "Site") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

(p_resid | p_box)
```"""

# 3. pcrit-analysis
pcrit_chunk_pattern = re.compile(r'```{r pcrit-analysis,.*?```', re.DOTALL)
new_pcrit_chunk = """```{r pcrit-analysis, fig.width=14, fig.height=12}
# ---------------------------------------------------------------------------
# P_crit = STP at which Y = 95% of maximum yield
#
# Since we modeled c_eff directly via NLME, we just compute it deterministically
# and plot the extracted effects without needing a redundant secondary lmer!
# ---------------------------------------------------------------------------

calc_pcrit <- function(model, name) {
    cf <- fixef(model)
    D_Yield |>
        mutate(
            Model = name,
            c_base_crop = cf["c_base.(Intercept)"] + tidyr::replace_na(cf[paste0("c_base.crop", crop)], 0),
            plot_full_id = paste0(site, "/", plot_nr),
            re_total = ranef(model)$site[as.character(site), 1] + ranef(model)$plot_nr[plot_full_id, 1],
            c_eff  = (c_base_crop + tidyr::replace_na(re_total, 0)) * exp(
                cf["beta_invb"] * z_inv_b +
                cf["beta_pH"] * z_pH +
                cf["beta_K"] * z_ln_K +
                cf["beta_Mg"] * z_ln_Mg +
                cf["beta_N"] * z_fert_N +
                cf["beta_Temp"] * z_Temp_Mean +
                cf["beta_Prec"] * z_Prec_Anom
            ),
            P_crit = (log(20) / c_eff) - cf['E_base'],
            ln_P_crit = log(P_crit),
            crop   = as.factor(crop)
        ) |>
        filter(is.finite(ln_P_crit))
}

D_Pcrit_all <- dplyr::bind_rows(
    calc_pcrit(m_yield_raw_co2, "Raw P_CO2"),
    calc_pcrit(m_yield_thm_co2, "Thermo a_CO2"),
    calc_pcrit(m_yield_raw_aae, "Legacy P_AAE10")
)

# --- Visualizations ---
# 1. P_crit distributions by site
p_pcrit_box <- ggplot(D_Pcrit_all, aes(x = site, y = P_crit, fill = site)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21) +
    facet_wrap(~Model, scales = "free_y", ncol = 1) +
    labs(title = "Critical STP Thresholds per Site",
        subtitle = "Derived from One-Step NLME Mitscherlich",
        x = "Site", y = "Critical P Quantity/Intensity", fill = "Site") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

# 2. Forest plot: drivers of Mitscherlich rate constant (c)
extract_effects <- function(model, name) {
    broom.mixed::tidy(model, effects = "fixed") |>
        filter(!grepl("c_base|E_base", term)) |>
        mutate(
            Model = name,
            lower = estimate - 1.96 * std.error,
            upper = estimate + 1.96 * std.error,
            estimate_inv = -estimate,
            lower_inv = -upper,
            upper_inv = -lower,
            term_clean = case_when(
                term == "beta_invb" ~ "Physical Buffer Power (1/b)",
                term == "beta_pH"   ~ "Soil pH",
                term == "beta_K"    ~ "Soil Extractable K",
                term == "beta_Mg"   ~ "Soil Extractable Mg",
                term == "beta_N"    ~ "Nitrogen Fertilizer",
                term == "beta_Temp" ~ "Mean Annual Temperature",
                term == "beta_Prec" ~ "Precipitation Anomaly"
            ),
            sig = ifelse(p.value < 0.05, "p < 0.05", "p \u2265 0.05")
        )
}

nlme_effects_all <- dplyr::bind_rows(
    extract_effects(m_yield_raw_co2, "Raw P_CO2"),
    extract_effects(m_yield_thm_co2, "Thermo a_CO2"),
    extract_effects(m_yield_raw_aae, "Legacy P_AAE10")
)

p_pcrit_forest <- ggplot(nlme_effects_all, aes(x = estimate, y = reorder(term_clean, estimate), color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, linewidth = 0.9) +
    geom_point(size = 4) +
    facet_wrap(~Model, ncol = 1) +
    scale_color_manual(values = c("p < 0.05" = "#2c7bb6", "p \u2265 0.05" = "gray60")) +
    labs(title = "Drivers of P-Foraging Efficiency (Rate Constant c)",
        subtitle = "Negative = Slower uptake (Requires higher P_crit)",
        x = "Standardised Coefficient (log scale)", y = "", color = "") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

# 3. Forest plot: drivers of P_crit (1/c)
p_pcrit_forest_inv <- ggplot(nlme_effects_all, aes(x = estimate_inv, y = reorder(term_clean, estimate_inv), color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = lower_inv, xmax = upper_inv), height = 0.2, linewidth = 0.9) +
    geom_point(size = 4) +
    facet_wrap(~Model, ncol = 1) +
    scale_color_manual(values = c("p < 0.05" = "#d73027", "p \u2265 0.05" = "gray60")) +
    labs(title = "Drivers of the Critical P Threshold (P_crit)",
        subtitle = "Positive = Increases the required P_crit (worse foraging)",
        x = "Standardised Coefficient (effect on P_crit)", y = "", color = "") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

(p_pcrit_box | p_pcrit_forest | p_pcrit_forest_inv) + plot_layout(widths = c(1, 1.2, 1.2))
```"""

# Apply regex replacements
content = yield_chunk_pattern.sub(new_yield_chunk, content)
content = resid_chunk_pattern.sub(new_resid_chunk, content)
content = pcrit_chunk_pattern.sub(new_pcrit_chunk, content)

with open('notebooks/qi_modelling1.qmd', 'w') as f:
    f.write(content)

print("Successfully replaced chunks in qi_modelling1.qmd")
