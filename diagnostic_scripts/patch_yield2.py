import re

def patch_yield_model(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # 1. Update random effect
    content = content.replace("random = c_base ~ 1 | site,", "random = c_base ~ 1 | site/plot_nr,")

    # 2. Fix the partial effects plot c_base
    content = content.replace(
        'Predicted = 1 - exp(-cf["c_base"] * exp(cf[[beta_name]] * val) * P)',
        'c_base_mean <- mean(cf[grep("c_base", names(cf))])\n            Predicted = 1 - exp(-c_base_mean * exp(cf[[beta_name]] * val) * P)'
    )

    # 3. Add the forest plot diagnostic for beta effects on C right before the residual diagnostics
    forest_plot_code = """
### Drivers of the Rate Constant (c)
The following forest plot visualizes the effect size of each standardized pedoclimatic driver on the rate constant $c$. 
Because the rate constant is modeled as $c = c_{base} \\cdot \\exp(\\sum \\beta_i Z_i)$, the x-axis represents the log-response ratio for a 1 standard deviation increase in each predictor. 
A negative effect means the environmental driver lowers the crop's P-foraging efficiency (slower yield response to soil P).

```{r forest-plot-c-drivers, fig.width=8, fig.height=5}
library(broom.mixed)
nlme_effects <- tidy(m_yield_raw_co2, effects = "fixed") |>
    filter(!grepl("c_base", term)) |>
    mutate(
        lower = estimate - 1.96 * std.error,
        upper = estimate + 1.96 * std.error,
        term_clean = case_when(
            term == "beta_invb" ~ "Physical Buffer Power (1/b)",
            term == "beta_b"    ~ "Buffer b",
            term == "beta_n"    ~ "Freundlich n",
            term == "beta_pH"   ~ "Soil pH",
            term == "beta_fertK" ~ "Potassium Fertilizer",
            term == "beta_fertMg" ~ "Magnesium Fertilizer",
            term == "beta_N"    ~ "Nitrogen Fertilizer",
            term == "beta_Temp" ~ "Mean Annual Temperature",
            term == "beta_Prec" ~ "Precipitation Anomaly",
            TRUE ~ term
        ),
        sig = ifelse(p.value < 0.05, "p < 0.05", "p ≥ 0.05")
    )

ggplot(nlme_effects, aes(x = estimate, y = reorder(term_clean, estimate), color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, linewidth = 0.9) +
    geom_point(size = 4) +
    scale_color_manual(values = c("p < 0.05" = "#2c7bb6", "p ≥ 0.05" = "gray60")) +
    labs(title = "Drivers of Yield P-Foraging Efficiency (Rate Constant c)",
         subtitle = "Negative = Slower uptake efficiency (requires higher STP to reach max yield)",
         x = "Standardised Coefficient (log scale effect on c)", y = "", color = "") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
```
"""

    if "### Residual Diagnostics per Site (Yield Models)" in content and "forest-plot-c-drivers" not in content:
        content = content.replace("### Residual Diagnostics per Site (Yield Models)", forest_plot_code + "\n### Residual Diagnostics per Site (Yield Models)")

    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Patched {filepath}")

patch_yield_model('notebooks/qi_modelling1.qmd')
patch_yield_model('notebooks/qi_modelling1.R')
patch_yield_model('notebooks/temp_qi.R')
