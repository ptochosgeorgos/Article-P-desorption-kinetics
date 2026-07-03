import re

def update_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()
    
    # 1. Update RMSE printing
    old_rmse = 'cat("RMSE:", round(sqrt(mean(residuals(m_yield_nlme)^2)), 3), "\\n")'
    new_rmse = 'cat("RMSE (Conditional, includes site/plot):", round(sqrt(mean(residuals(m_yield_nlme, level = 2)^2)), 3), "\\n")\ncat("RMSE (Marginal, fixed effects only):", round(sqrt(mean(residuals(m_yield_nlme, level = 0)^2)), 3), "\\n")'
    if old_rmse in content:
        content = content.replace(old_rmse, new_rmse)

    # 2. Update pcrit plot
    old_plot = '(p_pcrit_box | p_pcrit_forest) + plot_layout(widths = c(1, 1.5))'
    
    new_plot_code = """
# 3. Forest plot: drivers of P_crit (1/c)
nlme_effects_inv <- nlme_effects |>
    dplyr::mutate(
        estimate_inv = -estimate,
        lower_inv = -upper,
        upper_inv = -lower
    )

p_pcrit_forest_inv <- ggplot(nlme_effects_inv, aes(x = estimate_inv, y = reorder(term_clean, estimate_inv), color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = lower_inv, xmax = upper_inv), height = 0.2, linewidth = 0.9) +
    geom_point(size = 4) +
    scale_color_manual(values = c("p < 0.05" = "#d73027", "p ≥ 0.05" = "gray60")) +
    labs(title = "Drivers of the Critical P Threshold (P_crit)",
        subtitle = "Positive = Increases the required P_crit (worse foraging)",
        x = "Standardised Coefficient (effect on P_crit)", y = "", color = "") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

(p_pcrit_box | p_pcrit_forest | p_pcrit_forest_inv) + plot_layout(widths = c(1, 1.2, 1.2))
"""
    if old_plot in content:
        content = content.replace(old_plot, new_plot_code)
        
        # also update fig.width in qmd if it's there
        content = re.sub(r'```{r pcrit-analysis, fig.width=10, fig.height=5}', '```{r pcrit-analysis, fig.width=14, fig.height=5}', content)
        # and in R script comments
        content = re.sub(r'## ----pcrit-analysis, fig.width=10, fig.height=5', '## ----pcrit-analysis, fig.width=14, fig.height=5', content)

    with open(filepath, 'w') as f:
        f.write(content)

update_file('notebooks/qi_modelling1.R')
update_file('notebooks/qi_modelling1.qmd')
print("Successfully added Marginal RMSE and P_crit forest plot to both files.")
