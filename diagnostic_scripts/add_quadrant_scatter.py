import os

new_plot_chunk = """
# 5. Scatter Plot of Quadrants (Normalized by P_crit)
quad_co2_plot <- quad_co2 |> dplyr::mutate(P_Ratio = P_test / P_crit_loo, Extractant = "P_CO2 (Intensity)")
quad_aae_plot <- quad_aae |> dplyr::mutate(P_Ratio = P_test / P_crit_loo, Extractant = "P_AAE10 (Legacy Pool)")
quad_all <- dplyr::bind_rows(quad_co2_plot, quad_aae_plot)

p_scatter <- ggplot(quad_all, aes(x = P_Ratio, y = Relative_Yield, color = Quadrant)) +
    geom_point(alpha = 0.3, size = 1) +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "black", linewidth = 0.8) +
    geom_vline(xintercept = 1.0, linetype = "dashed", color = "black", linewidth = 0.8) +
    scale_color_manual(values = c(
        "True Positive (Success)" = "#1a9641",
        "True Negative (Correct Warning)" = "#a6d96a",
        "False Positive (Failure)" = "#d7191c",
        "False Negative (Over-fertilized)" = "#fdae61",
        "NA" = "gray"
    )) +
    facet_wrap(~Extractant, scales = "free_x") +
    scale_x_log10(labels = scales::comma) +
    labs(
        title = "Predictive Quadrant Analysis: Normalized P vs Yield",
        subtitle = "Vertical line: Predicted P_crit threshold. Horizontal line: 95% Yield Target.",
        x = "Ratio: Actual Soil P / Predicted P_crit (Log Scale)", 
        y = "Relative Yield"
    ) +
    theme_minimal(base_size = 11) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 11)
    ) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3), nrow = 2))

print(p_scatter)
"""

def insert_before_closing_ticks(filepath):
    if not os.path.exists(filepath):
        return
        
    with open(filepath, 'r') as f:
        content = f.read()
        
    if "Scatter Plot of Quadrants" in content:
        print(f"Scatter plot already exists in {filepath}")
        return
        
    if filepath.endswith('.qmd'):
        # Find the last ``` and insert before it
        lines = content.split('\n')
        for i in range(len(lines)-1, -1, -1):
            if lines[i].strip() == '```':
                lines.insert(i, new_plot_chunk)
                break
        content = '\n'.join(lines)
    else:
        content += new_plot_chunk + "\n"
        
    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Appended scatter plot to {filepath}")

insert_before_closing_ticks('notebooks/qi_modelling1.R')
insert_before_closing_ticks('notebooks/qi_modelling1.qmd')
