import os

new_chunk = """
# 7. Geochemical Boundary Condition: Filtering for pH < 7.0
# In highly alkaline/calcareous soils (pH > 7.0), P dynamics are governed by calcium phosphate precipitation 
# (e.g., apatite) rather than simple Fe/Al oxide adsorption. Our Fe/Al buffer-based model (1/b) correctly extrapolates
# that to reach high solution P in such calcareous soils, you would need absurd amounts of P (equivalent to pure apatite).
# Therefore, we bound the agronomic prescription model to acidic-to-neutral soils where the Fe/Al buffer primarily operates.

def_data_acidic <- def_data |> dplyr::filter(rollMean_soil_0_20_pH_H2O < 7.0)

# Display a summary for acidic/neutral soils
cat("\\n### Calculated P Deficit (Delta Q) Summary for Deficient Acidic/Neutral Plots (pH < 7) ###\\n")
print(summary(def_data_acidic$Delta_Q))

p_delta_q_acidic <- ggplot(def_data_acidic, aes(x = Delta_Q, y = Yield_Gap, color = site)) +
    geom_point(alpha = 0.8, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dotted") +
    geom_vline(xintercept = 0, color = "gray50", linetype = "dotted") +
    labs(
        title = "Agronomic Prescription Validation (pH < 7.0)",
        subtitle = "Geochemically bounded: P Deficit vs Yield Penalty in Acidic-to-Neutral soils.",
        x = expression(Delta*Q~" (Target "*P[AAE10]*" - Actual "*P[AAE10]*", mg/kg)"),
        y = "Yield Gap (0.95 - Relative Yield)",
        color = "Site"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
    )

print(p_delta_q_acidic)
"""

def insert_before_closing_ticks(filepath):
    if not os.path.exists(filepath):
        return
        
    with open(filepath, 'r') as f:
        content = f.read()
        
    if "Geochemical Boundary Condition: Filtering for pH < 7.0" in content:
        print(f"pH filter analysis already exists in {filepath}")
        return
        
    if filepath.endswith('.qmd'):
        lines = content.split('\n')
        for i in range(len(lines)-1, -1, -1):
            if lines[i].strip() == '```':
                lines.insert(i, new_chunk)
                break
        content = '\n'.join(lines)
    else:
        content += new_chunk + "\n"
        
    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Appended pH filter analysis to {filepath}")

insert_before_closing_ticks('notebooks/qi_modelling1.R')
insert_before_closing_ticks('notebooks/qi_modelling1.qmd')
