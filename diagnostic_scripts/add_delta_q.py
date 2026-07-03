import os

new_chunk = """
## ----delta-q-analysis, fig.width=8, fig.height=6------------------------------
# 6. Translating Intensity (P_crit) into a Practical Fertilizer Prescription (Delta Q)
# We focus on the "True Negative" quadrant (soils correctly identified as deficient).
# We want to calculate exactly how much P_AAE10 (Quantity) needs to be built up 
# to reach the target P_CO2 (Intensity).

def_data <- quad_co2 |>
    dplyr::filter(Quadrant == "True Negative (Correct Warning)")

# Safely predict the target Quantity by overriding the ln_P_CO2 column temporarily
# and using the established PTF fixed-effects (re.form = NA).
def_data$ln_P_CO2_actual <- def_data$ln_P_CO2
def_data$ln_P_CO2 <- log(def_data$P_crit_loo)

# Predict target legacy pool (ln scale)
def_data$pred_ln_Q <- predict(ptf_practical_raw, newdata = def_data, re.form = NA)

# Restore original column and calculate Delta Q
def_data$ln_P_CO2 <- def_data$ln_P_CO2_actual
def_data <- def_data |>
    dplyr::mutate(
        Q_crit = exp(pred_ln_Q),
        Delta_Q = Q_crit - soil_0_20_P_AAE10,
        Yield_Gap = 0.95 - Relative_Yield
    )

# Display a summary
cat("### Calculated P Deficit (Delta Q) Summary for Deficient Plots ###\\n")
print(summary(def_data$Delta_Q))

# Plot the Prescription Validation
p_delta_q <- ggplot(def_data, aes(x = Delta_Q, y = Yield_Gap, color = site)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dotted") +
    geom_vline(xintercept = 0, color = "gray50", linetype = "dotted") +
    labs(
        title = "Agronomic Prescription Validation: P Deficit vs Yield Penalty",
        subtitle = "Focusing on correctly identified deficient plots (True Negatives).",
        x = expression(Delta*Q~" (Target "*P[AAE10]*" - Actual "*P[AAE10]*", mg/kg)"),
        y = "Yield Gap (0.95 - Relative Yield)",
        color = "Site"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
    )

print(p_delta_q)
"""

def insert_before_closing_ticks(filepath):
    if not os.path.exists(filepath):
        return
        
    with open(filepath, 'r') as f:
        content = f.read()
        
    if "Translating Intensity (P_crit) into a Practical Fertilizer" in content:
        print(f"Delta Q analysis already exists in {filepath}")
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
    print(f"Appended Delta Q analysis to {filepath}")

insert_before_closing_ticks('notebooks/qi_modelling1.R')
insert_before_closing_ticks('notebooks/qi_modelling1.qmd')
