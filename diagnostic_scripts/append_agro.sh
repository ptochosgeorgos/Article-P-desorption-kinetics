cat << 'EOF' > add_agronomic_recommendation.py
import re

qmd_path = 'notebooks/qi_modelling1.qmd'
with open(qmd_path, 'r') as f:
    content = f.read()

new_section = """
## 12. Practical Agronomic Recommendation: The Non-Linear Integral

Historically, soil science approximated the buffer capacity ($b$) as a linear constant ($d^2Q/dI^2 = 0$). Agronomists forced a straight line onto a non-linear system, leading to arbitrary "correction factors" to handle the extremes:
$$\\Delta Q \\approx b \\cdot (P_{crit} - P_{CO2})$$

Because Phosphorus follows a Freundlich isotherm, $b$ is a function of Intensity $b(I)$. The true fertilizer deficit is the exact mathematical integral of the non-linear buffer curve:
$$\\Delta Q_{lab} = \\int_{P_{CO2}}^{P_{crit}} b(I) dI = Q(P_{crit}) - Q(P_{CO2})$$

By using our Pedotransfer Function (PTF) to predict $Q$ directly, we automatically calculate the exact analytical integral $\\Delta Q_{lab}$, completely bypassing the need for linear approximations or destructive laboratory buffer measurements. 

### Converting to Field-Scale (kg P / ha)
To turn this exact thermodynamic deficit into a practical agronomic recommendation, we convert the lab measurement (mg P / kg soil) to a field recommendation (kg P / ha) by assuming standard physical properties for the soil layer:
- **Depth:** 30 cm ($0.30$ m)
- **Bulk Density:** $1.2 \\text{ g/cm}^3$ ($1200 \\text{ kg/m}^3$)
- **Mass per Hectare:** $10,000 \\text{ m}^2 \\times 0.30 \\text{ m} \\times 1200 \\text{ kg/m}^3 = 3,600,000 \\text{ kg soil / ha}$

This yields a conversion multiplier of **3.6**.
$$\\Delta Q_{field} \\text{ (kg P/ha)} = \\Delta Q_{lab} \\times 3.6$$

Our continuous, mechanistically rigorous fertilizer recommendation rule is therefore an affine function:
$$P_{fert} = P_{up} + \\left( \\frac{\\Delta Q_{field}}{T} \\right)$$
*(Where $T$ is the amortization period in years, allowing the farmer to fix the historical soil deficit gradually over a crop rotation).*

```{r field-recommendation, fig.width=10, fig.height=5}
# Calculate the real-world field deficit (kg P / ha) for all deficient plots
# We use def_data_acidic to ensure we only evaluate plots within the model's valid boundary (pH < 7.2)
def_data_field <- def_data_acidic |>
    dplyr::mutate(
        Delta_Q_field = Delta_Q * 3.6,
        Amortized_5_Year = Delta_Q_field / 5
    )

# Summary of the required P additions
cat("### Real-World Deficit Summary (kg P/ha) ###\\n")
print(summary(def_data_field$Delta_Q_field))

# Plot the distribution of required field additions
p_field <- ggplot(def_data_field, aes(x = Delta_Q_field, fill = site)) +
    geom_histogram(bins=30, color="black", alpha=0.8) +
    labs(
        title = "Distribution of Soil Phosphorus Deficits (kg P / ha)",
        subtitle = "Based on the exact non-linear integral to reach optimal P_crit",
        x = "Total Required Soil-Building P Addition (kg P / ha)",
        y = "Number of Plot-Years"
    ) +
    theme_minimal() +
    theme(legend.position="bottom")

print(p_field)
```

**Conclusion:** 
When a soil is deficient, our model outputs a direct, physically rigorous mass of P needed to fix the thermodynamic buffer deficit ($\Delta Q_{field}$). If the soil is already at or above optimal equilibrium ($P_{CO2} \ge P_{crit}$), the deficit is zero (or negative), and the farmer simply applies maintenance fertilizer ($P_{fert} = P_{up}$) or mines the soil ($P_{fert} = 0$). This completely replaces empirical discrete lookup tables with a single, continuous physical equation.
"""

# Insert right after the end of the previous chunk
lines = content.split('\n')
for i in range(len(lines)-1, -1, -1):
    if lines[i].strip() == '```':
        lines.insert(i+1, new_section)
        break

with open(qmd_path, 'w') as f:
    f.write('\n'.join(lines))
print("Successfully appended Agronomic Recommendation section.")
EOF
python3 add_agronomic_recommendation.py
quarto render notebooks/qi_modelling1.qmd
