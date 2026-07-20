import re

with open("presentation/index.qmd", "r") as f:
    text = f.read()

# 1. Refactor Piper-Steenbjerg Paradox Slide
piper_orig = """## The Target Variable: The Piper-Steenbjerg Paradox { .scrollable .smaller }

<details>
<summary>Why model Uptake and Yield instead of Concentration?</summary>
Statistically speaking, modeling plant tissue concentration ($C_P$) directly would be much cleaner. It is a direct measurement of the plant's chemical state. However, the biological reality of the **Piper-Steenbjerg effect** (growth dilution) destroys this statistical convenience. 

When a P-starved plant finally absorbs Phosphorus, it rapidly generates new biomass. This sudden flush of carbon paradoxically *dilutes* the $C_P$ measurement, causing the concentration to drop even though the absolute mass of Phosphorus in the plant increased. 
</details>

**The necessity of cumulative flow:**

- **The Illusion of $C_P$:** Because of the dilution effect, a low $C_P$ is biologically ambiguous. It could mean severe starvation, or it could mean explosive, healthy growth.
- **The Solution:** We are forced to abandon $C_P$ as a direct target. Instead, we must model the cumulative physical flow: **Absolute Uptake ($P_{up} = Y \\times C_P$)** or **Relative Yield ($Y$)**. 
- **The Trade-off:** While absolute Uptake and Yield are fundamentally correct biological targets, Yield ($Y$) is extremely noisy because it integrates every other environmental factor (weather, nitrogen, pests) across the entire season. Our mechanistic models must explicitly account for this noise."""

piper_new = """## The Target Variable: The Piper-Steenbjerg Paradox { .scrollable .smaller }

**The Illusion of Tissue Concentration ($C_P$):**
- **Biological Ambiguity:** A low $C_P$ could mean severe starvation, OR it could mean explosive, healthy growth. 
- **The Cause:** When a P-starved plant absorbs Phosphorus, it rapidly generates new biomass, paradoxically *diluting* the $C_P$ measurement (The Piper-Steenbjerg effect).
- **The Solution:** We must model the cumulative physical flow instead: **Absolute Uptake ($P_{up}$)** or **Relative Yield ($Y$)**.
- **The Trade-off:** Yield integrates immense environmental noise (weather, nitrogen, pests). Our mechanistic models must explicitly absorb this variance.

::: {.callout-note collapse="true" title="Deep Dive: Why model Uptake and Yield instead of Concentration?"}
Statistically speaking, modeling plant tissue concentration ($C_P$) directly would be much cleaner, as it is a direct measurement of the plant's chemical state. However, the biological reality of the Piper-Steenbjerg effect (growth dilution) destroys this statistical convenience. When a P-starved plant finally absorbs Phosphorus, it rapidly generates new biomass. This sudden flush of carbon dilutes the $C_P$ measurement, causing the concentration to drop even though the absolute mass of Phosphorus in the plant actually increased.
:::

::: {.notes}
Statistically speaking, modeling plant tissue concentration ($C_P$) directly would be much cleaner, as it is a direct measurement of the plant's chemical state. However, the biological reality of the Piper-Steenbjerg effect (growth dilution) destroys this statistical convenience. When a P-starved plant finally absorbs Phosphorus, it rapidly generates new biomass. This sudden flush of carbon dilutes the $C_P$ measurement, causing the concentration to drop even though the absolute mass of Phosphorus in the plant actually increased.
:::"""
text = text.replace(piper_orig, piper_new)

# 2. Extract and Remove Yield Model from Annex
# We find the slide starting with "## 3. Mechanistic Yield Model" until the next "## 4."
yield_pattern = re.compile(r"(## 3\. Mechanistic Yield Model \{ \.scrollable \.smaller \}.*?)(?=\n## 4\. Mechanistic Uptake Model)", re.DOTALL)
yield_match = yield_pattern.search(text)
if yield_match:
    yield_slide_content = yield_match.group(1)
    # Remove from annex
    text = text.replace(yield_slide_content, "")
    
    # Rename it to remove the "3."
    yield_slide_content = yield_slide_content.replace("## 3. Mechanistic Yield Model", "## The Mechanistic Yield Model")
else:
    yield_slide_content = ""

# 3. Insert Yield Model and New CP model after "The Mechanistic Uptake Model" slide
# Find where the Uptake slide ends. It ends at "## The $P_{crit}$ Target & Agronomic Goals"
uptake_end_pattern = re.compile(r"(?=\n## The \$P_\{crit\}\$ Target \\& Agronomic Goals)")

new_cp_model = """
---

## The Unified Tissue Concentration Model { .scrollable .smaller }

Instead of only targeting Uptake or Yield, we can mathematically unify them by directly parameterizing the **Tissue Concentration ($C_P$)** using kinetics.

**The Exponential-Linear Framework:**
$$ C_P = C_{base} + A_{dil}\\exp(-k \\cdot I) + S_{acc} \\cdot I $$

- **Dilution Phase ($A_{dil}\\exp(-k \\cdot I)$):** The explosive drop in concentration driven by rapid biomass expansion.
- **Luxury Accumulation ($S_{acc} \\cdot I$):** The linear gorging of excess Phosphorus into vegetative tissues (straw/leaves) once yield plateaus.

**Integrating Soil Chemistry:**
- **Baseline Nutrition:** Soil **pH** and **Texture** modify the baseline concentration ($C_{base}$).
- **Luxury Kinetics:** The soil's **Diffusion Penalty** ($1/b$) and **Desorption Velocity** ($v_0$) act as hard bottlenecks on the luxury accumulation slope ($S_{acc}$):
  $$ S_{acc\\_eff} = S_{base} \\cdot \\exp\\left(\\beta_{invb} \\cdot \\frac{1}{b} + \\beta_{v0} \\cdot v_0\\right) $$

::: {.callout-note collapse="true" title="Deep Dive: Statistical Proof of Covariates"}
When fit using a Non-Linear Mixed-Effects (NLME) framework across all field trials, both the diffusion penalty (1/b) and the desorption velocity (v0) returned highly significant p-values, proving that luxury consumption is strictly governed by soil physics. The unified framework captures the exact biological stoichiometry of the crop architecture without relying on arbitrary targets.
:::

::: {.notes}
When fit using a Non-Linear Mixed-Effects (NLME) framework across all field trials, both the diffusion penalty (1/b) and the desorption velocity (v0) returned highly significant p-values, proving that luxury consumption is strictly governed by soil physics. The unified framework captures the exact biological stoichiometry of the crop architecture without relying on arbitrary targets.
:::
"""

if yield_slide_content:
    insertion = "\n---\n\n" + yield_slide_content.strip() + "\n" + new_cp_model
    # Insert right before the P_crit target slide
    text = uptake_end_pattern.sub(lambda m: insertion + "\n", text, count=1)


# 4. Refactor The Q(I) Model & Desorption Kinetics
qi_orig = """## The Q(I) Model & Desorption Kinetics { .scrollable .smaller }

At the core of the mechanistic approach is the Quantity/Intensity ($Q/I$) relationship. This framework physically defines how much Phosphorus is bound to the soil matrix ($Q$, Quantity) versus how much is dissolved in the soil solution and available for immediate plant uptake ($I$, Intensity).

- **The Freundlich Isotherm:** 
  We model this relationship using the non-linear Freundlich equation:
  $$ Q = K \cdot I^n $$
  Where $K$ represents the total binding capacity of the soil, and $n$ describes the curvature of the adsorption sites.

- **The Buffer Power ($b$):** 
  The instantaneous ability of the soil to replenish the solution as the plant roots extract $P$. It is defined as the derivative of the $Q/I$ curve:
  $$ b = \\frac{dQ}{dI} = n \cdot K \cdot I^{n-1} $$

- **The Desorption Velocity ($v_0$):**
  A critical kinetic parameter representing the maximum initial speed at which $P$ releases from the solid phase into a pure sink. High Buffer Power often inversely restricts $v_0$.
"""

qi_new = """## The Q(I) Model & Desorption Kinetics { .scrollable .smaller }

The **Quantity/Intensity ($Q/I$)** relationship mathematically defines the physical bottleneck of soil phosphorus.

- **Quantity ($Q$):** Total Phosphorus bound to the soil matrix.
- **Intensity ($I$):** Phosphorus dissolved in solution (immediately available).
- **The Equation (Freundlich):** $$ Q = K \cdot I^n $$
  - $K$: Total binding capacity.
  - $n$: Curvature of adsorption sites.
- **The Buffer Power ($b = \\frac{dQ}{dI}$):** The soil's ability to resist changes in Intensity as plants extract P. 
- **Desorption Velocity ($v_0$):** The kinetic speed limit at which solid-phase P releases into solution. 

::: {.callout-note collapse="true" title="Deep Dive: The Physical Bottlenecks"}
At the core of the mechanistic approach is the Quantity/Intensity relationship. We model this using the non-linear Freundlich equation. The Buffer Power ($b = n \\cdot K \\cdot I^{n-1}$) represents the instantaneous ability of the soil to replenish the solution. High Buffer Power implies that the soil tightly grips the Phosphorus, imposing a severe "Diffusion Penalty" ($1/b$) on root foraging. Furthermore, the Desorption Velocity ($v_0$) represents the maximum initial speed at which P releases from the solid phase into a pure sink. High Buffer Power often inversely restricts $v_0$.
:::

::: {.notes}
At the core of the mechanistic approach is the Quantity/Intensity relationship. We model this using the non-linear Freundlich equation. The Buffer Power ($b = n \\cdot K \\cdot I^{n-1}$) represents the instantaneous ability of the soil to replenish the solution. High Buffer Power implies that the soil tightly grips the Phosphorus, imposing a severe "Diffusion Penalty" ($1/b$) on root foraging. Furthermore, the Desorption Velocity ($v_0$) represents the maximum initial speed at which P releases from the solid phase into a pure sink. High Buffer Power often inversely restricts $v_0$.
:::"""
text = text.replace(qi_orig, qi_new)


with open("presentation/index.qmd", "w") as f:
    f.write(text)

print("Presentation successfully refactored.")
