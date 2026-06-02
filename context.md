# PROJECT OVERVIEW
**Title:** P-release kinetics as a predictor for P-availability in Swiss cropping systems.
**Dataset:** The long-term Swiss agricultural experiment (STYCS), specifically utilizing the expanded dataset including P, Mg, K, and Ca trials spanning multiple pedo-climatic zones.
**Core Objective:** Demonstrating that kinetic parameters derived from sequential extractions ($P_{desorb}$ and rate $k$) serve as vastly superior functional proxies for long-term P status and sustainability compared to traditional static soil tests (STPs like $P_{CO2}$ and $P_{AAE10}$).

# KEY SCIENTIFIC BREAKTHROUGHS & DECISIONS
1. **The Thermodynamic Fix (Davies Equation):**
   * *Issue:* Heavy Ca and Mg treatments artificially crowd the soil solution, altering raw mass extraction ($P_{CO2}$) and confusing standard models.
   * *Solution:* We implemented the Davies equation to convert raw mass into thermodynamic effective concentration ($a_{CO2}$). This strips away ionic interference and proves the underlying chemical phase equilibrium is stable across all treatments.
2. **Modeling Mass Flux vs. Tissue Concentration:**
   * *Issue:* Modeling plant tissue concentration ($C_P$) is highly vulnerable to the Dilution Trap (massive biomass diluting P) and the Piper-Steenbjerg effect (stunted plants showing artificially high $C_P$).
   * *Solution:* We strictly model absolute **P-Uptake** (Yield $\times C_P$) because it represents the true physical mass flux delivered by the soil's buffer power ($1/b$), independent of the plant's internal stoichiometric drama.
3. **Handling Co-Limitation (N & K):**
   * *Issue:* Sink limitations caused by Nitrogen or Potassium deficiency can halt P-uptake regardless of optimal soil supply.
   * *Solution:* We use relative yield/uptake normalizations at the site level and explicit climate ceiling variables ($\beta_{temp}$) to isolate the soil chemistry from biological and climatic constraints.

# STATISTICAL VALIDATION STRATEGY
* **Spatial Out-of-Sample Validation:** We rejected naive random sampling. We are using **Leave-One-Site-Out Cross-Validation (LOSO CV)**. 
* By training on 4 STYCS sites and testing on the 5th (which includes extreme ionic variance from Ca/Mg/K trials), we force the model to prove that the thermodynamic equations are generalized physical laws, not localized overfits. We explicitly excluded the DOK (organic) trial for now to prevent scope creep into biological P-mining via root exudates.

# INFRASTRUCTURE & BUG FIXES
* **The CI/CD Timeout:** GitHub Actions initially timed out (6+ hours) because the runner was attempting to compute heavy `rlmer`/`nlme` models live on 2-core VMs. 
* **The Fix:** We implemented Quarto's `freeze: auto` (and forced `freeze: true` during debugging) in `_quarto.yml` and the `.qmd` headers. Models are now computed locally on an Ubuntu VM (or HPC cluster), generating a `_freeze/` directory. GitHub Actions now only stitches the pre-computed HTML together, deploying in seconds. 
* **Path Management:** Replaced relative paths (`../data/`) with `here::here("data/")` to eliminate working directory collisions between RStudio interactive execution and Quarto terminal rendering.



Here is a formal summary of our modeling philosophy, variable selection, and theoretical framework. You can append this directly to your `CONTEXT.md` file so your new agent understands exactly *why* the models are structured the way they are.

---

### 3. Theoretical Framework & Model Formalization

#### **The Core Philosophy: Mass Flux vs. Empirical Correlation**

The central thesis of this manuscript is that soil phosphorus availability cannot be accurately predicted by static, empirical mass extractions (Standard Soil Tests / STPs). Instead, it must be modeled as a dynamic, biophysical system where fundamental pedochemistry (the source) dictates the mass flux of P to the plant root (the sink).

To achieve this, we formalized models that isolate the chemical phase equilibrium of the soil from the physiological noise of the plant and the climatic noise of the environment.

#### **Predictor Selection & Rationale**

**1. Thermodynamic Activity ($a_{CO2}$) vs. Raw Concentration ($P_{CO2}$)**

* **The Choice:** We strictly rely on $a_{CO2}$ calculated via the Davies equation.
* **The Rationale:** The STYCS dataset contains experimental trials heavily loaded with Ca, Mg, and K. These treatments massively increase the **Ionic Strength ($I$)** of the soil pore water. Raw mass extractions ($P_{CO2}$) fail here because the crowded ions suppress the effective solubility and mobility of P. By mathematical converting raw mass to thermodynamic activity ($a_{CO2}$), we strip away the ionic interference. This proves that the underlying solid-liquid phase equilibrium (your physical buffer power, $1/b$) remains stable across all treatments, demonstrating a universal physical law rather than a localized correlation.

**2. Absolute P-Uptake vs. Tissue Concentration ($C_P$)**

* **The Choice:** We model $P_{Uptake}$ ($Yield \times C_P$) as the biological target variable, actively avoiding direct modeling of tissue concentration ($C_P$).
* **The Rationale:** $C_P$ is physiologically volatile. If a plant grows perfectly due to optimal N and weather, massive biomass dilutes the P ($C_P$ looks artificially low: the **Dilution Trap**). If a plant is stunted by drought or N-starvation, it accumulates P in tiny leaves ($C_P$ looks artificially high: the **Piper-Steenbjerg Effect**). $P_{Uptake}$ calculates the exact, absolute mass of P physically extracted from the soil, directly matching the physical mass flux predicted by our kinetic parameters ($P_{desorb}$ and $k$).

**3. Relative Normalizations & Climate Ceilings ($\beta_{temp}$)**

* **The Choice:** We predict relative yield/uptake (normalized per site) and include abiotic covariates like temperature anomalies.
* **The Rationale (Liebig’s Law):** Plant uptake requires a biological sink. Co-limitations (like Nitrogen starvation in certain plots) cause the plant to stop growing, artificially capping P-uptake regardless of how well the soil supplies it. By normalizing within sites and factoring out climate ceilings (heat stress), we isolate the biological sink limitations. This ensures the model does not mathematically blame the soil chemistry ($1/b$) when the actual failure was weather or N-management.

#### **Model Architecture Summary**

* **The Baseline (Null) Models:** Traditional static predictions ($Y \sim P_{CO2}$ or $Y \sim P_{AAE10}$).
* **The Kinetic Models:** Non-linear derivations of maximum equilibrium intensity ($P_{desorb}$) and rate constants ($k$), combined into functional proxies like buffer power ($1/b$).
* **The Ultimate Validation (LOSO-CV):** Spatial Leave-One-Site-Out Cross-Validation. By training the kinetic/thermodynamic models on 4 distinct pedo-climatic sites and testing on a 5th unseen site containing extreme Ca/Mg/K gradients, we prove the framework's generalized physical validity.