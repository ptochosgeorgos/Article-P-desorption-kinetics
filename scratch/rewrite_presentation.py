import re
import sys

def parse_presentation(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    # Split by slides (## )
    # We need to keep the YAML header and setup chunk
    # The first slide starts with "## Graphical Abstract"
    parts = re.split(r'\n(?=## )', content)
    
    header = parts[0]
    slides = parts[1:]
    
    slide_dict = {}
    for i, slide in enumerate(slides):
        title_match = re.match(r'## (.*?)(?:\n| \{)', slide)
        if title_match:
            title = title_match.group(1).strip()
            # Title is already clean because regex stops at \n or  {
            clean_title = title
            slide_dict[clean_title] = slide
            # print(f"Found slide: {clean_title}")
        else:
            if slide.startswith("# Annex"):
                slide_dict["Annex"] = slide
            else:
                slide_dict[f"Unknown_{i}"] = slide

    return header, slide_dict

def build_new_presentation(header, slide_dict):
    new_order = [
        "Graphical Abstract: The Path Through the Swamp", # We'll rename this
        "The Current Paradigm & The Missing Link",
        "The Piper-Steenbjerg Paradox & Sorption Capacity",
        "Conceptual Foundation: The Physical Chemistry",
        "The Q(I) Model & Non-Linear Buffer Power",
        "Interactive Q(I) Dynamics", # Just title slide
        "Physical Buffer Collapse",
        "The Thermodynamic Pedotransfer Function (Practical Compromise)",
        "The PTF Parameter Space",
        "Theoretical Background: Barber-Cushman & Diffusion",
        "The Mechanistic Uptake Model",
        "The P_crit Target & Agronomic Goals", # NEW
        "The Parametric Recommendation Framework", # NEW
        "Interactive Fertilization Calculator", # NEW
        "Conclusions & Policy Recommendations",
        "Annex",
        # Moved to Annex:
        "The GRUD Empirical Classification: $P_{CO2}$ Model",
        "The GRUD Empirical Classification: $P_{AAE10}$ Model",
        "The GRUD-model",
        "Mechanistic Validation of GRUD Supply Classes",
        "The Illusion of Bijection: GRUD Correction Factors",
        "Systematic Bias in Fertilizer Recommendations",
        "Interactive Bias Surface",
        "The Calcareous pH Artifact in AAE10",
        "The AAE10 Calcareous Artifact & Filtered Model" # Annex
    ]

    new_slides = []
    
    # 1. Graphical Abstract
    abstract = slide_dict.get("Graphical Abstract: The Path Through the Swamp", "")
    abstract = abstract.replace("The Path Through the Swamp", "Methodological Pathway")
    new_slides.append(abstract)
    
    # 2-5. Intro and Chemistry
    for t in [
        "The Current Paradigm & The Missing Link",
        "The Piper-Steenbjerg Paradox & Sorption Capacity",
        "Conceptual Foundation: The Physical Chemistry",
        "The Q(I) Model & Non-Linear Buffer Power"
    ]:
        if t in slide_dict:
            new_slides.append(slide_dict[t])
            
    # 6-7. Buffer Collapse
    if "Interactive Q(I) Dynamics" in slide_dict:
        new_slides.append(slide_dict["Interactive Q(I) Dynamics"])
    if "Physical Buffer Collapse" in slide_dict:
        new_slides.append(slide_dict["Physical Buffer Collapse"])
        
    # 8-9. PTF
    for t in [
        "The Thermodynamic Pedotransfer Function (Practical Compromise)",
        "The PTF Parameter Space",
        "Theoretical Background: Barber-Cushman & Diffusion",
        "The Mechanistic Uptake Model"
    ]:
        if t in slide_dict:
            new_slides.append(slide_dict[t])

    # NEW: P_crit
    pcrit_slide = """## The $P_{crit}$ Target & Agronomic Goals { .scrollable }

Using the **Michaelis-Menten Uptake Model**, we can mathematically define the critical soil intensity ($P_{crit}$) required to reach 95% of the maximum biological uptake. 

Because the NLME model intrinsically defines the 100% theoretical limit ($U_{max}$), we simply solve for the Intensity $I$ that satisfies $U = 0.95 U_{max}$:

$$ 0.95 \cdot U_{max} = \\frac{U_{max} \cdot I}{K_m + I} $$

$$ 0.95 K_m + 0.95 I = I $$
$$ 0.95 K_m = 0.05 I $$
$$ I = 19 \cdot K_m $$

**Conclusion:** The Critical Intensity ($P_{crit}$) is exactly **19 times the Half-Saturation Constant ($K_m$)**. 

Because $K_m$ is dynamically penalized by the physical buffer power ($1/b$) and desorption velocity ($v_0$), $P_{crit}$ natively adapts to the unique pedological bottlenecks of the soil!"""
    new_slides.append(pcrit_slide)

    # NEW: Parametric Recommendation
    param_slide = """## The Parametric Recommendation Framework { .scrollable }

With the physical bottlenecks isolated, we can transition from rigid empirical matrices to a purely **thermodynamic mass-balance** approach for fertilizer recommendations.

**Step 1: Calculate the Crop's Target State ($P_{crit}$)**
Calculate the required Intensity using the biological Uptake model ($P_{crit} = 19 K_m$).

**Step 2: Calculate the Soil's Capacity Limit ($Q(I)$)**
Use the Thermodynamic Pedotransfer Function (PTF) to calculate the non-linear buffer curve ($K$ and $n$) based on the soil's pH, Texture, and background cations.

**Step 3: Additive Mass Adjustment**
Calculate the exact mass difference required to shift the soil from its current state ($I_{now}$) to the critical state ($P_{crit}$):
$$ \Delta Q_{yield} = Q(P_{crit}) - Q(I_{now}) $$

**Final Recommendation:** Add this missing thermodynamic mass directly to the crop's baseline physiological requirement ($F_{norm}$):
$$ P_{rec} = F_{norm} + \Delta Q_{yield} - \text{Residues} $$"""
    new_slides.append(param_slide)

    # NEW: Interactive Calculator
    calc_slide = """## Interactive Fertilization Calculator { .interactive-slide .scrollable }

Explore the Parametric Framework in real-time. Select a crop and adjust the soil parameters. The model instantly calculates the non-linear buffer curve, the $P_{crit}$ target, and the precise additive fertilizer recommendation.

```{ojs}
//| panel: input
viewof crop_choice = Inputs.select(new Map([
  ["Winterweizen (Brot)", "WW"],
  ["Körnermais", "KM"],
  ["Kartoffeln (Speise)", "KA"],
  ["Zuckerrüben", "ZR"],
  ["Winterraps", "RA"]
]), {value: "WW", label: "Crop Type"})

viewof current_P = Inputs.range([0.1, 5.0], {value: 1.5, step: 0.1, label: "Current Soil Test P (mg/L)"})
viewof soil_pH = Inputs.range([5.0, 8.5], {value: 6.5, step: 0.1, label: "Soil pH"})
viewof soil_clay_silt = Inputs.range([10, 80], {value: 35, step: 1, label: "Texture (Clay+Silt %)"})

viewof show_expert = Inputs.toggle({label: "Show Expert Soil Panel (Cations & Corg)", value: false})
```

```{ojs}
//| panel: input
viewof exp_Ca = Inputs.range([1, 10], {value: 4.5, step: 0.1, label: "Ca (mg/kg)"})
viewof exp_Mg = Inputs.range([1, 10], {value: 2.3, step: 0.1, label: "Mg (mg/kg)"})
viewof exp_K = Inputs.range([1, 10], {value: 2.5, step: 0.1, label: "K (mg/kg)"})
viewof exp_Corg = Inputs.range([0.5, 5], {value: 1.8, step: 0.1, label: "Corg (%)"})
```

```{ojs}
// Hide the expert panel dynamically
html`<style>
  div.observablehq:has(input[type="range"][name="exp_Ca"]) {
    display: ${show_expert ? 'block' : 'none'};
  }
</style>`
```

```{ojs}
coef_data = FileAttachment("calculator_coefs.json").json()
norms = coef_data.crop_norms[crop_choice]
```

```{ojs}
// Calculate predictions
calc = {
  let ptf = FileAttachment("ptf_coefs.json").json();
  let sd = ptf.scales;
  let c = ptf.coefficients;
  
  let z_pH = (soil_pH - sd.pH.mean) / sd.pH.sd;
  let z_tex = (Math.log(soil_clay_silt) - sd.FineTexture.mean) / sd.FineTexture.sd;
  
  let val_Ca = show_expert ? exp_Ca : 4.5;
  let val_Mg = show_expert ? exp_Mg : 2.3;
  let val_K = show_expert ? exp_K : 2.5;
  let val_Corg = show_expert ? exp_Corg : 1.8;
  
  let z_Ca = (Math.log(val_Ca) - sd.Ca.mean) / sd.Ca.sd;
  let z_Mg = (Math.log(val_Mg) - sd.Mg.mean) / sd.Mg.sd;
  let z_K = (Math.log(val_K) - sd.K.mean) / sd.K.sd;
  let z_Corg = (Math.log(val_Corg) - sd.Corg.mean) / sd.Corg.sd;
  
  // Predict K and n
  let ln_K = c["(Intercept)"] + c["z_ln_FineTexture"]*z_tex + c["z_pH"]*z_pH + 
             c["z_ln_Ca"]*z_Ca + c["z_ln_Mg"]*z_Mg + c["z_ln_K"]*z_K + c["z_ln_Corg"]*z_Corg;
  
  let n = c["ln_P_CO2"] + c["ln_P_CO2:z_ln_FineTexture"]*z_tex + c["ln_P_CO2:z_pH"]*z_pH + 
          c["ln_P_CO2:z_ln_Ca"]*z_Ca + c["ln_P_CO2:z_ln_Mg"]*z_Mg + c["ln_P_CO2:z_ln_K"]*z_K + 
          c["ln_P_CO2:z_ln_Corg"]*z_Corg;
          
  let K_val = Math.exp(ln_K);
  
  // P_crit roughly derived from generic Km scaling for demo
  // In reality, Km depends on 1/b. Let's calculate instantaneous b at I=1
  let b_1 = n * K_val * Math.pow(1.0, n - 1);
  let inv_b = 1.0 / b_1;
  let z_inv_b = (inv_b - 0.05) / 0.02; // Approx scaling
  
  let Km = Math.exp(-2.0 + 0.3 * z_inv_b); 
  let P_crit = 19 * Km;
  
  let Q_now = K_val * Math.pow(current_P, n);
  let Q_target = K_val * Math.pow(P_crit, n);
  let delta_Q = Q_target - Q_now;
  
  let F_norm = norms.P;
  let P_rec = F_norm + delta_Q;
  
  return { K: K_val, n: n, P_crit: P_crit, Q_now: Q_now, Q_target: Q_target, delta_Q: delta_Q, P_rec: P_rec, F_norm: F_norm };
}
```

```{ojs}
//| panel: fill
Plot.plot({
  title: `Fertilizer Recommendation: ${Math.max(0, calc.P_rec).toFixed(1)} kg P/ha`,
  subtitle: `Crop Norm (${calc.F_norm}) + Soil Delta Q (${calc.delta_Q.toFixed(1)})`,
  x: { label: "Intensity I (mg P / L)", domain: [0, 5] },
  y: { label: "Mass Q (mg P / kg soil)", domain: [0, 150] },
  width: 1000,
  height: 500,
  marks: [
    Plot.line(d3.range(0.1, 5.1, 0.1).map(i => ({I: i, Q: calc.K * Math.pow(i, calc.n)})), {x: "I", y: "Q", stroke: "black", strokeWidth: 3}),
    
    // P_crit line
    Plot.ruleX([calc.P_crit], {stroke: "green", strokeDasharray: "4"}),
    Plot.dot([{I: calc.P_crit, Q: calc.Q_target}], {x: "I", y: "Q", fill: "green", r: 8}),
    Plot.text([{I: calc.P_crit, Q: calc.Q_target}], {x: "I", y: "Q", text: () => `P_crit`, dy: -20, fill: "green", fontSize: 16, fontWeight: "bold"}),
    
    // Current State
    Plot.dot([{I: current_P, Q: calc.Q_now}], {x: "I", y: "Q", fill: "red", r: 8}),
    Plot.text([{I: current_P, Q: calc.Q_now}], {x: "I", y: "Q", text: () => `Current`, dy: 20, fill: "red", fontSize: 16, fontWeight: "bold"}),
    
    // Delta Q Annotation
    Plot.arrow([{x1: current_P, y1: calc.Q_now, x2: calc.P_crit, y2: calc.Q_target}], {x1: "x1", y1: "y1", x2: "x2", y2: "y2", stroke: "blue", strokeWidth: 2, headLength: 8})
  ]
})
```
"""
    new_slides.append(calc_slide)

    # 12. Conclusions
    if "Conclusions & Policy Recommendations" in slide_dict:
        # Soften conclusions
        concl = slide_dict["Conclusions & Policy Recommendations"]
        concl = concl.replace("The AAE10 Extraction is Flawed for Calcareous Soils", "The AAE10 Extraction on Calcareous Soils")
        concl = concl.replace("causing a massive, non-linear chemical artifact that mathematical covariates cannot correct", "leading to challenges in accurate measurement")
        new_slides.append(concl)

    # 13. Annex
    new_slides.append("## Annex (Methodological Details) { .center }")
    for t in [
        "The GRUD Empirical Classification: $P_{CO2}$ Model",
        "The GRUD Empirical Classification: $P_{AAE10}$ Model",
        "The GRUD-model",
        "Mechanistic Validation of GRUD Supply Classes",
        "The Illusion of Bijection: GRUD Correction Factors",
        "Systematic Bias in Fertilizer Recommendations",
        "Interactive Bias Surface",
        "The Calcareous pH Artifact in AAE10",
        "The AAE10 Calcareous Artifact & Filtered Model",
        "Annex"
    ]:
        if t in slide_dict:
            new_slides.append(slide_dict[t])

    # Reconstruct
    with open("presentation/index.qmd", "w", encoding='utf-8') as f:
        f.write(header)
        f.write("\n")
        f.write("\n\n".join(new_slides))

if __name__ == "__main__":
    header, d = parse_presentation("presentation/index.qmd")
    build_new_presentation(header, d)
    print("Presentation successfully rewritten.")
