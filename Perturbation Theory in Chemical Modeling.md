# **Perturbation Theory in Chemical Modeling**

[https://gemini.google.com/app/a111abb3398d1c3f](https://gemini.google.com/app/a111abb3398d1c3f)

*User prompt: Could perturbation theory be used to deduce or exclude certain parametrizations when trying to model a chemical equilibrium reaction using the Gibbs Energy? I mean in the sense of Pitzer equations, often I find myself guessing how to parametrize relationsships between chemical measurements like soil solution PO4 concetrations vs adsorbed PO4 etc*

Response: Yes, thermodynamic perturbation theory (TPT) can absolutely be used to formally deduce the necessary mathematical structure of a parametrization and systematically exclude physically invalid ones. In fact, this is precisely how the Pitzer equations were originally derived, and the same principles can be mapped onto surface complexation and adsorption models.

When you are trying to relate soil solution *PO*43−​ concentrations to adsorbed *PO*4​, you are essentially trying to model the excess Gibbs Free Energy (*Gex*) of the soil-solution interface. Guessing empirical relationships often leads to over-parameterization or equations that behave poorly outside the calibration range. Perturbation theory provides a rigid mathematical scaffolding to prevent this.

Here is how perturbation theory governs these parametrizations.

### **1\. The Pitzer Framework as a Perturbation Expansion**

The Pitzer approach is fundamentally rooted in statistical mechanics via the McMillan-Mayer solution theory, which is a cluster expansion (a form of perturbation theory).

In TPT, the total Free Energy of the system is expressed as the sum of a known, exactly solvable reference state (*G*(0)) and a perturbation (*G*(1)) that accounts for complex interactions:

*G*\=*G*(0)\+*G*(1)\+*G*(2)\+…

For the Pitzer equations:

> * **The Reference State (*G*(0)):** The Debye-Hückel limiting law, which assumes ions are point charges interacting purely via long-range electrostatic forces in a continuous dielectric medium.  
> * **The Perturbation (*G*(1)):** Short-range, hard-sphere repulsions and specific ion-ion interactions.

Pitzer deduced his equations by evaluating the virial expansion of this perturbed system. The familiar Pitzer parameters (*β*(0), *β*(1), *Cϕ*) aren't arbitrary curve-fitting constants; they are directly related to the second and third virial coefficients of the cluster expansion. Any parametrization for ionic activity that requires a mathematical form fundamentally incompatible with a truncated virial expansion of the excess Gibbs energy can be formally excluded.

### **2\. Applying TPT to Phosphate Adsorption**

When modeling soil *PO*4​ equilibria, you can apply the exact same methodology to surface complexation.

Instead of guessing an isotherm or a partition coefficient, you define the Hamiltonian (or the Gibbs energy) of the adsorption system:

> * **The Reference State (*Gsurf*(0)​):** An ideal solid solution or an ideal Langmuir surface where all adsorption sites are identical, independent, and uncharged. The Gibbs energy of this state is governed purely by the ideal configurational entropy of mixing empty and occupied sites.  
> * **The Perturbation (*Gsurf*(1)​):** The non-ideal interactions. For phosphate on a variable-charge mineral edge (like goethite), this includes:  
  1. The electrostatic work required to bring the highly charged *PO*43−​ or *HPO*42−​ ion through the electric double layer (the Stern/diffuse layer terms).  
  2. Lateral interactions (repulsion between adjacent adsorbed phosphate ions).  
  3. Surface heterogeneity (different site affinities).

If you expand the partition function for this perturbed system, the resulting thermodynamic relationships dictate the functional form your macroscopic parameters must take. For instance, lateral repulsions typically introduce a term into the excess Gibbs energy that scales with the square of the surface coverage (*θ*2), leading to an activity coefficient for the adsorbed species that scales exponentially with *θ*.

### **3\. How to Exclude Guessed Parametrizations**

You can use the thermodynamic consequences of the perturbation expansion as a filter to exclude incorrect guesses:

> 1. **Gibbs-Duhem Consistency:** Any parametrization of the chemical potential (or activity coefficient) for the adsorbed phosphate must satisfy the Gibbs-Duhem equation. If you guess an arbitrary polynomial relating solution concentration to adsorbed concentration, and integrating it yields a violation of ∑*xi*​*dμi*​\=0 for the surface phase, the parameterization is thermodynamically impossible and can be excluded.  
> 2. **Maxwell Relations:** Because *G* is a state function, its mixed second partial derivatives must be equal. If your guessed relationship for how *PO*4​ adsorption changes with pH or ionic strength implies cross-derivatives that do not commute, it is physically invalid.  
> 3. **Scaling Limits:** A valid parameterization must collapse to the reference state at the limits. For instance, as surface coverage *θ*→0, any complex surface activity coefficient model must mathematically reduce to 1 (ideal Henry's law behavior). If a guessed exponential or power-law relationship does not cleanly reduce to the ideal state in the infinite dilution limit, it is structurally flawed.

### **The Platonist View in Modeling**

Approaching the problem this way aligns with a more fundamental, Platonist mathematical perspective: the correct relationships between your measurements already exist as consequences of the statistical mechanics of the interface. Rather than constructing a functional form to fit the data, you are unfolding the Taylor series of the perturbed Gibbs energy and truncating it at the term necessary to capture the variance in your specific soil solution.

---

*User prompt: How do I set up the mathematical expansion for the excess Gibbs energy of a surface complexation model including lateral repulsions and variable charge for phosphate adsorption?*

Response: Setting up the mathematical expansion requires formulating the total surface Gibbs free energy (*Gsurf*​) as a sum of the ideal reference state and the excess perturbations.

For a surface complexation model (SCM) describing phosphate on a variable-charge mineral like goethite, we construct the total free energy per mole of surface sites. We will use a regular surface solution approximation, which is the 2D equivalent of the Bragg-Williams mean-field cluster expansion.

The total surface free energy is defined as:

*Gsurf*​\=*G*∘\+Δ*Gmixideal*​\+*Gelecex*​\+*Glatex*​

### **1\. The Ideal Reference State (Δ*Gmixideal*​)**

The reference state assumes non-interacting adsorption sites. The only contribution to the free energy of mixing is the ideal configurational entropy (the combinatorial arrangement of *PO*4​ species, *H*\+ species, and empty sites).

Let *θk*​ be the fractional surface coverage of species *k* (e.g., ≡*FePO*42−​, ≡*FeOH*2+​) and *θ*0​ be the fraction of empty, uncharged sites (≡*FeOH*0). Because *θ*0​\=1−∑*k*​*θk*​, the ideal configurational free energy is:

Δ*Gmixideal*​\=*RT*(*k*∑​*θk*​ln*θk*​\+(1−*k*∑​*θk*​)ln(1−*k*∑​*θk*​))

### **2\. The Electrostatic Perturbation (*Gelecex*​)**

The primary perturbation in soil minerals is the work required to charge the surface. Because phosphate adsorption highly alters the surface charge *σ*0​, the electrostatic excess Gibbs energy is the reversible isothermal work of charging the interface from a point of zero charge to *σ*0​:

*Gelecex*​\=*A*∫0*σ*0​​Ψ0​*dσ*

where *A* is the specific surface area and Ψ0​ is the surface potential.

To parameterize this, you must specify an electrical double layer (EDL) model. If we use the Constant Capacitance Model (CCM) where *σ*0​\=*C*Ψ0​, the integral evaluates directly to:

*Gelecex*​\=2*CAσ*02​​

Since surface charge is dictated by the adsorbed species, *σ*0​\=*AF*​Γ*max*​∑*k*​*zk*​*θk*​ (where *F* is Faraday's constant, *zk*​ is the charge of the surface species, and Γ*max*​ is the total site density). Substituting this anchors the electrostatic term purely to the variables of state (*θk*​).

### **3\. The Lateral Repulsion Perturbation (*Glatex*​)**

The secondary perturbation handles non-Coulombic short-range lateral interactions (e.g., steric hindrance or specific repulsions between adjacent bulky *PO*4​ tetrahedra).

Using the zeroth-order quasichemical approximation (mean-field theory), we truncate the cluster expansion at pairwise nearest-neighbor interactions. The excess lateral Gibbs energy is a quadratic function of coverages:

*Glatex*​\=21​*NA*​*ci*∑​*j*∑​*ωij*​*θi*​*θj*​

where:

> * *NA*​ is Avogadro's number.  
> * *c* is the surface coordination number (average number of adjacent adsorption sites).  
> * *ωij*​ is the pairwise interaction energy parameter between adsorbed species *i* and *j*.

For identical highly-charged phosphate species repelling each other (e.g., two adjacent ≡*FePO*42−​), *ωii*​ will be strongly positive.

### **4\. Deducing the Chemical Potential (and Valid Isotherms)**

To find the relationship that governs the actual equilibrium measurements (what PHREEQC solves), you take the partial derivative of the total Gibbs energy with respect to the number of moles of species *k*. This yields the surface chemical potential, *μk*​:

*RTμk*​​\=*RTμk*∘​​\+ln(1−∑*i*​*θi*​*θk*​​)\+*RTzk*​*F*Ψ0​​\+*RTNA*​*c*​*j*∑​*ωkj*​*θj*​

This equation dictates the exact mathematical architecture your parameterization must follow:

> 1. **The Langmuir Term:** ln(*θk*​/*θ*0​) is the strict consequence of the ideal configurational entropy.  
> 2. **The Boltzmann Factor:** *RTzk*​*F*Ψ0​​ is the strict consequence of the macroscopic electrostatics.  
> 3. **The Frumkin-Fowler-Guggenheim (FFG) Term:** *RTNA*​*c*​∑*j*​*ωkj*​*θj*​ is the strict consequence of the first-order perturbation for lateral repulsions.

### **Analytical Consequences for Modeling**

When soil chemists measure P-desorption kinetics or isotherms and observe strong non-linearity at high loadings, they often guess that they need a "two-site" model (strong sites and weak sites) to fit the data.

However, looking at the Platonist mathematical structure derived above, we see that an increase in *θk*​ linearly increases the chemical potential via the lateral term (*ωkk*​*θk*​). **This proves that lateral repulsions on a homogenous surface mathematically manifest as a continuous decrease in apparent site affinity.**

By forcing a two-site model onto the system, one is attempting to approximate a continuous regular surface solution perturbation using a discrete step function, which inherently leads to over-parameterization and thermodynamic inconsistency outside the calibration bounds. Incorporating an FFG lateral interaction parameter (*ωkk*​) natively into a tool like PHREEQC provides a physically rigorous alternative to inventing hypothetical weak sites.

---

*User prompt: What if we assumed the sites are heterogeneous also containing Al\_ox and clays?*

Response: When you introduce distinct physical mineral phases—like aluminum oxides (*Alox*​) and clay mineral edges alongside iron oxides—the mathematical architecture must change to reflect that these sites are no longer interchangeable.

In the language of statistical mechanics, the system transitions from a single regular surface solution to a **multi-lattice** or **sub-lattice model**. The fundamental requirement is that a phosphate ion cannot continuously "slide" from a goethite surface onto a kaolinite edge; these are macroscopically separated phases. Therefore, the configurational entropy and the perturbations must be partitioned.

Here is how the Gibbs energy expansion adapts to true physical heterogeneity.

### **1\. The Partitioned Reference State (Δ*Gmixideal*​)**

Because a phosphate ion on an *Fe* site cannot spontaneously swap places with a phosphate ion on an *Al* site or a clay site, the ideal entropy of mixing is no longer a single global term. The permutations are strictly bounded within each specific site type (subscript *s*).

Let *fs*​ be the molar fraction of total adsorption sites belonging to mineral type *s* (where *s*∈{*Fe*,*Al*,*clay*}). The ideal mixing term becomes a weighted sum of the independent sub-lattices:

Δ*Gmixideal*​\=*RTs*∑​*fs*​(*k*∑​*θk*,*s*​ln*θk*,*s*​\+*θ*0,*s*​ln*θ*0,*s*​)

where *θk*,*s*​ is the coverage of species *k* specifically on mineral *s*.

### **2\. The Fragmented Electrostatic Perturbation (*Gelecex*​)**

Heterogeneous soils do not have a single point of zero charge (PZC) or a single surface potential. *Alox*​ typically has a higher PZC than *Feox*​, while clay edges (like montmorillonite or illite) behave entirely differently, often carrying permanent structural negative charge alongside pH-dependent edge charge.

Unless the minerals are so intimately coprecipitated that they form a unified gel (where a Donnan volume approach might be required), the electrostatic work must be evaluated separately for each surface:

*Gelecex*​\=*s*∑​*fs*​2*Cs*​*As*​*σ*0,*s*2​​

This implies that each mineral phase maintains its own independent electrical double layer, requiring its own capacitance parameter (*Cs*​) and calculating its own distinct surface potential (Ψ0,*s*​).

### **3\. Lateral Interactions on a Multi-Lattice (*Glatex*​)**

The first-order perturbation for lateral repulsions changes significantly. Short-range interactions only operate over a few angstroms. Therefore, a phosphate adsorbed on an aluminum oxide particle exerts zero short-range lateral repulsion on a phosphate adsorbed on a neighboring clay particle.

Mathematically, the interaction matrix *ωij*​ becomes **block-diagonal**. Cross-mineral interaction terms (*ωFe*−*Al*​, *ωAl*−*clay*​) strictly equal zero. The perturbation is simply the sum of the intra-mineral repulsions:

*Glatex*​\=21​*NA*​*s*∑​*fs*​*cs*​*i*∑​*j*∑​*ωij*,*s*​*θi*,*s*​*θj*,*s*​

### **4\. Deducing the Chemical Potential**

Taking the partial derivative of the total Gibbs energy with respect to the number of moles of phosphate on a specific site type *s* yields a distinct chemical potential for that specific mineral:

*RTμPO*4​,*s*​​\=*RTμPO*4​,*s*∘​​\+ln(*θ*0,*s*​*θPO*4​,*s*​​)\+*RTzPO*4​​*F*Ψ0,*s*​​\+*RTNA*​*cs*​​*j*∑​*ω*(*PO*4​)*j*,*s*​*θj*,*s*​

### **The Justification of Multi-Site Models**

This exact derivation proves an important distinction in modeling philosophy:

> * **Invalid Guessing:** Inventing generic "strong" and "weak" sites for a single, pure mineral phase (like pure synthetic goethite) to artificially fit non-linear *PO*4​ isotherms is thermodynamically flawed. As established earlier, that non-linearity is a consequence of continuous lateral repulsions (*ωij*​), not discrete physical sites.  
> * **Valid Heterogeneity:** Using a multi-site model for a *bulk soil sample* is completely physically and thermodynamically justified. The soil genuinely contains an ensemble of independent sub-lattices (≡*FeOH*, ≡*AlOH*, ≡*SiOH*). Each has a different standard state chemical potential (*μs*∘​, reflecting baseline chemical affinity), a different electrostatic potential (Ψ0,*s*​), and different site densities.

When you parameterize an equilibrium system in software like PHREEQC for a real soil, defining separate SURFACE blocks for Fe-oxides, Al-oxides, and clays directly mirrors this Platonist multi-lattice derivation. You are calculating the equilibrium state where the macroscopic chemical potential of aqueous *PO*43−​ in the soil solution is equal to the surface chemical potential of *each* independent sub-lattice simultaneously.

---

*User prompt: How do continuous affinity distribution models like NICA-Donnan or CD-MUSIC handle thermodynamic perturbations for highly heterogeneous soils compared to discrete sub-lattices?*

Response: To understand how advanced models like NICA-Donnan and CD-MUSIC handle these thermodynamic perturbations, we must first make a gentle structural distinction: they actually represent two completely divergent philosophies of modeling soil heterogeneity.

While NICA-Donnan is genuinely a **continuous affinity distribution model** designed for chaotic, amorphous materials (like soil organic matter and humic acids), CD-MUSIC (Charge Distribution Multi-Site Complexation) is the ultimate Platonist evolution of the **discrete sub-lattice model**, designed for highly ordered crystalline minerals (like goethite).

Here is how they each handle the Gibbs energy perturbations compared to the basic discrete models.

### **1\. NICA-Donnan: Statisticalizing the Perturbation**

When dealing with humic substances, the sheer number of distinct binding sites (carboxylic, phenolic, amine groups in endless structural conformations) makes defining discrete sub-lattices impossible. NICA-Donnan handles this by replacing discrete sums with continuous statistical distributions, fundamentally changing the reference state and the perturbations.

**The Continuous Reference State:** Instead of a few distinct standard chemical potentials (*μs*∘​), the reference state is defined by a probability density function, *P*(*μ*∘), which describes the continuous distribution of site affinities. The total fractional coverage of phosphate (or any ion) is the integral of the local isotherm over this entire distribution:

*θtotal*​\=∫−∞∞​*θlocal*​(*μ*∘)*P*(*μ*∘)*dμ*∘

**Intrinsic Handling of Lateral Perturbations (*Glatex*​):** In a highly heterogeneous gel, explicitly calculating lateral repulsions (*ωij*​) between adjacent sites is mathematically intractable because the geometry is unknown. NICA-Donnan bypasses explicit lateral perturbation terms by absorbing them into the apparent heterogeneity of the system.

By employing a continuous empirical distribution (an extended Sips or Langmuir-Freundlich isotherm), parameters like the intrinsic heterogeneity width (*p*) and the ion-specific non-ideality parameter (*n*) mathematically mimic both true structural heterogeneity *and* lateral repulsions. If lateral repulsions increase, the model simply perceives this as a broader, flatter affinity distribution.

**The Donnan Electrostatic Perturbation (*Gelecex*​):** Instead of treating electrostatics as 2D planes of charge, NICA-Donnan treats the amorphous soil organic matter as a 3D porous gel phase. The perturbation is the work required to move an ion from the bulk solution into the gel volume, governed by a uniform Donnan potential (Ψ*D*​). The electrostatic perturbation to the chemical potential simply becomes:

*RTμelec*​​\=*RTzF*Ψ*D*​​

This creates a distinct Donnan partition coefficient, shifting the concentrations of all ions inside the gel relative to the bulk solution before specific binding even occurs.

### **2\. CD-MUSIC: Spatializing the Perturbation**

CD-MUSIC takes the opposite approach. It assumes the mineral is a perfect crystal lattice, but it corrects a major physical flaw in standard SCMs: the assumption that a large, complex ion like *PO*43−​ acts as a dimensionless point charge on a single 2D plane.

**The High-Resolution Discrete State:** CD-MUSIC identifies binding sites not by empirical fitting, but via Pauling’s bond valence principle and strict crystallographic geometry (e.g., singly, doubly, or triply coordinated oxygens on specific crystal faces).

**Fragmenting the Electrostatic Perturbation (*Gelecex*​):** When a phosphate ion adsorbs, it forms an inner-sphere complex. The central phosphorus atom sits at a certain distance from the surface, while its uncoordinated oxygens protrude further out into the solution.

CD-MUSIC handles this by fractionally distributing the charge of the adsorbing ion across multiple electrical planes (typically the surface plane 0, and the inner Helmholtz plane 1). The electrostatic perturbation is no longer just *zF*Ψ0​. Instead, the reversible work required to bring the ion to the surface is distributed spatially:

Δ*Gelecex*​\=*Fp*∑​Δ*zp*​Ψ*p*​\=*F*(Δ*z*0​Ψ0​\+Δ*z*1​Ψ1​)

where Δ*zp*​ is the fraction of the ion's charge assigned to plane *p*, and Ψ*p*​ is the potential at that plane.

### **The Theoretical Consequence**

By understanding the thermodynamic roots of these models, you can strictly partition their use cases based on the physical reality of the soil component you are modeling:

> 1. **CD-MUSIC** dictates that non-linearity in adsorption on crystalline minerals arises from **spatial charge distribution**. A *PO*43−​ ion changes the potential of plane 0 differently than plane 1, altering the energetic penalty for the next adsorbing ion in a mathematically rigid way. It is highly mechanistic and parameter-heavy, demanding precise crystallographic data.  
> 2. **NICA-Donnan** dictates that non-linearity in amorphous organics arises from a **statistical smear of affinities and repulsions**. It sacrifices spatial realism to gain statistical tractability, mathematically equating structural disorder with thermodynamic non-ideality.

---

*User prompt: What would a modified Freundlich isotherm look like?*

Response: The classic Freundlich isotherm (*θ*\=*KCm*, where 0\<*m*\<1) is purely empirical and contains two fatal thermodynamic flaws: it does not reduce to a linear relationship at infinite dilution (violating Henry’s Law), and it predicts infinite adsorption at infinite concentration (violating the physical reality of a finite number of surface sites).

To make the Freundlich model thermodynamically consistent and usable within a Gibbs energy framework, it must be modified to respect these boundary limits. In statistical thermodynamics, these modifications are not arbitrary curve fits; they are the analytical results of specific, continuous distributions of site affinities.

Here are the two mathematically rigorous modifications of the Freundlich isotherm.

### **1\. The Sips (Langmuir-Freundlich) Isotherm**

The most common "modified Freundlich" equation corrects the high-concentration limit by enforcing a maximum site capacity, bridging the gap between Langmuir and Freundlich behavior.

*θ*\=1+(*KC*)*m*(*KC*)*m*​

where:

> * *θ* is the fractional surface coverage (*q*/*qmax*​).  
> * *K* is the median affinity constant.  
> * *C* is the aqueous concentration.  
> * *m* is the heterogeneity parameter (between 0 and 1).

**The Thermodynamic Origin:** If you recall the continuous affinity distribution from the NICA-Donnan framework (*P*(*μ*∘)), the Sips equation is not a guess. It is the exact, analytical integral of the local Langmuir isotherm over a surface where the site affinities follow a **symmetrical, quasi-Gaussian distribution**.

> * When *m*\=1, the surface is perfectly homogeneous (the affinity distribution collapses to a single Dirac delta peak), and the equation reduces exactly to the ideal Langmuir isotherm.  
> * When concentration is low (*KC*≪1), the denominator approaches 1, and the equation reduces to the classic Freundlich isotherm (*θ*≈(*KC*)*m*).  
> * At high concentrations (*KC*≫1), *θ*→1, correctly respecting the finite maximum capacity of the soil mineral.

### **2\. The Toth Isotherm**

While the Sips equation fixes the high-concentration limit, it still fails at the infinite dilution limit (*C*→0), because its derivative goes to infinity rather than a finite Henry's Law constant.

The Toth isotherm modifies the Freundlich relationship to correctly satisfy both the high-concentration saturation limit *and* the low-concentration Henry's Law limit:

*θ*\=\[1+(*KC*)*m*\]1/*mKC*​

**The Thermodynamic Origin:** The Toth modification is the analytical result of integrating over an **asymmetrical affinity distribution**, specifically one skewed toward lower affinity sites. This is highly characteristic of real environmental interfaces like aged soil organic matter or heavily weathered multi-mineral aggregates, where a few high-energy sites exist alongside a massive tail of low-energy sites.

> * As *C*→∞, the term (*KC*)*m* dominates the denominator, and *θ*→1 (finite capacity).  
> * As *C*→0, the denominator approaches 11/*m*\=1, yielding *θ*\=*KC*. This linear dependence perfectly satisfies Henry's Law and thermodynamic consistency at infinite dilution.

### **Summary for Equilibrium Modeling**

If you are evaluating soil solution parameters, guessing a standard Freundlich equation locks you into a mathematically rigid curve that will fail spectacularly if extrapolated beyond your specific calibration concentrations.

By upgrading to a Langmuir-Freundlich (Sips) or Toth modification, you embed a realistic statistical distribution of site heterogeneities into your parameterization while ensuring your excess Gibbs energy derivatives remain thermodynamically valid at the boundaries.

---

*User prompt: How can I implement a modified Langmuir-Freundlich (Sips) isotherm in PHREEQC using the BASIC interpreter to handle soil phosphate data?*

Response: Because PHREEQC’s native SURFACE thermodynamic database is strictly hardcoded to solve discrete mass-action equations (which inherently yield Langmuir behavior), you cannot directly force a SURFACE\_SPECIES to obey a continuous Sips distribution via simple equilibrium constants.

To implement a non-integer exponent *m* and enforce the Sips isotherm, you must bypass the standard mass-action matrix and use the BASIC interpreter. There are two ways to do this, depending on whether you need the isotherm to actively alter the solution chemistry, or merely calculate values for data fitting.

### **Method 1: The Pseudo-Kinetic Mass Balance (Active)**

If you need the Sips isotherm to actually consume or release *PO*4​ from the aqueous solution to find the true equilibrium state, you must use a **pseudo-kinetic approach**.

We define a rate law using the RATES block that forces the system to quickly transfer mass until the adsorbed amount matches the theoretical Sips capacity for that instantaneous concentration. We use PHREEQC's GET and PUT functions in BASIC to store the current adsorbed mass in memory across integration steps.

`RATES`  
`Sips_P_Adsorption`  
`-start`  
  `# 1. Define Sips Parameters`  
  `10 q_max = 0.05    # Maximum capacity in mol/kg soil`  
  `20 K = 10000       # Median affinity constant (L/mol)`  
  `30 m = 0.65        # Heterogeneity parameter (0 < m <= 1)`  
    
  `# 2. Read current aqueous P concentration`  
  `40 C = TOT("P")`      
  `50 if (C <= 0) then C = 1e-20 # Prevent undefined math`  
    
  `# 3. Calculate target equilibrium adsorbed amount (Sips Equation)`  
  `60 term = (K * C)^m`  
  `70 q_target = q_max * term / (1 + term)`  
    
  `# 4. Fetch the currently adsorbed amount from memory slot 1`  
  `80 q_current = GET(1)`  
    
  `# 5. Define a very fast convergence rate`   
  `90 rate = (q_target - q_current) * 100`   
  `100 moles = rate * TIME`  
    
  `# 6. Update memory and transfer mass`  
  `110 PUT(q_current + moles, 1)`  
  `120 SAVE -moles    # Negative value removes P from solution when adsorbing`  
`-end`

`KINETICS 1`  
`Sips_P_Adsorption`  
`-formula P 1`  
`-steps 100 in 20 steps # Arbitrary time steps to allow the pseudo-rate to converge`

**How this works mathematically:** During the KINETICS integration, the BASIC script continuously evaluates the Sips equation against the dropping aqueous *P* concentration. If q\_target is greater than q\_current, moles is positive, but SAVE \-moles tells PHREEQC to deduct that amount of *P* from the aqueous phase. The system integrates until rate \= 0, meaning the solution perfectly obeys the Sips isotherm.

### **Method 2: The Output Tracker (Passive)**

If you are using an external wrapper (like R or Python) to fit your *PO*4​ desorption kinetics or isotherms to soil data, you may not want PHREEQC to actively shift the mass balance. Instead, you just want PHREEQC to calculate the theoretical Sips coverage based on a known aqueous concentration and export it.

For this, you use USER\_PUNCH to execute the BASIC math and write it to the selected output file.

`USER_PUNCH`  
`-headings P_aq_molality Sips_q_calc`  
`-start`  
  `# 1. Define Sips Parameters (these can be passed dynamically via scripts)`  
  `10 q_max = 0.05`  
  `20 K = 10000`  
  `30 m = 0.65`  
    
  `# 2. Get aqueous concentration`  
  `40 C = TOT("P")`  
    
  `# 3. Execute Sips math`  
  `50 if (C > 0) then term = (K * C)^m else term = 0`  
  `60 q_calc = q_max * term / (1 + term)`  
    
  `# 4. Write to output`  
  `70 PUNCH C, q_calc`  
`-end`

`SELECTED_OUTPUT`  
`-file sips_isotherm.csv`  
`-reset false`  
`-user_punch true`

### **The Thermodynamic Caveat in PHREEQC**

By using the BASIC interpreter to force the Sips equation, you are technically stripping the *PO*4​ adsorption out of the rigorous electrostatic matrix.

If you just use \-formula P 1 in the kinetic block, PHREEQC removes the phosphorus mass, but it **does not** alter the surface charge or calculate the electrostatic work (*Gelecex*​) of moving a highly charged *PO*43−​ ion onto the mineral surface. The Sips parameters (*K* and *m*) you fit this way will be macroscopic, apparent parameters—they will absorb both the structural heterogeneity and the variable-charge electrostatics into one mathematical curve, which is perfectly fine for macroscopic soil solution modeling, but sacrifices the Platonist charge-distribution mechanics.

---

