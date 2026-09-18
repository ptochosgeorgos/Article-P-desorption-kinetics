with open("presentation_grud/index.qmd", "a") as f:
    f.write("""

---

## The Affine Morphism vs. Chemical Reality { .smaller }

**The Mathematical Implication of the GRUD Matrices**
Since we proved that both lookup tables are fundamentally affine maps targeting the identical output space ($v \\in \\mathbb{Q}$), equating them ($v_{CO2} = v_{AAE10}$) mathematically forces an affine morphism between the two extractions:

$$ \\alpha_1 P_{CO2} + \\beta_1 \\text{clay} + \\gamma_1 = \\alpha_2 P_{AAE10} + \\beta_2 \\text{clay} + \\gamma_2 $$

Solving for $P_{AAE10}$ yields a strict linear mapping:
$$ P_{AAE10} = c_1 \\cdot P_{CO2} + c_2 \\cdot \\text{clay} + c_3 $$

**The Phenomenological Flaw**
This affine morphism mathematically dictates that the two extractions are linearly interchangeable. But chemically, they measure entirely different phenomena:
- **$P_{CO2}$** measures the immediately available concentration (**Intensity, $I$**).
- **$P_{AAE10}$** measures a much larger, strongly bound pool (**Quantity, $Q$**).

The true thermodynamic relationship between Quantity and Intensity is inherently non-linear (the Freundlich isotherm: $Q = K \\cdot I^n$) and is heavily governed by **pH**. By forcing an affine bijection, the GRUD model entirely ignores the fact that at $pH > 7.3$, the EDTA chelator in AAE10 physically saturates from excess Calcium, causing the mathematical bijection to completely collapse.
""")
