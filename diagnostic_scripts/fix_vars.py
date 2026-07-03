import re

filepath = 'notebooks/qi_modelling1.qmd'
with open(filepath, 'r') as f:
    content = f.read()

# Replace variables that were changed in the model refactor
content = content.replace('m_yield_raw_co2', 'm_yield_nlme')
content = content.replace('beta_fertK', 'beta_K')
content = content.replace('beta_fertMg', 'beta_Mg')
content = content.replace('z_fert_K', 'z_ln_K')
content = content.replace('z_fert_Mg', 'z_ln_Mg')
content = content.replace('Potassium Fertilizer', 'Soil Extractable K')
content = content.replace('Magnesium Fertilizer', 'Soil Extractable Mg')

with open(filepath, 'w') as f:
    f.write(content)
print("Fixed variable names in qmd")
