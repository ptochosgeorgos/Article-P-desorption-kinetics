import re

with open('notebooks/qi_modelling1.qmd', 'r') as f:
    content = f.read()

# Pre-calculate n_crops and inject it
new_content = content.replace('m_yield_raw_co2 <- nlme(', 'n_crops <- length(levels(D_Yield$crop))\n\nm_yield_raw_co2 <- nlme(')
new_content = new_content.replace('length(unique(D_Yield$crop))', 'n_crops')

with open('notebooks/qi_modelling1.qmd', 'w') as f:
    f.write(new_content)

print("Fixed nlme start environment issue")
