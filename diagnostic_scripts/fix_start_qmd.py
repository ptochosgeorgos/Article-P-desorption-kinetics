import re

filepath = 'notebooks/qi_modelling1.qmd'
with open(filepath, 'r') as f:
    content = f.read()

# Replace all instances of beta_Prec = 0) with beta_Prec = 0, E_base = 0) in the start vectors
content = re.sub(r'beta_Prec\s*=\s*0\s*\)', 'beta_Prec = 0, E_base = 0)', content)

with open(filepath, 'w') as f:
    f.write(content)
print("Added E_base to start vectors in qmd")

