import re

with open('notebooks/qi_modelling1.qmd', 'r') as f:
    content = f.read()

pattern = r'start = local\(\{.*?\}\),'
replacement = 'start = c(0.04, rep(0, 15), 0),'

content = re.sub(pattern, replacement, content, flags=re.DOTALL)

with open('notebooks/qi_modelling1.qmd', 'w') as f:
    f.write(content)

print("Applied clean starts to P_AAE10")
