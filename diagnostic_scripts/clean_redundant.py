import re

with open('notebooks/qi_modelling1.qmd', 'r') as f:
    content = f.read()

# Remove the inline paragraph referencing m_yield_nlme
inline_pattern = re.compile(r'The final non-linear mixed-effects formulation achieves a marginal pseudo-\$R\^2\$.*?across nested sites and plots\)\.', re.DOTALL)
content = inline_pattern.sub('', content)

# Remove the forest-plot-c-drivers chunk and its preceding text
forest_pattern = re.compile(r'### Drivers of the Rate Constant \(c\).*?```\{r forest-plot-c-drivers.*?\n```', re.DOTALL)
content = forest_pattern.sub('', content)

with open('notebooks/qi_modelling1.qmd', 'w') as f:
    f.write(content)

print("Cleaned redundant chunks!")
