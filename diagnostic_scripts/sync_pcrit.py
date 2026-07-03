import re

qmd_path = 'notebooks/qi_modelling1.qmd'
r_path = 'notebooks/qi_modelling1.R'

with open(qmd_path, 'r') as f:
    qmd_content = f.read()

with open(r_path, 'r') as f:
    r_content = f.read()

# Extract the chunk from R
r_chunk = r_content.split('## ----pcrit-analysis, fig.width=10, fig.height=5-------------------------------')[1].split('## ----loso-cv------------------------------------------------------------------')[0]
r_chunk = r_chunk.lstrip('\n').rstrip('\n')

# Find the chunk in qmd
qmd_pattern = re.compile(r'```{r pcrit-analysis, fig.width=10, fig.height=5}\n.*?\n```', re.DOTALL)

new_qmd_chunk = '```{r pcrit-analysis, fig.width=10, fig.height=5}\n' + r_chunk + '\n```'

new_qmd_content = qmd_pattern.sub(new_qmd_chunk, qmd_content)

with open(qmd_path, 'w') as f:
    f.write(new_qmd_content)

print("Synchronized pcrit-analysis chunk from .R to .qmd")
