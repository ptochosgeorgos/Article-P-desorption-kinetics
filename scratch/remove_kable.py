import re
with open('notebooks/qi_modelling_parallel.qmd', 'r') as f:
    lines = f.readlines()

new_lines = []
i = 0
while i < len(lines):
    if 'kable_styling' in lines[i]:
        # If the previous line ends with ` |>`, remove it
        if i > 0 and ' |>' in new_lines[-1]:
            new_lines[-1] = new_lines[-1].replace(' |>\n', '\n').replace(' |>\r\n', '\n')
        i += 1
        continue
    new_lines.append(lines[i])
    i += 1

with open('notebooks/qi_modelling_parallel.qmd', 'w') as f:
    f.writelines(new_lines)

