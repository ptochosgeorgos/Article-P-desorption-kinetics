import sys

filepath = 'notebooks/qi_modelling1.qmd'
with open(filepath, 'r') as f:
    lines = f.readlines()

# The original block around line 427 was:
#         z_inv_b_geo = as.numeric(scale(inv_b_geo)),
#         z_inv_b_agro = as.numeric(scale(inv_b_agro)),
#         z_k = as.numeric(scale(k)),
#         z_v0 = as.numeric(scale(v0_kPS)),
#         z_fert_N = as.numeric(scale(fert_N_tot)),
#         site = as.factor(site),
#         year_f = as.factor(year)
#     )

# We will search for z_k and replace the following lines properly.

for i, line in enumerate(lines):
    if 'z_k = as.numeric(scale(k)),' in line:
        lines[i+1] = '        z_v0 = as.numeric(scale(v0_kPS)),\n'
        lines[i+2] = '        z_fert_N = as.numeric(scale(fert_N_tot)),\n'
        lines[i+3] = '        site = as.factor(site),\n'
        lines[i+4] = '        year_f = as.factor(year)\n'
        lines[i+5] = '    )\n'
        break

with open(filepath, 'w') as f:
    f.writelines(lines)
