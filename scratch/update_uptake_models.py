import re

filepath = "notebooks/qi_modelling1.qmd"
with open(filepath, "r") as f:
    content = f.read()

# Make sure crop is a factor before the Uptake section
# Find line 534: group_by(site, crop, year) |>
content = re.sub(
    r'(group_by\(site, crop, year\) \|>\n\s*mutate\()',
    r'\1\n        crop = droplevels(as.factor(crop)),',
    content
)

# Insert n_crops calculation right after the filtering for Agro/Geo/Cons
content = re.sub(
    r'(D_Long_Agro <- D_Long \|> filter\(is\.finite\(z_inv_b_agro\)\))',
    r'\1\n    n_crops <- length(levels(D_Long_Agro$crop))',
    content
)
content = re.sub(
    r'(D_Long_Geo <- D_Long \|> filter\(is\.finite\(z_inv_b_geo\)\))',
    r'\1\n    n_crops <- length(levels(D_Long_Geo$crop))',
    content
)

# Now, we need to modify the `fixed = ` and `start = ` arguments of the 18 nlme models.
# The `fixed` formulas are in the form: fixed = Var1 + Var2 + ... + K_base + ... ~ 1
def replace_fixed(match):
    fixed_str = match.group(1)
    # Split by '+' and trim
    vars = [v.strip() for v in fixed_str.split('+') if v.strip() != '~ 1']
    # Rebuild as list(Var1 ~ 1, K_base ~ crop, ...)
    new_vars = []
    for v in vars:
        if 'K_base' in v:
            new_vars.append("K_base ~ crop")
        else:
            new_vars.append(f"{v} ~ 1")
    return "fixed = list(" + ", ".join(new_vars) + ")"

content = re.sub(r'fixed = (.*?~ 1)', replace_fixed, content)

# Modify the start vectors.
# They look like: start = c(U_base = 0.68, ..., K_base = median(D_Long_Geo$soil_0_20_P_CO2), beta_invb = 0)
def replace_start(match):
    before = match.group(1)
    median_call = match.group(2)
    after = match.group(3)
    return f"{before}K_base = c({median_call}, rep(0, n_crops - 1)){after}"

content = re.sub(r'(start = c\(.*?)K_base = (median\([^)]+\))(.*?\))', replace_start, content)

with open(filepath, "w") as f:
    f.write(content)

print("Successfully updated qi_modelling1.qmd")
