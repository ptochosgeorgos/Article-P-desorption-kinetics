import re

def patch_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # 1. Modify the equation: `)) * P_TERM),` -> `)) * (P_TERM + E_base))),`
    # The P terms are soil_0_20_P_CO2, a_CO2_total_mg_L, soil_0_20_P_AAE10
    
    # We can match `)) * (soil_0_20_P_CO2|a_CO2_total_mg_L|soil_0_20_P_AAE10)),`
    # and replace with `)) * (\1 + E_base))),`
    content = re.sub(
        r'\)\) \* (soil_0_20_P_CO2|a_CO2_total_mg_L|soil_0_20_P_AAE10)\),',
        r')) * (\1 + E_base)),',
        content
    )

    # 2. Modify `fixed = list(c_base ~ crop - 1, ... ~ 1),`
    # by adding ` + E_base` before ` ~ 1)`
    content = re.sub(
        r'(fixed\s*=\s*list\(c_base\s*~\s*crop\s*-\s*1,\s*[^~]+)(~\s*1\),)',
        r'\1+ E_base \2',
        content
    )

    # 3. Add an extra 0 to `start = c(...)`
    # Because there's a variable number of 0s, we can just append `, 0` before the closing parenthesis of the `start = c(...)` line.
    content = re.sub(
        r'(start\s*=\s*c\(rep\(1\.2,\s*length\(unique\(D_Yield\$crop\)\)\)(?:,\s*0)*)(?=\),)',
        r'\1, 0',
        content
    )

    # Also need to add E_base to the P_crit calculation:
    # P_crit = (log(20) / c_eff) - E_base
    # Wait, the P_crit derivation is:
    # 0.95 = 1 - exp(-c_eff * (P_crit + E_base))
    # exp(-c_eff * (P_crit + E_base)) = 0.05
    # -c_eff * (P_crit + E_base) = ln(0.05) = -ln(20)
    # c_eff * (P_crit + E_base) = ln(20)
    # P_crit + E_base = ln(20) / c_eff
    # P_crit = (ln(20) / c_eff) - E_base
    
    # Let's fix the P_crit calculation
    # "P_crit = log(20) / c_eff," -> "P_crit = (log(20) / c_eff) - cf['E_base'],"
    content = content.replace(
        "P_crit = log(20) / c_eff,",
        "P_crit = (log(20) / c_eff) - cf['E_base'],"
    )
    
    # Fix the forest plot effect names filtering to also filter out E_base
    content = content.replace(
        'filter(!grepl("c_base", term))',
        'filter(!grepl("c_base", term), term != "E_base")'
    )

    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Patched {filepath}")

patch_file('notebooks/qi_modelling1.qmd')
patch_file('notebooks/qi_modelling1.R')
patch_file('notebooks/temp_qi.R')
