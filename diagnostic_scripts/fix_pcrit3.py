import re

def fix_pcrit3(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # The broken syntax is:
    # re_total = ranef(m_yield_nlme)$site[as.character(site), "(Intercept)"] + ranef(m_yield_nlme)$plot_nr[plot_full_id, "(Intercept)"],

    content = content.replace('"(Intercept)"', '1')

    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Fixed Pcrit syntax (column index) in {filepath}")

fix_pcrit3('notebooks/qi_modelling1.qmd')
fix_pcrit3('notebooks/qi_modelling1.R')
fix_pcrit3('notebooks/temp_qi.R')
