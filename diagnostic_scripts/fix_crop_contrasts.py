import re
import sys

def fix_nlme_crop(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Change `c_base ~ crop - 1` to `c_base ~ crop`
    content = content.replace("c_base ~ crop - 1", "c_base ~ crop")

    # Change `rep(1.2, length(unique(D_Yield$crop)))` to `1.2, rep(0, length(unique(D_Yield$crop)) - 1)`
    content = content.replace(
        "rep(1.2, length(unique(D_Yield$crop)))",
        "1.2, rep(0, length(unique(D_Yield$crop)) - 1)"
    )

    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Fixed crop contrast in {filepath}")

fix_nlme_crop('notebooks/qi_modelling1.qmd')
fix_nlme_crop('notebooks/qi_modelling1.R')
fix_nlme_crop('notebooks/temp_qi.R')
