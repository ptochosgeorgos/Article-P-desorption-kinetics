import os
import re
import glob

writing_dir = "/home/marc/Documents/Agroscope/Article-P-desorption-kinetics/writing"
qmd_files = glob.glob(os.path.join(writing_dir, "*.qmd"))

replacements = [
    # Y_rel
    (r'\$Y_\{rel\}\$', r'$Y_{\text{rel}}$'),
    # P_uptake / P_up
    (r'\$P_\{uptake\}\$', r'$P_{\text{up}}$'),
    (r'\$P_\{up\}\$', r'$P_{\text{up}}$'),
    # P_up, rel
    (r'\$Uptake_\{rel\}\$', r'$P_{\text{up, rel}}$'),
    (r'\$\\mathit\{Uptake\}_\{rel\}\$', r'$P_{\text{up, rel}}$'),
    (r'\\mathit\{Uptake\}_\{rel\}', r'P_{\text{up, rel}}'),
    (r'\$\\mathit\{Uptake\}_\{\\text\{rel\}\}\$', r'$P_{\text{up, rel}}$'),
    # P_bal
    (r'\$P_\{bal\}\$', r'$P_{\text{bal}}$'),
    # P_desorb
    (r'\$P_\{desorb\}\$', r'$P_{\text{desorb}}$'),
    # T_anom
    (r'\$Temp_\{Anom\}\$', r'$T_{\text{anom}}$'),
    (r'\$Temp_\{anom\}\$', r'$T_{\text{anom}}$'),
    # Pr_anom
    (r'\$Prec_\{Anom\}\$', r'$Pr_{\text{anom}}$'),
    (r'\$Prec_\{anom\}\$', r'$Pr_{\text{anom}}$'),
    # P_CO2
    (r'\$P_\{CO_2\}\$', r'$P_{\text{CO}_2}$'),
    # Al_ox / Fe_ox
    (r'\$Al_\{ox\}\$', r'$Al_{\text{ox}}$'),
    (r'\$Fe_\{ox\}\$', r'$Fe_{\text{ox}}$'),
]

for filepath in qmd_files:
    with open(filepath, 'r') as f:
        content = f.read()
    
    new_content = content
    for old, new in replacements:
        # Be careful not to replace things that are already correctly formatted
        new_content = re.sub(old, new, new_content)
    
    if new_content != content:
        with open(filepath, 'w') as f:
            f.write(new_content)
        print(f"Updated {os.path.basename(filepath)}")

print("Harmonization complete.")
