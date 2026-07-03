import re

def fix_mutate_syntax(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Find the broken chunk:
    # c_base_mean <- mean(cf[grep("c_base", names(cf))])
    #             Predicted = 1 - exp(-c_base_mean * exp(cf[[beta_name]] * val) * P)
    #
    # Change to:
    # c_base_mean = mean(cf[grep("c_base", names(cf))]),
    #             Predicted = 1 - exp(-c_base_mean * exp(cf[[beta_name]] * val) * (P + cf['E_base']))
    # Note: we need to add `(P + cf['E_base'])` to match the new model equation for the partial effects plot!
    
    bad_str = 'c_base_mean <- mean(cf[grep("c_base", names(cf))])\n            Predicted = 1 - exp(-c_base_mean * exp(cf[[beta_name]] * val) * P)'
    good_str = 'c_base_mean = mean(cf[grep("c_base", names(cf))]),\n            Predicted = 1 - exp(-c_base_mean * exp(cf[[beta_name]] * val) * (P + cf["E_base"]))'
    
    content = content.replace(bad_str, good_str)

    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Fixed mutate syntax in {filepath}")

fix_mutate_syntax('notebooks/qi_modelling1.qmd')
fix_mutate_syntax('notebooks/qi_modelling1.R')
fix_mutate_syntax('notebooks/temp_qi.R')
