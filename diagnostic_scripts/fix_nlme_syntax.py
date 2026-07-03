import re
import sys

def fix_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # The broken syntax is: fixed = list(c_base ~ crop - 1, beta_invb + beta_pH + ... + E_base ~ 1),
    # We want to find `fixed = list(c_base ~ crop - 1, (.*?) ~ 1),`
    # and split the `(.*?)` by ` + ` and map to `x ~ 1`
    
    def repl(match):
        betas_str = match.group(1)
        betas = [b.strip() for b in betas_str.split('+')]
        fixed_list = ["c_base ~ crop - 1"] + [f"{b} ~ 1" for b in betas]
        return "fixed = list(" + ", ".join(fixed_list) + "),"
        
    content = re.sub(r'fixed\s*=\s*list\(c_base\s*~\s*crop\s*-\s*1,\s*(.*?)\s*~\s*1\),', repl, content)

    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Fixed {filepath}")

fix_file('notebooks/qi_modelling1.qmd')
fix_file('notebooks/qi_modelling1.R')
fix_file('notebooks/temp_qi.R')
