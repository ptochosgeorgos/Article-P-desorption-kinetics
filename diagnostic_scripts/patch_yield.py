import re
import sys

def process_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # 1. Replace yield sum
    content = content.replace(
        "total_yield = tidyr::replace_na(annual_yield_mp_DM, 0) + tidyr::replace_na(annual_yield_bp_DM, 0)",
        "total_yield = tidyr::replace_na(annual_yield_mp_DM, 0)"
    )

    # 2. Find all nlme calls related to Yield
    # They look like: fixed = c_base + beta_invb + beta_pH + ... ~ 1,
    # We want to change to: fixed = list(c_base ~ crop - 1, beta_invb + beta_pH + ... ~ 1),
    
    # Regex to match the fixed = ... line
    fixed_pattern = re.compile(r"fixed\s*=\s*c_base\s*\+\s*(.*?)\s*~\s*1\s*,")
    
    def repl_fixed(match):
        betas = match.group(1)
        return f"fixed = list(c_base ~ crop - 1, {betas} ~ 1),"

    content = fixed_pattern.sub(repl_fixed, content)

    # Regex to match the start = c(c_base = 1.2, ... line
    start_pattern = re.compile(r"start\s*=\s*c\(\s*c_base\s*=\s*1\.2\s*,")
    
    def repl_start(match):
        return "start = c(rep(1.2, length(unique(D_Yield$crop))), "

    content = start_pattern.sub(repl_start, content)

    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Patched {filepath}")

if __name__ == '__main__':
    process_file('notebooks/qi_modelling1.qmd')
    process_file('notebooks/qi_modelling1.R')
