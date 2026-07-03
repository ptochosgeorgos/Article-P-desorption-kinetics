import re

def fix_r_script():
    with open('notebooks/qi_modelling1.R', 'r') as f:
        content = f.read()

    # Find the m_yield_nlme call
    # Replace it completely with a clean version
    pattern = re.compile(r'm_yield_nlme <- nlme\(.*?control = nlmeControl\(maxIter = 2000, returnObject = TRUE\)\s*\)', re.DOTALL)
    
    replacement = """m_yield_nlme <- nlme(
    Relative_Yield ~ 1 - exp(-(c_base * exp(
        beta_invb * z_inv_b + 
        beta_pH * z_pH + 
        beta_K * z_ln_K +
        beta_Mg * z_ln_Mg +
        beta_N * z_fert_N +
        beta_Temp * z_Temp_Mean + 
        beta_Prec * z_Prec_Anom
    )) * (soil_0_20_P_CO2 + E_base)),
    data = D_Yield,
    fixed = list(c_base ~ crop, beta_invb ~ 1, beta_pH ~ 1, beta_K ~ 1, beta_Mg ~ 1, beta_N ~ 1, beta_Temp ~ 1, beta_Prec ~ 1, E_base ~ 1),
    random = c_base ~ 1 | site/plot_nr,
    start = c(1.2, rep(0, length(unique(D_Yield$crop)) - 1), 0, 0, 0, 0, 0, 0, 0, 0),
    control = nlmeControl(maxIter = 2000, returnObject = TRUE)
)"""
    
    content = pattern.sub(replacement, content)
    
    # Let's also do the same for temp_qi.R to be safe
    with open('notebooks/qi_modelling1.R', 'w') as f:
        f.write(content)

fix_r_script()
