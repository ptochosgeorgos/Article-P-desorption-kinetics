import re

def fix_pcrit(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # We need to rewrite the c_eff calculation.
    # The old code:
    #         year_re = re[as.character(year), "c_base"],
    #         c_eff  = (cf["c_base"] + year_re) * exp(
    # ...
    #
    # The new code should use the correct crop-specific c_base and nested random effects.
    # Actually, we can get the crop-specific intercept by looking it up in `cf`, but it's much easier
    # to just write a vectorized `case_when` or match. 
    # Let's just create a helper:
    # 
    # c_base_fixed = cf["c_base.(Intercept)"] + ifelse(crop == "ALT", 0, cf[paste0("c_base.crop", crop)]) # Assuming ALT is ref, but wait, `model.matrix` is safer.
    # Actually, since we only need to fix the script to not crash, let's look at what `re` is.
    
    new_code = """
        # Extract random effects for site and plot
        re_site <- ranef(m_yield_nlme)$site
        re_plot <- ranef(m_yield_nlme)$plot_nr
        
        # Calculate the crop-specific base (if crop is the reference, the contrast is NA so we replace with 0)
        c_base_crop = cf["c_base.(Intercept)"] + tidyr::replace_na(cf[paste0("c_base.crop", crop)], 0),
        
        # Total random effect
        plot_full_id = paste0(site, "/", plot_nr),
        re_total = re_site[as.character(site), "(Intercept)"] + re_plot[plot_full_id, "(Intercept)"],
        
        c_eff  = (c_base_crop + tidyr::replace_na(re_total, 0)) * exp(
"""
    
    content = re.sub(
        r'year_re\s*=\s*re\[as\.character\(year\),\s*"c_base"\],\s*c_eff\s*=\s*\(cf\["c_base"\]\s*\+\s*year_re\)\s*\*\s*exp\(',
        new_code.strip() + '\n',
        content
    )

    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Fixed Pcrit calculation in {filepath}")

fix_pcrit('notebooks/qi_modelling1.qmd')
fix_pcrit('notebooks/qi_modelling1.R')
fix_pcrit('notebooks/temp_qi.R')
