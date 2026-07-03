import re

def fix_pcrit2(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # The broken syntax is:
    #         # Extract random effects for site and plot
    #         re_site <- ranef(m_yield_nlme)$site
    #         re_plot <- ranef(m_yield_nlme)$plot_nr
    # 
    #         # Calculate the crop-specific base (if crop is the reference, the contrast is NA so we replace with 0)
    #         c_base_crop = cf["c_base.(Intercept)"] + tidyr::replace_na(cf[paste0("c_base.crop", crop)], 0),
    # 
    #         # Total random effect
    #         plot_full_id = paste0(site, "/", plot_nr),
    #         re_total = re_site[as.character(site), "(Intercept)"] + re_plot[plot_full_id, "(Intercept)"],
    # 
    #         c_eff  = (c_base_crop + tidyr::replace_na(re_total, 0)) * exp(

    bad_pattern = re.compile(
        r'# Extract random effects for site and plot\s*re_site <- ranef\(m_yield_nlme\)\$site\s*re_plot <- ranef\(m_yield_nlme\)\$plot_nr\s*# Calculate the crop-specific base.*?c_eff\s*=\s*\(c_base_crop \+ tidyr::replace_na\(re_total, 0\)\) \* exp\(',
        re.DOTALL
    )

    # We will lift the extraction BEFORE D_Pcrit <- ...
    # We find `D_Pcrit <- D_Yield |> \n    mutate(`
    # and put `re_site <- ranef(m_yield_nlme)$site; re_plot <- ranef(m_yield_nlme)$plot_nr` BEFORE it.
    
    # First, let's just replace the bad syntax inside mutate to use standard lookups directly from global env:
    new_mutate_code = """
        c_base_crop = cf["c_base.(Intercept)"] + tidyr::replace_na(cf[paste0("c_base.crop", crop)], 0),
        plot_full_id = paste0(site, "/", plot_nr),
        re_total = ranef(m_yield_nlme)$site[as.character(site), "(Intercept)"] + ranef(m_yield_nlme)$plot_nr[plot_full_id, "(Intercept)"],
        c_eff  = (c_base_crop + tidyr::replace_na(re_total, 0)) * exp(
"""
    
    content = bad_pattern.sub(new_mutate_code.strip() + '\n', content)

    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Fixed Pcrit syntax again in {filepath}")

fix_pcrit2('notebooks/qi_modelling1.qmd')
fix_pcrit2('notebooks/qi_modelling1.R')
fix_pcrit2('notebooks/temp_qi.R')
