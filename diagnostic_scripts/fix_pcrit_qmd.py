import re

filepath = 'notebooks/qi_modelling1.qmd'
with open(filepath, 'r') as f:
    content = f.read()

# Original string in qmd:
old_code = """
D_Pcrit <- D_Yield |>
    mutate(
        site_re = re[as.character(site), "c_base"],
        year_re = 0, # Temporarily zeroing out year effect to debug
        re_total = site_re + year_re,
"""

new_code = """
D_Pcrit <- D_Yield |>
    mutate(
        plot_full_id = paste0(site, "/", plot_nr),
        site_re = ranef(m_yield_nlme)$site[as.character(site), 1],
        plot_re = ranef(m_yield_nlme)$plot_nr[plot_full_id, 1],
        re_total = site_re + plot_re,
"""

# if it's slightly different, we can just replace the block via regex
import sys
if old_code in content:
    content = content.replace(old_code, new_code)
else:
    # Try more flexible regex
    pattern = re.compile(r'D_Pcrit <- D_Yield \|>\n\s*mutate\(\n\s*site_re = re\[as\.character\(site\), "c_base"\],\n\s*year_re = 0.*?\n\s*re_total = site_re \+ year_re,', re.DOTALL)
    content = pattern.sub("""D_Pcrit <- D_Yield |>
    mutate(
        plot_full_id = paste0(site, "/", plot_nr),
        site_re = ranef(m_yield_nlme)$site[as.character(site), 1],
        plot_re = ranef(m_yield_nlme)$plot_nr[plot_full_id, 1],
        re_total = site_re + plot_re,""", content)

with open(filepath, 'w') as f:
    f.write(content)

print("Fixed pcrit chunk in qmd")
