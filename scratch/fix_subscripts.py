import re

file_path = "/home/marc/Documents/Agroscope/Article-P-desorption-kinetics/writing/_20-methods.qmd"

with open(file_path, "r") as f:
    content = f.read()

# Dictionary of replacements
replacements = {
    r"_{rel}": r"_{\text{rel}}",
    r"_{ox}": r"_{\text{ox}}",
    r"_{org}": r"_{\text{org}}",
    r"_{Anom}": r"_{\text{Anom}}",
    r"_{base}": r"_{\text{base}}",
    r"_{eff}": r"_{\text{eff}}",
    r"_{up}": r"_{\text{up}}",
    r"_{crit}": r"_{\text{crit}}",
    r"_{max}": r"_{\text{max}}",
    r"_{min}": r"_{\text{min}}",
    r"_{uptake}": r"_{\text{uptake}}",
    r"_{base\\_crop}": r"_{\text{base\_crop}}",
    r"_{invb}": r"_{\text{invb}}",
    r"_{temp}": r"_{\text{temp}}",
    r"_{acc}": r"_{\text{acc}}",
    r"_{desorption}": r"_{\text{desorption}}",
}

for old, new in replacements.items():
    content = re.sub(old, new, content)

# Special cases for CO2 and AAE10 in text body
content = re.sub(r"_{CO2}", r"_{\text{CO}_2}", content)
content = re.sub(r"_{CO_2}", r"_{\text{CO}_2}", content)
content = re.sub(r"_{AAE10}", r"_{\text{AAE10}}", content)
content = re.sub(r"Cum\\_P\\_bal", r"Cum\_P\_{\text{bal}}", content)


with open(file_path, "w") as f:
    f.write(content)

print("Subscripts formatted.")
