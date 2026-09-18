import re

file_path = "/home/marc/Documents/Agroscope/Article-P-desorption-kinetics/writing/_20-methods.qmd"

with open(file_path, "r") as f:
    content = f.read()

# Fix the tab + ext issues introduced by the previous script
content = content.replace("_{\t", "_{\\t") # just in case
content = content.replace("_{	ext", "_{\\text")

with open(file_path, "w") as f:
    f.write(content)

print("Fixed tabs.")
