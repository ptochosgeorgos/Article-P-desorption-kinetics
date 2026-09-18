import re

filename = "writing/_20-methods.qmd"
with open(filename, "r") as f:
    content = f.read()

# Replace inline block math: $$ equation $$ -> $$\n equation \n$$
# Only match if it starts with $$ and ends with $$, and has no newlines inside
def replacer(match):
    eq = match.group(1).strip()
    return f"$$\n{eq}\n$$"

# regex pattern: $$ (anything not containing \n) $$
pattern = re.compile(r"^\$\$(.*?)\$\$", re.MULTILINE)
new_content = pattern.sub(replacer, content)

with open(filename, "w") as f:
    f.write(new_content)

print("Math blocks reformatted.")
