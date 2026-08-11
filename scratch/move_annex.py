import os

filepath = "presentation/index.qmd"

with open(filepath, "r") as f:
    lines = f.readlines()

annex_start = -1
annex_end = -1

for i, line in enumerate(lines):
    if line.startswith("## Annex (Methodological Details)"):
        annex_start = i
    elif line.startswith("## The PTF Parameter Space"):
        annex_end = i
        break

if annex_start != -1 and annex_end != -1:
    # The annex is from annex_start to annex_end (exclusive)
    annex_content = lines[annex_start:annex_end]
    
    # Remove it from the current position
    new_lines = lines[:annex_start] + lines[annex_end:]
    
    # Ensure there's a newline before appending
    if not new_lines[-1].endswith("\n"):
        new_lines[-1] += "\n"
    
    # Append the annex to the very end
    new_lines.extend(["\n", "---\n", "\n"])
    new_lines.extend(annex_content)
    
    with open(filepath, "w") as f:
        f.writelines(new_lines)
    print(f"Moved {len(annex_content)} lines to the end.")
else:
    print("Could not find Annex boundaries.")
