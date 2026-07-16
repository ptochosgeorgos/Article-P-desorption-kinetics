import re

with open("presentation/index.qmd", "r") as f:
    content = f.read()

# 1. Replace callouts with HTML <details>
# Match `::: {.callout-note collapse="true" title="Something"}`
def replace_callout(match):
    title = match.group(1)
    return f"<details>\n<summary>{title}</summary>"

content = re.sub(r':::\s*\{\.callout-note\s+collapse="true"\s+title="([^"]+)"\}', replace_callout, content)

# To replace the closing `:::` for these callouts, we can just replace all `:::` that are on their own line 
# IF they were opened by the details tag. But Quarto might have other `:::`.
# Let's assume all `:::` currently in the file are closing the callouts we just opened, 
# or we can do a more careful replacement.
content = re.sub(r'^:::$', r'</details>', content, flags=re.MULTILINE)

# 2. Add blank lines before lists
content = content.replace("**Effect Structure: The Physical Buffer**\n-", "**Effect Structure: The Physical Buffer**\n\n-")
content = content.replace("**Effect Structure: The Pedotransfer Function**\n-", "**Effect Structure: The Pedotransfer Function**\n\n-")
content = content.replace("**Effect Structure: Effective Diffusion Penalty**\n-", "**Effect Structure: Effective Diffusion Penalty**\n\n-")
content = content.replace("**Effect Structure: Plant Uptake**\nWe explicitly structure the fixed and random effects to penalize nutrient delivery based on the physical chemistry of diffusion and desorption.\n-", "**Effect Structure: Plant Uptake**\nWe explicitly structure the fixed and random effects to penalize nutrient delivery based on the physical chemistry of diffusion and desorption.\n\n-")
content = content.replace("**Effect Structure: Additive Recommendation**\n-", "**Effect Structure: Additive Recommendation**\n\n-")
content = content.replace("**The Failure of Empirical Concentrations:**\n-", "**The Failure of Empirical Concentrations:**\n\n-")
content = content.replace("**The Key Variables:**\n-", "**The Key Variables:**\n\n-")


# 3. Add .smaller to slide headers
# We want to match `## Slide Title { .scrollable }` -> `## Slide Title { .scrollable .smaller }`
# Or `## Slide Title` -> `## Slide Title { .smaller }` (only if it doesn't have `{`)
def add_smaller(match):
    full_line = match.group(0)
    if '{' in full_line:
        if '.smaller' not in full_line:
            return full_line.replace('}', '.smaller }')
        return full_line
    else:
        return full_line + " { .smaller }"

content = re.sub(r'^##\s+.*$', add_smaller, content, flags=re.MULTILINE)

# 4. Move Slide 10 ("The PTF Parameter Space") to Annex
# It starts at `## The PTF Parameter Space` and ends before `## Theoretical Background`
ptf_start = content.find("## The PTF Parameter Space")
ptf_end = content.find("## Theoretical Background")

if ptf_start != -1 and ptf_end != -1:
    ptf_slide = content[ptf_start:ptf_end]
    content = content[:ptf_start] + content[ptf_end:]
    
    annex_marker = "## Annex (Methodological Details)"
    annex_idx = content.find(annex_marker)
    if annex_idx != -1:
        # insert after the annex header
        annex_end_idx = content.find("\n", annex_idx)
        content = content[:annex_end_idx+1] + "\n\n" + ptf_slide + content[annex_end_idx+1:]

with open("presentation/index.qmd", "w") as f:
    f.write(content)

print("Done")
