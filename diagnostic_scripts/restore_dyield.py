import re

with open('notebooks/qi_modelling1.R', 'r') as f:
    r_content = f.read()

# Extract D_Yield creation block from R file
d_yield_block = re.search(r'# 1\. Prepare the Dataset for YIELD \(Grouped by Site AND Crop\).*?m_yield_raw_co2 <- nlme\(', r_content, re.DOTALL)
if not d_yield_block:
    d_yield_block = re.search(r'# 1\. Prepare the Dataset for YIELD \(Grouped by Site AND Crop\).*?n_crops <- length', r_content, re.DOTALL)
if not d_yield_block:
    d_yield_block = re.search(r'# 1\. Prepare the Dataset for YIELD \(Grouped by Site AND Crop\).*?m_yield_nlme <- nlme\(', r_content, re.DOTALL)

if d_yield_block:
    d_yield_code = d_yield_block.group(0)
    # Remove the m_yield... <- nlme( part from the end
    d_yield_code = re.sub(r'm_yield_.*$', '', d_yield_code).strip()
    d_yield_code = re.sub(r'n_crops <-.*$', '', d_yield_code).strip()
else:
    print("Could not find D_Yield block in R file")
    sys.exit(1)

with open('notebooks/qi_modelling1.qmd', 'r') as f:
    qmd_content = f.read()

# Replace the top of mitscherlich-yield-models chunk with the D_Yield code + n_crops
old_chunk_start = '```{r mitscherlich-yield-models, fig.width=14, fig.height=5}\nn_crops <- length(levels(D_Yield$crop))\n\nm_yield_raw_co2 <- nlme('
old_chunk_start_alt = '```{r mitscherlich-yield-models, fig.width=10, fig.height=5}\nm_yield_raw_co2 <- nlme('
old_chunk_start_alt2 = '```{r mitscherlich-yield-models, fig.width=10, fig.height=5}\nn_crops <- length(levels(D_Yield$crop))\n\nm_yield_raw_co2 <- nlme('

new_chunk_start = '```{r mitscherlich-yield-models, fig.width=14, fig.height=5}\n' + d_yield_code + '\n\nn_crops <- length(levels(D_Yield$crop))\n\nm_yield_raw_co2 <- nlme('

if old_chunk_start in qmd_content:
    qmd_content = qmd_content.replace(old_chunk_start, new_chunk_start)
elif old_chunk_start_alt in qmd_content:
    qmd_content = qmd_content.replace(old_chunk_start_alt, new_chunk_start)
elif old_chunk_start_alt2 in qmd_content:
    qmd_content = qmd_content.replace(old_chunk_start_alt2, new_chunk_start)
else:
    # use regex
    qmd_content = re.sub(r'```{r mitscherlich-yield-models.*?\nm_yield_raw_co2 <- nlme\(', new_chunk_start, qmd_content, count=1, flags=re.DOTALL)

with open('notebooks/qi_modelling1.qmd', 'w') as f:
    f.write(qmd_content)

print("Restored D_Yield definition to qmd file!")
