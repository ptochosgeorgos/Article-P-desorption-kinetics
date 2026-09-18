import re

files = ["notebooks/cluster_bayesian_offload.R", "notebooks/cluster_bayesian_offload.qmd"]

for file in files:
    with open(file, "r") as f:
        content = f.read()

    # Add file_refit = "on_change" to every brm() call
    content = re.sub(r'file\s*=\s*("\.\./models/[^"]+")', r'file = \1, file_refit = "on_change"', content)

    with open(file, "w") as f:
        f.write(content)
