# Heavy Computations & Rendering (LXC Offload)

The user has an LXC container (IP: `192.168.1.54`) equipped with 8 cores dedicated to running heavy R scripts and Quarto `.qmd` renderings.

- **Do NOT** run intensive models (`lmer`, `nlme`, large bootstrapping) or full `quarto render` directly on the local machine unless instructed otherwise.
- **Instead**, use the provided `./crunch.sh` wrapper script in the root directory.
  - Example for R scripts: `./crunch.sh script.R`
  - Example for Quarto: `./crunch.sh notebooks/document.qmd`

This script automatically syncs the local directory to the LXC node, executes the heavy computation using the container's CPUs, and syncs the results (like `.rds` or `.html` files) back to the local workspace seamlessly.
