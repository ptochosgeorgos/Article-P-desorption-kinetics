library(knitr)
purl("notebooks/qi_modelling_parallel.qmd", output = "scratch/qi_code.R", documentation = 0)

# Read the R script and stop before the yield model evaluation
lines <- readLines("scratch/qi_code.R")
# Find the line with m_yield_raw_co2
idx <- grep("jobs_yield <- list", lines)
if(length(idx) > 0) {
    cat(lines[1:(idx[1]-1)], sep="\n", file="scratch/qi_code_subset.R")
} else {
    stop("Could not find yield jobs definition")
}
