library(knitr)
purl("notebooks/qi_modelling_parallel.qmd", output = "scratch/qi_code5.R", documentation = 0)

lines <- readLines("scratch/qi_code5.R")
idx <- grep("jobs_yield <- list", lines)
if(length(idx) > 0) {
    cat(lines[1:(idx[1]-1)], sep="\n", file="scratch/qi_code_subset5.R")
} else {
    stop("Could not find yield jobs definition")
}

source("scratch/qi_code_subset5.R")
library(nlme)

cat("Dimensions of D_Yield:", dim(D_Yield), "\n")
cat("Crops in D_Yield:\n")
print(table(D_Yield$crop))

