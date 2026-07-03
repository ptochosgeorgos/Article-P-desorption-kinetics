library(dplyr)
library(nlme)

D_ready <- readRDS("data/3_D_ready.rds")

# Recreate C_agro, get_int_agro to run the code
# I will just load them from the R script without running the models
source("notebooks/qi_modelling1.R", local = TRUE)

# Wait, if I source qi_modelling1.R, it will run the whole thing.
# Let's just run the exact lines from qi_modelling1.qmd
