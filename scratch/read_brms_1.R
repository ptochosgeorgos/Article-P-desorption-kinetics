suppressPackageStartupMessages(library(brms))
m <- readRDS("models/brms_yield_raw_aae.rds")
print(summary(m))
