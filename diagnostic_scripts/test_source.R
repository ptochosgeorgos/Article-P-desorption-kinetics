# Source everything in qi_modelling1.R line by line, but wrap key blocks in tryCatch to locate failure.

library(nlme)
library(dplyr)
library(tidyr)
library(robustlmm)
library(performance)
library(kableExtra)

# Read the purled script
lines <- readLines("qi_modelling1.R")

# We want to run setup, create-master-dataset, ptf-showdown, plant-uptake-showdown, residual-diagnostics-uptake
# Let's find where the mitscherlich chunk starts.
# In the R script, line 396 is where m_yield_co2 starts.
# Let's run lines 1 to 395 first
cat("Running setup and data prep...\n")
eval(parse(text = paste(lines[1:395], collapse = "\n")))
cat("Finished setup and prep. D_Yield has rows:", nrow(D_Yield), "\n")

cat("Fitting m_yield_co2...\n")
tryCatch({
  eval(parse(text = paste(lines[396:403], collapse = "\n")))
  cat("m_yield_co2 succeeded!\n")
}, error = function(e) {
  cat("m_yield_co2 failed with: ", e$message, "\n")
})

cat("Fitting m_yield_thm...\n")
tryCatch({
  eval(parse(text = paste(lines[404:412], collapse = "\n")))
  cat("m_yield_thm succeeded!\n")
}, error = function(e) {
  cat("m_yield_thm failed with: ", e$message, "\n")
})

cat("Fitting m_yield_aae...\n")
tryCatch({
  eval(parse(text = paste(lines[413:421], collapse = "\n")))
  cat("m_yield_aae succeeded!\n")
}, error = function(e) {
  cat("m_yield_aae failed with: ", e$message, "\n")
})
