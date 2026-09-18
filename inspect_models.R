library(brms)
models <- list.files("../models", pattern = "\\.rds$", full.names = TRUE)
cat("Found models:\n")
print(models)

for (mod_file in models) {
  cat("\n========================================\n")
  cat("Inspecting:", basename(mod_file), "\n")
  mod <- tryCatch(readRDS(mod_file), error = function(e) NULL)
  
  if (!is.null(mod) && inherits(mod, "brmsfit")) {
    cat("Observations:", nobs(mod), "\n")
    print(fixef(mod))
    
    # Check for loo
    if (!is.null(mod$criteria$loo)) {
       cat("\nLOO computed (elpd_loo = ", mod$criteria$loo$estimates["elpd_loo", "Estimate"], ")\n")
    }
  } else {
    cat("Could not load as brmsfit.\n")
  }
}
