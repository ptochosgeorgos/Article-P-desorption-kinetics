metric_files <- list.files("models", pattern = "_metrics.rds$", full.names = TRUE)
for(f in metric_files) {
  res <- readRDS(f)
  if (is.matrix(res)) {
     cat(sprintf("%-40s : %.3f (%.3f - %.3f)\n", basename(f), res[1, "Estimate"], res[1, "Q2.5"], res[1, "Q97.5"]))
  } else {
     cat(sprintf("%-40s : Not a standard R2 matrix\n", basename(f)))
  }
}
