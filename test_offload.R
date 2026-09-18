library(nlme)
library(parallel)

cat("Starting intensive parallel test on LXC container...\n")
cat("Detected cores:", detectCores(), "\n")

# Mock data
set.seed(42)
df <- data.frame(
  y = rnorm(100),
  x = rnorm(100),
  group = factor(rep(1:10, each=10))
)

# Run a mock parallel process
cat("Running mock mclapply...\n")
results <- mclapply(1:4, function(i) {
  # Fit a mixed model on a subset
  m <- lme(y ~ x, random = ~ 1 | group, data = df)
  return(summary(m)$tTable)
}, mc.cores = min(4, detectCores()))

cat("Test completed successfully! Saving results to disk...\n")
saveRDS(results, "lxc_test_results.rds")
