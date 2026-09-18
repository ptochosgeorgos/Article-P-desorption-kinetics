models <- list.files("models", full.names=TRUE, pattern="\\.rds$")
for (m in models) {
  cat("---", m, "---\n")
  obj <- try(readRDS(m), silent=TRUE)
  if (inherits(obj, "try-error")) {
    cat("Error reading\n")
    next
  }
  print(class(obj))
  if (inherits(obj, c("lmerMod", "lmerModLmerTest", "glmerMod", "nlme", "lme"))) {
    print(formula(obj))
  } else if (inherits(obj, "list")) {
    print(names(obj))
    if (length(obj) > 0) {
      print(class(obj[[1]]))
      if (inherits(obj[[1]], c("lmerMod", "lmerModLmerTest", "glmerMod", "nlme", "lme"))) {
         print(formula(obj[[1]]))
      }
    }
  } else if (inherits(obj, "data.frame")) {
    cat("Rows:", nrow(obj), "Cols:", ncol(obj), "\n")
    print(head(colnames(obj), 10))
  }
  cat("\n")
}
