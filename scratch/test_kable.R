library(knitr)
variables_df <- data.frame(
  Abbreviation = c("$Y_{rel}$", "$Uptake_{rel}$", "$\\ln(P_{AAE10})$"),
  Variable = c("Relative Yield", "Relative P Uptake", "Log Quantity Pool"),
  Description = c("Yield normalized", "Uptake normalized", "AAE10"),
  `Processing and Filtering` = c("Norm", "Norm", "Log"),
  check.names = FALSE
)

cat("Output of knitr::kable(format='markdown'):\n\n")
print(knitr::kable(variables_df, format = "markdown"))
