source("notebooks/qi_modelling1.R")
cat("Correlation between P_AAE10 and Yield:\n")
print(cor(D_Yield$soil_0_20_P_AAE10, D_Yield$Relative_Yield, use="complete.obs"))

cat("\nLinear model fit:\n")
summary(lm(Relative_Yield ~ log(soil_0_20_P_AAE10), data=D_Yield))
