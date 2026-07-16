library(tidyverse)
library(nlme)

final_artifacts <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_artifacts$data$D_Long_Agro
D_Yield <- final_artifacts$data$D_Yield

# Ensure crop is a factor and drop empty levels
D_Long_Agro$crop <- droplevels(as.factor(D_Long_Agro$crop))
n_crops <- length(levels(D_Long_Agro$crop))

mod_test <- tryCatch({
  nlme(
      Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) / 
                        ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + soil_0_20_P_CO2),
      data = D_Long_Agro,
      fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop, beta_invb ~ 1),
      random = U_base ~ 1 | site / plot_nr,
      start = c(0.68, -0.03, 0.1, 0.1, median(D_Long_Agro$soil_0_20_P_CO2), rep(0, n_crops - 1), 0),
      control = nlmeControl(maxIter = 1000, returnObject = TRUE)
  )
}, error = function(e) e)

if (inherits(mod_test, "error")) {
  print("Model failed to converge:")
  print(mod_test$message)
} else {
  print("Model converged successfully!")
  print(summary(mod_test))
}
