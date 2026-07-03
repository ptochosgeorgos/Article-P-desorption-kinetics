cat << 'EOF' > scratch_env.R
source("notebooks/qi_modelling1.R")
library(ggplot2)

# Set an environmental danger threshold for P_CO2 (Intensity)
P_CO2_DANGER <- 1.0
D_env <- D_Yield |> filter(!is.na(inv_b))

# We want to use the PTF model to predict Q (P_AAE10) at this dangerous Intensity
# Ensure the dataframe matches what ptf_practical_raw expects
D_env$ln_P_CO2_actual <- D_env$ln_P_CO2
D_env$ln_P_CO2 <- log(P_CO2_DANGER)

D_env$pred_ln_Q_env <- predict(ptf_practical_raw, newdata = D_env, re.form = NA)
D_env$Q_env_limit <- exp(D_env$pred_ln_Q_env)

# Plot Q_env_limit vs inv_b
cat("Correlation between Q_env_limit and inv_b:", cor(D_env$Q_env_limit, D_env$inv_b, use="complete.obs"), "\n")
cat("Correlation between Q_env_limit and 1/inv_b (which is b):", cor(D_env$Q_env_limit, 1/D_env$inv_b, use="complete.obs"), "\n")

EOF
Rscript scratch_env.R
