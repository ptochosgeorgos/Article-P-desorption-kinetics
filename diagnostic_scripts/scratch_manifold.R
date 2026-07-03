library(dplyr)
library(rpart)
source("notebooks/qi_modelling1.R")

cat("Extracting ranges from D_Yield...\n")
# Extract limits for our covariates
covs <- c("z_inv_b", "z_pH", "z_ln_K", "z_ln_Mg", "z_fert_N", "z_Temp_Mean", "z_Prec_Anom", 
          "z_ln_FineTexture", "z_ln_Ca", "z_ln_Corg", "z_Temp_Anom")

ranges <- lapply(covs, function(x) c(min = min(D_Yield[[x]], na.rm=TRUE), max = max(D_Yield[[x]], na.rm=TRUE)))
names(ranges) <- covs

cat("Generating Monte Carlo grid (100,000 points)...\n")
set.seed(42)
N <- 100000
grid <- data.frame(
    crop = factor(rep("WW", N), levels = levels(D_Yield$crop)),
    z_inv_b = runif(N, ranges$z_inv_b[1], ranges$z_inv_b[2]),
    z_pH = runif(N, ranges$z_pH[1], ranges$z_pH[2]),
    z_ln_K = runif(N, ranges$z_ln_K[1], ranges$z_ln_K[2]),
    z_ln_Mg = runif(N, ranges$z_ln_Mg[1], ranges$z_ln_Mg[2]),
    z_fert_N = runif(N, ranges$z_fert_N[1], ranges$z_fert_N[2]),
    z_Temp_Mean = runif(N, ranges$z_Temp_Mean[1], ranges$z_Temp_Mean[2]),
    z_Prec_Anom = runif(N, ranges$z_Prec_Anom[1], ranges$z_Prec_Anom[2]),
    z_ln_FineTexture = runif(N, ranges$z_ln_FineTexture[1], ranges$z_ln_FineTexture[2]),
    z_ln_Ca = runif(N, ranges$z_ln_Ca[1], ranges$z_ln_Ca[2]),
    z_ln_Corg = runif(N, ranges$z_ln_Corg[1], ranges$z_ln_Corg[2]),
    z_Temp_Anom = runif(N, ranges$z_Temp_Anom[1], ranges$z_Temp_Anom[2])
)

cat("Evaluating Yield Model (P_crit)...\n")
cf_y <- fixef(m_yield_nlme)
# We use baseline Winter Wheat intercept
c_base_ww <- cf_y["c_base.(Intercept)"] + cf_y["c_base.cropWW"]
grid$c_eff <- c_base_ww * exp(
    cf_y["beta_invb"] * grid$z_inv_b +
    cf_y["beta_pH"] * grid$z_pH +
    cf_y["beta_K"] * grid$z_ln_K +
    cf_y["beta_Mg"] * grid$z_ln_Mg +
    cf_y["beta_N"] * grid$z_fert_N +
    cf_y["beta_Temp"] * grid$z_Temp_Mean +
    cf_y["beta_Prec"] * grid$z_Prec_Anom
)
grid$P_crit <- (log(20) / grid$c_eff) - cf_y["E_base"]

cat("Evaluating PTF Model (Q_crit)...\n")
# If c_eff < 0, P_crit is negative (absurd). We bound P_crit to avoid log errors.
grid <- grid |> filter(P_crit > 0)
grid$ln_P_CO2 <- log(grid$P_crit)

# Predict using PTF fixed effects
grid$pred_ln_Q <- predict(ptf_practical_raw, newdata = grid, re.form = NA)
grid$Q_crit <- exp(grid$pred_ln_Q)

# Calculate Delta Q
# We assume a depleted soil test of P_AAE10 = 10 for the Delta_Q calculation, 
# or we just bound Q_crit directly. 
# "Q(P_crit) - P_AAE10 <= 50". If P_AAE10 is around 20, then Q_crit <= 70. 
# Let's just bound Q_crit <= 60 (which implies Delta Q ~ 50 if P_AAE is 10).
# Even better: The user said "limit for Q(P_crit) - P_AAE10".
# Let's add a depleted P_AAE10 = 10 as baseline.
grid$Delta_Q <- grid$Q_crit - 10

cat("Classifying Safe vs Danger Zones...\n")
grid$Applicable <- ifelse(grid$Delta_Q <= 50, "Safe", "Danger")
grid$Applicable <- factor(grid$Applicable, levels=c("Danger", "Safe"))

cat("Safe points:", sum(grid$Applicable == "Safe"), " | Danger points:", sum(grid$Applicable == "Danger"), "\n")

cat("Training Decision Tree...\n")
tree <- rpart(Applicable ~ z_inv_b + z_pH + z_Temp_Mean + z_Prec_Anom + z_ln_FineTexture + z_ln_Ca + z_ln_Corg, 
              data = grid, 
              method = "class", 
              control = rpart.control(cp = 0.05, maxdepth = 3))

print(tree)
