library(jsonlite)

# Load coefficients
calc_coefs <- fromJSON("presentation/calculator_coefs.json")
ptf_coefs <- fromJSON("presentation/ptf_coefs.json")

# Crop: WW (Winter Wheat)
crop <- "WW"

# Extract Yield parameters (assuming average soil pH=7.3, K, Mg, etc. so z-scores = 0)
# c_eff = c_base_crop * exp(0) = c_base_crop
c_base <- calc_coefs$yield_coefs$c_base_crops[[crop]]
E_base <- calc_coefs$yield_coefs$E_base
c_eff <- c_base

# Extract Uptake parameters
# Km = K_base_crop * exp(0) = K_base_crop
K_base <- calc_coefs$uptake_coefs$K_base_crops[[crop]]
Km <- K_base

cat("Crop:", crop, "\n")
cat("c_eff:", c_eff, "\n")
cat("E_base:", E_base, "\n")
cat("Km:", Km, "\n")

# Equation 1: Minimum C_P (End of Steenbjerg effect)
# d/dI ln(P_up) = d/dI ln(Y)
# Km / (I * (Km + I)) = c_eff * exp(-c_eff*(I + E_base)) / (1 - exp(-c_eff*(I + E_base)))

f_min_cp <- function(I) {
  left <- Km / (I * (Km + I))
  Z <- exp(-c_eff * (I + E_base))
  right <- c_eff * Z / (1 - Z)
  return(left - right)
}

# Find root between 0.0001 and 5.0
res_min_cp <- uniroot(f_min_cp, lower = 0.0001, upper = 5.0, extendInt = "yes")
I_min_cp <- res_min_cp$root
cat("\nTheoretical I for Minimum C_P (End of Steenbjerg):", I_min_cp, "mg/L\n")

# Calculate Yield at this point
Y_min_cp <- 1 - exp(-c_eff * (I_min_cp + E_base))
cat("Yield at this point:", Y_min_cp * 100, "%\n")

# Let's check 95% Yield target for comparison
I_95 <- (-log(1 - 0.95) / c_eff) - E_base
cat("\nArbitrary 95% Yield Target I:", I_95, "mg/L\n")

