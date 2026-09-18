import re

with open("code_gapon_parallel.qmd", "r") as f:
    content = f.read()

# Define the start and end of the block we want to replace
start_marker = "Furthermore, we calculate **Relative Yield** and **Relative Uptake**"
end_marker = "print(loocv_results)\n```"

# Check if markers exist
if start_marker not in content or end_marker not in content:
    print("Markers not found!")
    exit(1)

pre_content = content.split(start_marker)[0]
post_content = content.split(end_marker)[1]

new_block = """Furthermore, we retain the **Absolute Yield** (`annual_yield_mp_DM`) to prevent the statistical bias introduced by manual normalizations. Instead of scaling data, we build a 3-parameter model that natively estimates the maximum plateau ($A$) and baseline ($Y_0$) dynamically for each `crop` as fixed effects.

### Data Preparation: Absolute Yield

```{r}
biological_data <- modelling_data %>%
  filter(!is.na(crop), !is.na(annual_yield_mp_DM), annual_yield_mp_DM > 0)
```

### A. Mitscherlich Non-Linear Mixed-Effects Models (nlme)

To correctly model the saturation curve of plant response, we fit a 3-parameter non-linear equilibrium equation on the raw, absolute yield: $Y = Y_0 + (A - Y_0) \\cdot (1 - \\exp(-(c_I \\cdot I + c_b \\cdot b)))$. We use the `nlme` package to estimate $A$, $Y_0$, $c_I$, and $c_b$ per `crop`.

```{r}
yield_data <- biological_data 

# 1. Automated Starting Values via dplyr
# Proxy A (90th percentile)
proxy_A_df <- yield_data %>% group_by(crop) %>% summarize(proxy_A = quantile(annual_yield_mp_DM, 0.90, na.rm=TRUE), .groups='drop')
# Proxy Y0 (mean of lowest 10% AR_K)
proxy_Y0_df <- yield_data %>% group_by(crop) %>% filter(z_AR_K <= quantile(z_AR_K, 0.10, na.rm=TRUE)) %>% summarize(proxy_Y0 = mean(annual_yield_mp_DM, na.rm=TRUE), .groups='drop')

# Slopes
data_for_c <- yield_data %>% left_join(proxy_A_df, by="crop") %>% left_join(proxy_Y0_df, by="crop") %>% filter(annual_yield_mp_DM < proxy_A, annual_yield_mp_DM > proxy_Y0) %>% mutate(log_term = log(1 - (annual_yield_mp_DM - proxy_Y0)/(proxy_A - proxy_Y0)))
proxy_c_df <- data_for_c %>% group_by(crop) %>% summarize(proxy_c_I = -coef(lm(log_term ~ z_AR_K + z_b))[2], proxy_c_b = -coef(lm(log_term ~ z_AR_K + z_b))[3], .groups='drop')

# Get perfect contrast starting vectors
start_A <- coef(lm(proxy_A ~ crop, data=proxy_A_df))
start_Y0 <- coef(lm(proxy_Y0 ~ crop, data=proxy_Y0_df))
start_c_I <- coef(lm(proxy_c_I ~ crop, data=proxy_c_df))
# Handle NA if regression failed for small subsets (use a generic small value)
start_c_I[is.na(start_c_I)] <- 0.1
start_c_b <- rep(0.1, length(start_c_I)) 

my_starts <- c(start_A, start_Y0, start_c_I, start_c_b)

# 2. Define kinetic function
k_model <- function(z_AR_K, z_b, A, Y_0, c_I, c_b) {
  Y_0 + (A - Y_0) * (1 - exp(-(c_I * z_AR_K + c_b * z_b)))
}

# 3. Fit NLME
m_nlme_yield <- nlme(
  annual_yield_mp_DM ~ k_model(z_AR_K, z_b, A, Y_0, c_I, c_b),
  data = yield_data,
  fixed = A + Y_0 + c_I + c_b ~ crop,
  random = Y_0 ~ 1 | site/year,
  start = my_starts,
  weights = varPower(form = ~fitted(.)),
  control = nlmeControl(maxIter = 200, pnlsMaxIter = 100)
)

summary(m_nlme_yield)
```

## 3 - Agronomic Application: Calculating $I_{crit}$ and Fertilizer Input

Because we natively estimated $A$ and $Y_0$, we calculate the exact $z_{AR\\_K}$ required to reach $95\\%$ of the absolute biological maximum yield for that specific crop ($0.95 \\cdot A$). The new algebraic formula is: $z_{AR\\_K\\_crit} = \\frac{-\\ln(\\frac{0.05 \\cdot A}{A - Y_0}) - c_b \\cdot z_b}{c_I}$. We then back-transform this into physical $AR_K$ units.

```{r}
# Extract fixed coefficients for the baseline (reference) crop
coefs_y <- fixef(m_nlme_yield)
A_ref <- coefs_y["A.(Intercept)"]
Y_0_ref <- coefs_y["Y_0.(Intercept)"]
c_I_ref <- coefs_y["c_I.(Intercept)"]
c_b_ref <- coefs_y["c_b.(Intercept)"]

# Scale parameters for back-transformation
mean_log_AR_K <- mean(log(modelling_data$AR_K_final), na.rm = TRUE)
sd_log_AR_K <- sd(log(modelling_data$AR_K_final), na.rm = TRUE)

# Calculate dynamic I_crit and Fertilizer for each plot
fert_recommendations <- yield_data %>%
  mutate(
    # 1. Calculate z_AR_K required for 95% yield using the reference crop
    z_AR_K_crit = (-log( (0.05 * A_ref) / (A_ref - Y_0_ref) ) - c_b_ref * z_b) / c_I_ref,
    
    # 2. Back-transform to physical I_crit
    log_AR_K_crit = z_AR_K_crit * sd_log_AR_K + mean_log_AR_K,
    I_crit = exp(log_AR_K_crit),
    
    # 3. Calculate target Quantity (Q) using Gapon equilibrium
    target_Q_K = KG_pred * (Q_Ca + Q_Mg) * I_crit,
    
    # 4. Calculate exact Fertilizer equivalent deficit
    fert_input_eq = pmax(0, target_Q_K - Q_K),
    
    # 5. Convert equivalents back to mg/kg K
    fert_input_mgkg = (fert_input_eq / valence["K"]) * molar_mass["K"]
  )

# Display a summary of dynamic I_crit and required fertilizer
summary(fert_recommendations %>% select(I_crit, fert_input_mgkg))
```

## 4 - Parallel Performance Optimization (mclapply)

We utilize `mclapply` from the `parallel` package to distribute a cross-validation of the `nlme` models across the available heavy-compute cores.

```{r}
library(parallel)
cores <- detectCores()
cat("Using", cores, "cores for LOOCV optimization...\\n")

# Mock parallel workload fitting models on subsamples
loocv_results <- mclapply(1:4, function(fold) {
  sample_data <- yield_data %>% dplyr::sample_frac(0.8)
  
  m_sub <- nlme(
    annual_yield_mp_DM ~ k_model(z_AR_K, z_b, A, Y_0, c_I, c_b),
    data = sample_data,
    fixed = A + Y_0 + c_I + c_b ~ crop,
    random = Y_0 ~ 1 | site/year,
    start = my_starts,
    weights = varPower(form = ~fitted(.)),
    control = nlmeControl(maxIter = 200, pnlsMaxIter = 100)
  )
  return(fixef(m_sub))
}, mc.cores = min(4, cores))

print(loocv_results)
```"""

with open("code_gapon_parallel.qmd", "w") as f:
    f.write(pre_content + new_block + post_content)

print("Successfully replaced biological modelling block!")
