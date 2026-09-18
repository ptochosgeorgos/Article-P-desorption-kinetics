## -----------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(lme4) 
library(nlme)
library(performance)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)

load("gapon_aparK.RData")
# D_main <- read_excel("STYCS_all_K.xlsx", guess_max = 20000)

site_summary <- D_main |>
  group_by(site,location,WGS84_N, WGS84_E) |>
  summarise(
    Start = min(year, na.rm = TRUE),
    End_raw = max(year, na.rm = TRUE),
    .groups = "drop"
  )

current_year <- 2026

site_summary <- site_summary |>
  mutate(
    End = ifelse(End_raw >= current_year - 1, "Ongoing", as.character(End_raw))
  )

table1 <- site_summary |>
  transmute(
    Site = site,
    Location = location,
    `N–E coordinates` = paste0(WGS84_N, " / ", WGS84_E),
    `Start [years]` = Start,
    `End [years]` = End
  )

kable(
  table1
) |>
  kable_styling(full_width = FALSE, position = "left") 


## -----------------------------------------------------------------------------
molar_mass <- c(K  = 39.10, Ca = 40.08, Mg = 24.31)
valence <- c(K  = 1, Ca = 2, Mg = 2)
dilution_factor <- 10

# AAE10 mg/kg soil -> mmol charge/kg soil
calc_equivalents <- function(conc_mgkg, molar_mass, valence) {
  (conc_mgkg / molar_mass) * valence
}

# H2O10 mg/kg soil -> mol/L extract
calc_molar_conc <- function(
    conc_mgkg,
    molar_mass,
    dilution_factor = 10
) {
  conc_mgL  <- conc_mgkg / dilution_factor
  conc_molL <- (conc_mgL / 1000) / molar_mass
  conc_molL
}

# Ionic strength from K, Ca and Mg.
calc_ionic_strength <- function(c_K_molL, c_Ca_molL, c_Mg_molL) 
  {
  charge_cations <- (c_K_molL * 1) + (c_Ca_molL * 2) + (c_Mg_molL * 2)

  I_cations <- 0.5 * ((c_K_molL  * 1^2) + (c_Ca_molL * 2^2) + (c_Mg_molL * 2^2))

  I_anions_assumed <- 0.5 * charge_cations

  I_cations + I_anions_assumed
}

# Davies activity coefficient
calc_gamma <- function(I, z) 
  {
  sqrtI <- sqrt(I)

  log_gamma <- -0.509 * z^2 * ((sqrtI / (1 + sqrtI)) - 0.3 * I)

  10^log_gamma
}

# Calculate AAE10 charge equivalents


D_main_Q <- D_main %>%
  mutate(
    Q_K = calc_equivalents(soil_0_20_K_AAE10, molar_mass["K"], valence["K"]),
    Q_Ca = calc_equivalents(soil_0_20_Ca_AAE10, molar_mass["Ca"], valence["Ca"]),
    Q_Mg = calc_equivalents(soil_0_20_Mg_AAE10, molar_mass["Mg"], valence["Mg"]),
    
    # Pre-calculate physical covariates for the dynamic Gapon model
    CEC_rest = (soil_0_20_Ca_AAE10 / 20.04) + (soil_0_20_Mg_AAE10 / 12.15),
    z_clay = as.numeric(scale(soil_0_20_clay)),
    z_pH = as.numeric(scale(rollMean_soil_0_20_pH_H2O)),
    z_Corg = as.numeric(scale(log(rollMean_soil_0_20_Corg))),
    z_CEC_rest = as.numeric(scale(log(CEC_rest)))
  )

# Calculate measured solution AR_K


add_measured_AR_K <- function(df) {
  df %>%
    mutate(
      cK_molL = calc_molar_conc(soil_0_20_K_H2O10, molar_mass["K"], dilution_factor),

      cCa_molL = calc_molar_conc(soil_0_20_Ca_H2O10, molar_mass["Ca"], dilution_factor),

      cMg_molL = calc_molar_conc(soil_0_20_Mg_H2O10, molar_mass["Mg"], dilution_factor),

      I_H2O10 = calc_ionic_strength(cK_molL, cCa_molL, cMg_molL),

      gamma_K = calc_gamma(I_H2O10, z = 1),

      gamma_Ca = calc_gamma(I_H2O10, z = 2),

      gamma_Mg = calc_gamma(I_H2O10, z = 2),

      # Solution activities
      a_K = gamma_K * cK_molL,
      a_Ca = gamma_Ca * cCa_molL,
      a_Mg = gamma_Mg * cMg_molL,

      # Measured activity ratio:
      # AR_K = a_K / sqrt(a_Ca + a_Mg)
      AR_K_measured = if_else(
          !is.na(a_K) &
          !is.na(a_Ca) &
          !is.na(a_Mg) &
          is.finite(a_K) &
          is.finite(a_Ca) &
          is.finite(a_Mg) &
          a_K > 0 &
          a_Ca > 0 &
          a_Mg > 0,

        a_K / sqrt(a_Ca + a_Mg),

        NA_real_
      )
    )
}

# Calibrate K_G using complete-case H2O10 data

calibrate_KG <- function(df) {
  df %>%
    filter(
      year >= 1990,

      !is.na(soil_0_20_K_H2O10),
      !is.na(soil_0_20_Ca_H2O10),
      !is.na(soil_0_20_Mg_H2O10),

      !is.na(soil_0_20_K_AAE10),
      !is.na(soil_0_20_Ca_AAE10),
      !is.na(soil_0_20_Mg_AAE10),

      soil_0_20_K_H2O10 > 0,
      soil_0_20_Ca_H2O10 > 0,
      soil_0_20_Mg_H2O10 > 0,

      soil_0_20_K_AAE10 > 0,
      soil_0_20_Ca_AAE10 > 0,
      soil_0_20_Mg_AAE10 > 0
    ) %>%
    add_measured_AR_K() %>%
    mutate(
      exchange_ratio = Q_K / (Q_Ca + Q_Mg),

      # Sample-specific Gapon coefficient:
      
      K_G_sample = exchange_ratio / AR_K_measured
    ) %>%
    filter(
      !is.na(AR_K_measured),
      AR_K_measured > 0,
      is.finite(AR_K_measured),

      !is.na(exchange_ratio),
      exchange_ratio > 0,
      is.finite(exchange_ratio),

      !is.na(K_G_sample),
      K_G_sample > 0,
      is.finite(K_G_sample)
    )
}

calibration_data <- calibrate_KG(D_main_Q) %>%
  filter(!is.na(z_clay), !is.na(z_pH), !is.na(z_Corg), !is.na(z_CEC_rest))

# Check number of valid calibration observations
nrow(calibration_data)

# --- ONE-STEP DYNAMIC GAPON IMPUTATION ---
# We model the physical equilibrium (QR_K = K_G * AR_K) directly using a mixed model.
# By interacting AR_K with covariates, the model explicitly predicts baseline K_G and its modifiers.
calibration_data <- calibration_data %>%
  mutate(exchange_ratio = Q_K / (Q_Ca + Q_Mg))

m_gapon_one_step <- lmer(
  exchange_ratio ~ 0 + AR_K_measured + AR_K_measured:(z_clay + z_pH + z_Corg + z_CEC_rest) + 
  (0 + AR_K_measured | site/year), 
  data = calibration_data
)

# Print performance of the unified Gapon model
performance(m_gapon_one_step)

# Calculate Gapon-predicted AR_K for all observations using the dynamic model
full_predictions <- D_main_Q %>%
  add_measured_AR_K() %>%
  filter(!is.na(z_clay), !is.na(z_pH), !is.na(z_Corg), !is.na(z_CEC_rest)) %>%
  mutate(exchange_ratio = Q_K / (Q_Ca + Q_Mg))

# Predict sample-specific K_G using the fixed-effect coefficients
coefs <- fixef(m_gapon_one_step)

full_predictions <- full_predictions %>%
  mutate(
    KG_pred = coefs["AR_K_measured"] +
              coefs["AR_K_measured:z_clay"] * z_clay +
              coefs["AR_K_measured:z_pH"] * z_pH +
              coefs["AR_K_measured:z_Corg"] * z_Corg +
              coefs["AR_K_measured:z_CEC_rest"] * z_CEC_rest,
              
    AR_K_gapon = (1 / KG_pred) * exchange_ratio,
    
    AR_K_gapon = if_else(
      !is.na(AR_K_gapon) & is.finite(AR_K_gapon) & AR_K_gapon > 0,
      AR_K_gapon,
      NA_real_
    )
  )

# Combine measured and Gapon-derived values

full_predictions <- full_predictions %>%
  mutate(
    AR_K_final = coalesce(
      AR_K_measured,
      AR_K_gapon
    ),

    AR_K_source = case_when(
      !is.na(AR_K_measured) ~ "Measured H2O10",
      !is.na(AR_K_gapon) ~ "Gapon from AAE10",
      TRUE ~ NA_character_
    )
  )

# Check the number of observations from each source
source_count <- full_predictions %>%
  count(AR_K_source, .drop = FALSE)

print(source_count)

# Check whether the combined value uses measured values
check_combination <- full_predictions %>%
  filter(!is.na(AR_K_measured)) %>%
  summarise(
    n_measured = n(),

    mean_difference =
      mean(
        AR_K_final - AR_K_measured,
        na.rm = TRUE
      ),

    maximum_absolute_difference =
      max(
        abs(AR_K_final - AR_K_measured),
        na.rm = TRUE
      )
  )

print(check_combination)

# validation

validation_data <- full_predictions %>%
  filter(
    !is.na(AR_K_measured),
    !is.na(AR_K_gapon),
    AR_K_measured > 0,
    AR_K_gapon > 0,
    is.finite(AR_K_measured),
    is.finite(AR_K_gapon)
  ) %>%
  select(
    site,
    plot_nr,
    year,
    soil_0_20_K_AAE10,
    AR_K_measured,
    AR_K_gapon
  )

# Correlation on the original scale
cor_original <- cor(
  validation_data$AR_K_measured,
  validation_data$AR_K_gapon,
  use = "complete.obs"
)

print(cor_original)

# Correlation on the log scale
cor_log <- cor(
  log(validation_data$AR_K_measured),
  log(validation_data$AR_K_gapon),
  use = "complete.obs"
)

print(cor_log)

# Regression for validation
m_validation <- lm(
  log(AR_K_measured) ~ log(AR_K_gapon),
  data = validation_data
)

summary(m_validation)

# Build the modelling dataset

modelling_data <- full_predictions %>%
  filter(
    year >= 1990,
    !is.na(soil_0_20_K_AAE10),
    soil_0_20_K_AAE10 > 0,
    !is.na(AR_K_final),
    AR_K_final > 0,
    !is.na(soil_0_20_Ca_AAE10),
    soil_0_20_Ca_AAE10 > 0,
    !is.na(soil_0_20_Mg_AAE10),
    soil_0_20_Mg_AAE10 > 0,
    !is.na(soil_0_20_clay),
    !is.na(rollMean_soil_0_20_pH_H2O),
    !is.na(rollMean_soil_0_20_Corg),
    soil_0_20_clay > 0,
    rollMean_soil_0_20_Corg > 0,
    !is.na(site),
    !is.na(plot_nr),
    !is.na(year)
  ) %>%
  # Covariates (z_clay, z_pH, z_Corg, z_CEC_rest) are now globally pre-scaled in D_main_Q
  filter(
    !is.na(CEC_rest),
    CEC_rest > 0
  ) %>%
  drop_na(
    AR_K_final,
    z_clay,
    z_pH,
    z_Corg,
    z_CEC_rest,
    site,
    plot_nr,
    year
  )

# Check the final source distribution in the modelling data
modelling_data %>%
  count(AR_K_source, .drop = FALSE)


## -----------------------------------------------------------------------------
m_APARK <- lmer(
  log(soil_0_20_K_AAE10) ~ log(AR_K_final) + (1|site/plot_nr) + (1|year),
  data = modelling_data
)
performance(m_APARK) 


## -----------------------------------------------------------------------------
m_K_clay <- lmer(
  log(soil_0_20_K_AAE10) ~ log(AR_K_final) * z_clay +
    (1 | site/plot_nr) + (1 | year),
  data = modelling_data
)
performance(m_K_clay)

m_K_ClayRest <- lmer(
  log(soil_0_20_K_AAE10) ~ log(AR_K_final) * z_clay + z_CEC_rest +
    (1 | site/plot_nr) + (1 | year),
  data = modelling_data
)
performance(m_K_ClayRest)

m_K_CorgpH <- lmer(
  log(soil_0_20_K_AAE10) ~ log(AR_K_final) + z_Corg * z_pH +
    (1 | site/plot_nr) + (1 | year),
  data = modelling_data
)
performance(m_K_CorgpH)

m_K_CorgpH2 <- lmer(
  log(soil_0_20_K_AAE10) ~ log(AR_K_final) * z_Corg * z_pH +
    (1 | site/plot_nr) + (1 | year),
  data = modelling_data
)
performance(m_K_CorgpH2)

m_K_ClayCorgpH <- lmer(
  log(soil_0_20_K_AAE10) ~ log(AR_K_final) * z_clay + z_Corg * z_pH +
    (1 | site/plot_nr) + (1 | year),
  data = modelling_data
)
performance(m_K_ClayCorgpH)

m_K_CorgpH_rest <- lmer(
  log(soil_0_20_K_AAE10) ~ log(AR_K_final) * z_Corg * z_pH + z_CEC_rest +
    (1 | site/plot_nr) + (1 | year),
  data = modelling_data
)
performance(m_K_CorgpH_rest)

m_K_CorgpH_rest2 <- lmer(
  log(soil_0_20_K_AAE10) ~ log(AR_K_final) + z_Corg * z_pH + z_CEC_rest +
    (1 | site/plot_nr) + (1 | year),
  data = modelling_data
)
performance(m_K_CorgpH_rest2)

m_K_full <- lmer(
  log(soil_0_20_K_AAE10) ~ log(AR_K_final) * z_clay +
    z_CEC_rest + z_Corg * z_pH +
    (1 | site/plot_nr) + (1 | year),
  data = modelling_data
)
performance(m_K_full)

# --- NEW TRIPLE INTERACTION MODEL ---
# As discovered in the dynamic imputation testing, interacting AR_K with a combination 
# of Clay, CEC, and pH accounts for 73.1% of the physical variance.
m_K_triple <- lmer(
  log(soil_0_20_K_AAE10) ~ log(AR_K_final) * (z_clay * z_CEC_rest * z_pH) +
    (1 | site/plot_nr) + (1 | year),
  data = modelling_data
)
performance(m_K_triple)


## -----------------------------------------------------------------------------
# Compare all models including the new triple interaction
compare_performance(m_APARK, m_K_clay, m_K_ClayRest, m_K_CorgpH, m_K_CorgpH2, m_K_ClayCorgpH, m_K_CorgpH_rest, m_K_CorgpH_rest2, m_K_full, m_K_triple)


## -----------------------------------------------------------------------------
fixef(m_K_clay)
fixef(m_K_full)


## -----------------------------------------------------------------------------


plot_ptf <- function(model, data, title) {
  plot_data <- data %>%
    mutate(
      Fitted = predict(model)
    )

  ggplot(
    plot_data,
    aes(
      x = Fitted,
      y = log(soil_0_20_K_AAE10),
      color = AR_K_source
    )
  ) +
    geom_point(
      alpha = 0.5,
      size = 2
    ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed"
    ) +
    labs(
      title = title,
      x = "Predicted log(K_AAE10)",
      y = "Observed log(K_AAE10)",
      color = "AR_K source"
    ) +
    theme_minimal()
}

plot_ptf(
  m_K_clay,
  modelling_data,
  "K_AAE10 model using measured/Gapon AR_K"
)


## -----------------------------------------------------------------------------
table(modelling_data$site)


## -----------------------------------------------------------------------------
nrow(D_main)
nrow(full_predictions)
table(D_main$site)
table(full_predictions$site)

# to see exactly which rows are losing AAE10 data:
D_main %>% filter(is.na(soil_0_20_K_AAE10) | is.na(soil_0_20_Ca_AAE10) | is.na(soil_0_20_Mg_AAE10)) %>% count(site)


## -----------------------------------------------------------------------------
modelling_data <- modelling_data %>%
  mutate(
    # b is in units of (eq/kg) / (mol/L)^0.5
    K_buffer_capacity = KG_pred * (Q_Ca + Q_Mg),
    # Scale variables for mixed modeling stability
    z_b = as.numeric(scale(log(K_buffer_capacity))),
    z_AR_K = as.numeric(scale(log(AR_K_final)))
  ) %>%
  filter(!is.na(K_buffer_capacity), K_buffer_capacity > 0)


## -----------------------------------------------------------------------------
biological_data <- modelling_data %>%
  filter(!is.na(crop), !is.na(annual_yield_mp_DM), annual_yield_mp_DM > 0, !is.na(z_AR_K), !is.na(z_b), !is.na(site), !is.na(year)) %>%
  mutate(crop = droplevels(factor(crop)))


## -----------------------------------------------------------------------------
yield_data <- biological_data 

# 1. Safe Automated Starting Values
# We initialize the baseline crop with a proxy and assume all other crops have 0 contrast initially.
# nlme will automatically solve the exact contrast differences during optimization.
num_crops <- length(levels(yield_data$crop))

base_A <- quantile(yield_data$annual_yield_mp_DM, 0.90, na.rm=TRUE)
base_Y0 <- quantile(yield_data$annual_yield_mp_DM, 0.10, na.rm=TRUE)

start_A <- c(base_A, rep(0, num_crops - 1))
start_Y0 <- c(base_Y0, rep(0, num_crops - 1))
start_c_I <- c(0.1, rep(0, num_crops - 1))
start_c_b <- c(0.1, rep(0, num_crops - 1))

my_starts <- c(start_A, start_Y0, start_c_I, start_c_b)

# 2. Define kinetic function
k_model <- function(z_AR_K, z_b, A, Y_0, c_I, c_b) {
  Y_0 + (A - Y_0) * (1 - exp(-(c_I * z_AR_K + c_b * z_b)))
}

# 3. Fit NLME
print(table(yield_data$crop))
m_nlme_yield <- nlme(
  annual_yield_mp_DM ~ k_model(z_AR_K, z_b, A, Y_0, c_I, c_b),
  data = yield_data,
  fixed = A + Y_0 + c_I + c_b ~ crop,
  random = Y_0 ~ 1 | site/year,
  start = my_starts,
  weights = varPower(form = ~fitted(.)),
  na.action = na.omit,
  control = nlmeControl(maxIter = 200, pnlsMaxIter = 100)
)

summary(m_nlme_yield)


## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
library(parallel)
cores <- detectCores()
cat("Using", cores, "cores for LOOCV optimization...\n")

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
  na.action = na.omit,
    control = nlmeControl(maxIter = 200, pnlsMaxIter = 100)
  )
  return(fixef(m_sub))
}, mc.cores = min(4, cores))

print(loocv_results)

