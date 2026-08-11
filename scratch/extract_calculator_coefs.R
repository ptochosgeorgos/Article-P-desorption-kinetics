library(jsonlite)
library(nlme)
library(dplyr)

final_artifacts <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_artifacts$data$D_Long_Agro
D_Yield <- final_artifacts$data$D_Yield

# Ensure crop is a factor and drop empty levels
D_Long_Agro$crop <- droplevels(as.factor(D_Long_Agro$crop))
n_crops <- length(levels(D_Long_Agro$crop))

# FIT THE UPTAKE MODEL WITH CROP
m_up <- nlme(
    Relative_Uptake ~ ((U_base + beta_temp * z_Temp_Anom + beta_N * z_fert_N + beta_v0 * z_v0) * soil_0_20_P_CO2) / 
                      ((K_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) + soil_0_20_P_CO2),
    data = D_Long_Agro,
    fixed = list(U_base ~ 1, beta_temp ~ 1, beta_N ~ 1, beta_v0 ~ 1, K_base ~ crop, beta_invb ~ 1),
    random = U_base ~ 1 | site / plot_nr,
    start = c(0.68, -0.03, 0.1, 0.1, median(D_Long_Agro$soil_0_20_P_CO2), rep(0, n_crops - 1), 0),
    control = nlmeControl(maxIter = 1000)
)

# LOAD YIELD MODEL
m_yd <- final_artifacts$models$yield_raw_co2

# Extract Uptake Coefs
up_cf <- fixef(m_up)
K_base_int <- up_cf["K_base.(Intercept)"]
crops <- levels(D_Long_Agro$crop)

K_base_crops <- list()
for (c in crops) {
  cf_name <- paste0("K_base.crop", c)
  if (cf_name %in% names(up_cf)) {
    K_base_crops[[c]] <- unname(K_base_int + up_cf[cf_name])
  } else {
    K_base_crops[[c]] <- unname(K_base_int)
  }
}

uptake_coefs <- list(
  U_base = unname(up_cf["U_base"]),
  beta_temp = unname(up_cf["beta_temp"]),
  beta_N = unname(up_cf["beta_N"]),
  beta_v0 = unname(up_cf["beta_v0"]),
  beta_invb = unname(up_cf["beta_invb"]),
  K_base_crops = K_base_crops
)

# Extract Yield Coefs
yd_cf <- fixef(m_yd)
c_base_int <- yd_cf["c_base.(Intercept)"]

c_base_crops <- list()
for (c in crops) {
  cf_name <- paste0("c_base.crop", c)
  if (cf_name %in% names(yd_cf)) {
    c_base_crops[[c]] <- unname(c_base_int + yd_cf[cf_name])
  } else {
    c_base_crops[[c]] <- unname(c_base_int)
  }
}

yield_coefs <- list(
  E_base = unname(yd_cf["E_base"]),
  beta_invb = unname(yd_cf["beta_invb"]),
  beta_pH = unname(yd_cf["beta_pH"]),
  beta_K = unname(yd_cf["beta_K"]),
  beta_Mg = unname(yd_cf["beta_Mg"]),
  beta_N = unname(yd_cf["beta_N"]),
  beta_Temp = unname(yd_cf["beta_Temp"]),
  beta_Prec = unname(yd_cf["beta_Prec"]),
  c_base_crops = c_base_crops
)

# Preserve existing crop_norms and scales
existing <- read_json("presentation/calculator_coefs.json")

existing$uptake_coefs <- uptake_coefs
existing$yield_coefs <- yield_coefs

existing$scales$inv_b_agro <- list(
    mean = mean(D_Long_Agro$inv_b_agro, na.rm=TRUE),
    sd = sd(D_Long_Agro$inv_b_agro, na.rm=TRUE)
)
existing$scales$inv_b <- list(
    mean = mean(D_Yield$inv_b, na.rm=TRUE),
    sd = sd(D_Yield$inv_b, na.rm=TRUE)
)

write_json(existing, "presentation/calculator_coefs.json", auto_unbox = TRUE, pretty = TRUE)
print("Updated calculator_coefs.json successfully.")
