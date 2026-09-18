library(nlme)
library(dplyr)
library(tidyr)

load("gapon_aparK.RData")

modelling_data <- full_predictions %>%
  filter(
    year >= 1990,
    !is.na(soil_0_20_K_AAE10), soil_0_20_K_AAE10 > 0,
    !is.na(AR_K_final), AR_K_final > 0,
    !is.na(soil_0_20_Ca_AAE10), soil_0_20_Ca_AAE10 > 0,
    !is.na(soil_0_20_Mg_AAE10), soil_0_20_Mg_AAE10 > 0,
    !is.na(soil_0_20_clay), !is.na(rollMean_soil_0_20_pH_H2O), !is.na(rollMean_soil_0_20_Corg),
    soil_0_20_clay > 0, rollMean_soil_0_20_Corg > 0,
    !is.na(site), !is.na(plot_nr), !is.na(year)
  ) %>%
  filter(!is.na(CEC_rest), CEC_rest > 0) %>%
  drop_na(AR_K_final, z_clay, z_pH, z_Corg, z_CEC_rest, site, plot_nr, year)

modelling_data <- modelling_data %>%
  mutate(
    K_buffer_capacity = KG_pred * (Q_Ca + Q_Mg),
    z_b = as.numeric(scale(log(K_buffer_capacity))),
    z_AR_K = as.numeric(scale(log(AR_K_final)))
  ) %>%
  filter(!is.na(K_buffer_capacity), K_buffer_capacity > 0)

yield_data <- modelling_data %>%
  filter(!is.na(crop), !is.na(annual_yield_mp_DM), annual_yield_mp_DM > 0, !is.na(z_AR_K), !is.na(z_b), !is.na(site), !is.na(year)) %>%
  mutate(crop = droplevels(factor(crop)))

k_model <- function(z_AR_K, z_b, A, Y_0, c_I, c_b) {
  Y_0 + (A - Y_0) * (1 - exp(-(c_I * z_AR_K + c_b * z_b)))
}

num_crops <- length(levels(yield_data$crop))
base_A <- quantile(yield_data$annual_yield_mp_DM, 0.90, na.rm=TRUE)
base_Y0 <- quantile(yield_data$annual_yield_mp_DM, 0.10, na.rm=TRUE)

start_A <- rep(base_A, num_crops)
start_Y0 <- rep(base_Y0, num_crops)
start_c_I <- rep(0.1, num_crops)
start_c_b <- rep(0.1, num_crops)
my_starts <- c(start_A, start_Y0, start_c_I, start_c_b)

trace("model.matrix.default", tracer = quote({
  print(object)
  print(str(data))
}), print = FALSE)

tryCatch({
m_nlme_yield <- nlme(
  annual_yield_mp_DM ~ k_model(z_AR_K, z_b, A, Y_0, c_I, c_b),
  data = yield_data,
  fixed = list(A ~ 0 + crop, Y_0 ~ 0 + crop, c_I ~ 0 + crop, c_b ~ 0 + crop),
  random = Y_0 ~ 1 | site/year,
  start = my_starts,
  weights = varPower(form = ~fitted(.)),
  na.action = na.omit,
  control = nlmeControl(maxIter = 200, pnlsMaxIter = 100)
)
}, error = function(e) { print(e) })
