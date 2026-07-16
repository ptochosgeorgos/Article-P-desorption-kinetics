library(nlme)
library(jsonlite)
library(dplyr)

# 1. Load Data
final_obj <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_obj$data$D_Long_Agro
mod_uptake <- final_obj$models$uptake_raw_co2

# 2. Extract Uptake Model Fixed Effects
coefs <- fixef(mod_uptake)

fixed_list <- as.list(coefs)

# 3. Extract Scaling Factors
vars_to_scale <- c(
  "Temp_Anom", "fert_N", "inv_b_geo", "v0"
)

scales <- list()
for (v in vars_to_scale) {
  if (v %in% colnames(D_Long_Agro)) {
    scales[[v]] <- list(
      mean = mean(D_Long_Agro[[v]], na.rm=TRUE),
      sd = sd(D_Long_Agro[[v]], na.rm=TRUE)
    )
  }
}



# 4. GRUD Crop Norms (N and P)
crop_norms <- list(
  "WW" = list(name = "Winterweizen (Brot)", N = 140, P = 27),
  "SW" = list(name = "Sommerweizen", N = 120, P = 23),
  "WG" = list(name = "Wintergerste", N = 110, P = 28),
  "KM" = list(name = "Körnermais", N = 110, P = 46),
  "SM" = list(name = "Silomais", N = 110, P = 46),
  "KA" = list(name = "Kartoffeln", N = 120, P = 36),
  "ZR" = list(name = "Zuckerrüben", N = 100, P = 40),
  "RA" = list(name = "Winterraps", N = 150, P = 28)
)

output <- list(
  uptake_coefs = fixed_list,
  scales = scales,
  crop_norms = crop_norms
)

write_json(output, "presentation/calculator_coefs.json", auto_unbox = TRUE, pretty = TRUE)
cat("Successfully extracted calculator coefficients and saved to presentation/calculator_coefs.json\n")
