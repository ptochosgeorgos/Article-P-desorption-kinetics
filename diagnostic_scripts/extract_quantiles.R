lines <- readLines("diagnostic_scripts/temp_qi.R")[1:453]
source(textConnection(lines))

library(dplyr)
library(jsonlite)

# We want K and n from the Agronomic PTF
# K = exp(ln_K_pred_agro), n = n_pred_agro
D_quantiles <- D_Long %>%
  filter(is.finite(inv_b_agro)) %>%
  group_by(site) %>%
  summarise(
    K_25 = quantile(exp(ln_K_pred_agro), 0.25, na.rm=TRUE),
    K_50 = quantile(exp(ln_K_pred_agro), 0.50, na.rm=TRUE),
    K_75 = quantile(exp(ln_K_pred_agro), 0.75, na.rm=TRUE),
    n_25 = quantile(n_pred_agro, 0.25, na.rm=TRUE),
    n_50 = quantile(n_pred_agro, 0.50, na.rm=TRUE),
    n_75 = quantile(n_pred_agro, 0.75, na.rm=TRUE),
    clay_mean = mean(rollMean_soil_0_20_clay, na.rm=TRUE)
  ) %>%
  arrange(clay_mean)

print(D_quantiles)

# Save to JSON for the presentation
write_json(D_quantiles, "presentation/site_quantiles.json", pretty = TRUE)
print("Saved to presentation/site_quantiles.json")
