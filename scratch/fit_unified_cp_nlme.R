library(nlme)
library(dplyr)
library(jsonlite)


options(warn=-1)

cat("Loading data...\n")
final_obj <- readRDS("data/Final_Models_Data.rds")
D_Long_Agro <- final_obj$data$D_Long_Agro

D_CP <- D_Long_Agro |>
    mutate(
        C_P = annual_P_uptake / annual_yield_mp_DM,
        crop_factor = as.factor(crop)
    ) |>
    filter(is.finite(C_P), C_P > 0, a_CO2_total_mg_L > 0) |>
    filter(!is.na(z_pH), !is.na(z_ln_FineTexture), !is.na(z_inv_b_agro), !is.na(z_v0))

n_crops <- length(levels(D_CP$crop_factor))
cat("Fitting full unified model for", n_crops, "crops...\n")

start_vec <- c(
    rep(1.5, n_crops), # C_base
    rep(0.05, n_crops), # S_base
    0, # beta_pH
    0, # beta_tex
    2.0, # A_dil
    8.0, # k_dil
    0, # beta_invb
    0  # beta_v0
)

mod_unified <- try(nlme(
    C_P ~ C_base + beta_pH * z_pH + beta_tex * z_ln_FineTexture + 
          A_dil * exp(-k_dil * a_CO2_total_mg_L) + 
          (S_base * exp(beta_invb * z_inv_b_agro + beta_v0 * z_v0)) * a_CO2_total_mg_L,
    data = D_CP,
    fixed = list(
        C_base ~ crop_factor - 1,
        S_base ~ crop_factor - 1,
        beta_pH + beta_tex + A_dil + k_dil + beta_invb + beta_v0 ~ 1
    ),
    random = C_base ~ 1 | site,
    start = start_vec,
    control = nlmeControl(maxIter = 1500, pnlsMaxIter = 200, msMaxIter = 400, returnObject = TRUE, opt="nlminb")
))

if(!inherits(mod_unified, "try-error")) {
    print(summary(mod_unified))
    
    cat("\n--- ANOVA ---\n")
    print(anova(mod_unified))
    
    pred <- predict(mod_unified, level = 0) # Population prediction
    actual <- D_CP$C_P
    
    rmse_val <- sqrt(mean((actual - pred)^2, na.rm=TRUE))
    mae_val <- mean(abs(actual - pred), na.rm=TRUE)
    r2_val <- cor(actual, pred, use = "complete.obs")^2
    
    cat(sprintf("\n--- Performance Metrics ---\n"))
    cat(sprintf("RMSE: %.4f\n", rmse_val))
    cat(sprintf("MAE:  %.4f\n", mae_val))
    cat(sprintf("R^2 (Population Level): %.4f\n", r2_val))
    
} else {
    cat("Model failed to converge.\n")
}
