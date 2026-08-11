library(jsonlite)

coefs <- fromJSON("presentation/calculator_coefs.json")
ptf <- fromJSON("presentation/ptf_coefs.json")

# Inputs
crop <- "WW"
I_now <- 0.5
pH <- 6.5
Ca <- 5000
Mg <- 250
K_val <- 115
Corg <- 2.0

calc_delta_Q <- function(clay_silt) {
    # Scale factors
    z_tex <- (log(clay_silt) - ptf$scales$FineTexture$mean) / ptf$scales$FineTexture$sd
    z_pH <- (pH - ptf$scales$pH$mean) / ptf$scales$pH$sd
    z_Ca <- (log(Ca) - ptf$scales$Ca$mean) / ptf$scales$Ca$sd
    z_Mg <- (log(Mg) - ptf$scales$Mg$mean) / ptf$scales$Mg$sd
    z_K <- (log(K_val) - ptf$scales$K$mean) / ptf$scales$K$sd
    z_Corg <- (log(Corg) - ptf$scales$Corg$mean) / ptf$scales$Corg$sd
    
    # PTF
    c <- ptf$coefficients
    ln_K_ptf <- c[["(Intercept)"]] + c[["z_ln_FineTexture"]]*z_tex + c[["z_pH"]]*z_pH + 
              c[["z_ln_Ca"]]*z_Ca + c[["z_ln_Mg"]]*z_Mg + c[["z_ln_K"]]*z_K + c[["z_ln_Corg"]]*z_Corg
    
    n <- c[["ln_P_CO2"]] + c[["ln_P_CO2:z_ln_FineTexture"]]*z_tex + c[["ln_P_CO2:z_pH"]]*z_pH + 
         c[["ln_P_CO2:z_ln_Ca"]]*z_Ca + c[["ln_P_CO2:z_ln_Mg"]]*z_Mg + c[["ln_P_CO2:z_ln_K"]]*z_K + 
         c[["ln_P_CO2:z_ln_Corg"]]*z_Corg
         
    K_sorp <- exp(ln_K_ptf)
    
    b_1 <- n * K_sorp * (1.0^(n-1))
    inv_b <- 1.0 / b_1
    
    # Z-scores for inv_b
    z_inv_b_agro = (inv_b - coefs$scales$inv_b_agro$mean) / coefs$scales$inv_b_agro$sd
    z_inv_b = (inv_b - coefs$scales$inv_b$mean) / coefs$scales$inv_b$sd
    
    # Clamp
    z_inv_b_agro <- max(-3, min(4, z_inv_b_agro))
    z_inv_b <- max(-3, min(4, z_inv_b))
    
    # Uptake model
    K_base_crop <- coefs$uptake_coefs$K_base_crops[[crop]]
    Km <- K_base_crop * exp(coefs$uptake_coefs$beta_invb * z_inv_b_agro)
    P_crit_up <- (0.80 * Km) / (1.0 - 0.80)
    
    # Yield model
    c_base_crop <- coefs$yield_coefs$c_base_crops[[crop]]
    z_pH_yd <- (pH - 6.45) / 0.8 # approx sd
    z_K_yd <- (log(K_val) - 3.5) / 0.7 # approx sd
    z_Mg_yd <- (log(Mg) - 4.5) / 0.6 # approx sd
    
    c_eff <- c_base_crop * exp(coefs$yield_coefs$beta_invb * z_inv_b + 
                               coefs$yield_coefs$beta_pH * z_pH_yd + 
                               coefs$yield_coefs$beta_K * z_K_yd + 
                               coefs$yield_coefs$beta_Mg * z_Mg_yd)
    
    P_crit_yd <- max(0, (-log(1 - 0.95) / c_eff) - coefs$yield_coefs$E_base)
    
    Q_now <- K_sorp * (I_now^n)
    Q_target_up <- K_sorp * (P_crit_up^n)
    Q_target_yd <- K_sorp * (P_crit_yd^n)
    
    delta_Q_up <- Q_target_up - Q_now
    delta_Q_yd <- Q_target_yd - Q_now
    
    return(c(Clay_Silt = clay_silt, K = K_sorp, n = n, inv_b = inv_b, 
             P_crit_up = P_crit_up, dQ_up = delta_Q_up, 
             P_crit_yd = P_crit_yd, dQ_yd = delta_Q_yd))
}

res <- do.call(rbind, lapply(seq(10, 80, by=10), calc_delta_Q))
print(round(res, 3))
