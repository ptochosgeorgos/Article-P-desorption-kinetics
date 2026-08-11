library(dplyr)
library(ggplot2)
library(nlme)
library(lme4)

final_artifacts <- readRDS("data/Final_Models_Data.rds")
D_Long <- final_artifacts$data$D_Long_Agro
D_Cum <- final_artifacts$data$D_Cum

# Let's refit the AAE10 PTF on the restricted dataset (pH <= 7.3)
D_ptf <- D_Long %>% 
    filter(soil_0_20_pH_H2O <= 7.3) %>%
    drop_na(rollMean_soil_0_20_clay, rollMean_soil_0_20_silt, soil_0_20_P_AAE10, soil_0_20_P_CO2)

# Calculate residuals of the PTF
# Simple approximation: ln(P_AAE) ~ ln(P_CO2) * Texture
lm_ptf <- lm(log(soil_0_20_P_AAE10) ~ log(soil_0_20_P_CO2) * log(rollMean_soil_0_20_clay + rollMean_soil_0_20_silt) + soil_0_20_pH_H2O, data=D_ptf)

D_ptf$resid_ptf <- residuals(lm_ptf)
D_ptf$fine_tex <- D_ptf$rollMean_soil_0_20_clay + D_ptf$rollMean_soil_0_20_silt

p1 <- ggplot(D_ptf, aes(x = fine_tex, y = resid_ptf, color=soil_0_20_pH_H2O)) +
    geom_point(alpha=0.5) + geom_smooth() +
    theme_minimal() + labs(title="PTF Residuals vs Clay+Silt (pH <= 7.3)")

ggsave("scratch/resid_vs_texture.png", p1, width=6, height=4)

# Let's check the cumulative P balance model to see if Delta Q is trustworthy
# Cumulated_P_Balance is the actual physical fertilizer added minus uptake over 30 years
m_bal_co2 <- lmer(Cumulated_P_Balance ~ log(mean_P_CO2) * mean_inv_b + mean_pH + mean_Temp + mean_Tex + (1 | site), data = D_Cum)

D_Cum$resid_bal <- residuals(m_bal_co2)

p2 <- ggplot(D_Cum, aes(x = mean_Tex, y = resid_bal, color=mean_pH)) +
    geom_point(alpha=0.5) + geom_smooth() +
    theme_minimal() + labs(title="P Balance Residuals vs Clay+Silt")

ggsave("scratch/resid_bal_vs_texture.png", p2, width=6, height=4)

# Print out prediction intervals or variance
cat("Variance of PTF residuals:\n")
D_ptf %>% mutate(tex_class = cut(fine_tex, breaks=c(0, 20, 40, 60, 100))) %>%
    group_by(tex_class) %>%
    summarize(MSE = mean(resid_ptf^2), Bias = mean(resid_ptf), N=n()) %>%
    print()

cat("\nVariance of P Balance residuals:\n")
D_Cum %>% mutate(tex_class = cut(mean_Tex, breaks=c(0, 20, 40, 60, 100))) %>%
    group_by(tex_class) %>%
    summarize(MSE = mean(resid_bal^2), Bias = mean(resid_bal), N=n()) %>%
    print()
