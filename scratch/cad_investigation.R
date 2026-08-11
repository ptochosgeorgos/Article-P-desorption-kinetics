library(dplyr)
library(ggplot2)
library(nlme)

cat("Loading artifacts...\n")
artifacts <- readRDS("data/Final_Models_Data.rds")

D_Yield <- artifacts$data$D_Yield
D_Long <- artifacts$data$D_Long_Agro
m_uptake <- artifacts$models$uptake_raw_co2
m_yield <- artifacts$models$yield_raw_co2

d_cad_ka <- D_Long %>% filter(site == "CAD", crop == "KA")
d_cad_ka_y <- D_Yield %>% filter(site == "CAD", crop == "KA")

cat("\n--- CADENAZZO POTATOES (KA) INVESTIGATION ---\n")

# Identify P balance column
bal_col <- grep("balance", names(d_cad_ka), ignore.case=TRUE, value=TRUE)
cat("P Balance columns found:", paste(bal_col, collapse=", "), "\n")
if(length(bal_col) > 0) {
    p_bal_col <- bal_col[1]
} else {
    p_bal_col <- NULL
}

d_cad_ka$pred_up_fixed <- predict(m_uptake, newdata = d_cad_ka, level = 0)
cor_year_up <- cor(d_cad_ka$year, d_cad_ka$Relative_Uptake, use="complete.obs")
cat(sprintf("Correlation Uptake ~ Year: %.3f\n", cor_year_up))

if(!is.null(p_bal_col)) {
    cor_pbal_up <- cor(d_cad_ka[[p_bal_col]], d_cad_ka$Relative_Uptake, use="complete.obs")
    cat(sprintf("Correlation Uptake ~ P_bal: %.3f\n", cor_pbal_up))
}

cat("\nUptake by Treatment:\n")
d_cad_ka %>% group_by(treatment) %>% summarize(
    Mean_Uptake = mean(Relative_Uptake, na.rm=TRUE),
    Mean_Pred_Up = mean(pred_up_fixed, na.rm=TRUE),
    Mean_P_CO2 = mean(soil_0_20_P_CO2, na.rm=TRUE)
) %>% print()

d_cad_ka_y$pred_y_fixed <- predict(m_yield, newdata = d_cad_ka_y, level = 0)
cor_year_y <- cor(d_cad_ka_y$year, d_cad_ka_y$Relative_Yield, use="complete.obs")
cat(sprintf("\nCorrelation Yield ~ Year: %.3f\n", cor_year_y))

cat("\nYield by Treatment:\n")
d_cad_ka_y %>% group_by(treatment) %>% summarize(
    Mean_Yield = mean(Relative_Yield, na.rm=TRUE),
    Mean_Pred_Y = mean(pred_y_fixed, na.rm=TRUE),
    Mean_P_CO2 = mean(soil_0_20_P_CO2, na.rm=TRUE)
) %>% print()

cat("\nLinear Model of Observed Uptake ~ Treatment + Year:\n")
lm_up <- lm(Relative_Uptake ~ treatment + year, data=d_cad_ka)
print(summary(lm_up))

cat("\nLinear Model of Observed Yield ~ Treatment + Year:\n")
lm_y <- lm(Relative_Yield ~ treatment + year, data=d_cad_ka_y)
print(summary(lm_y))

# Let's also check absolute values
cat("\nAbsolute Uptake (kg/ha) summary:\n")
print(summary(d_cad_ka$annual_P_uptake))

cat("\nAbsolute Yield (dt/ha) summary:\n")
print(summary(d_cad_ka_y$total_yield))

