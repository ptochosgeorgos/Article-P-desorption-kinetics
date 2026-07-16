final_artifacts <- readRDS("data/Final_Models_Data.rds")
D_ready <- final_artifacts$data$D_Long_Agro
cat("P_CO2 quantiles:\n")
print(quantile(D_ready$soil_0_20_P_CO2, na.rm=TRUE))
cat("P_AAE10 quantiles:\n")
print(quantile(D_ready$soil_0_20_P_AAE10, na.rm=TRUE))
