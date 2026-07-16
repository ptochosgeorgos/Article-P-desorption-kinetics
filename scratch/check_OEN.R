final_artifacts <- readRDS("data/Final_Models_Data.rds")
D_Long <- final_artifacts$data$D_Long_Agro
D_OEN <- subset(D_Long, site == "OEN")
cat("Summary of AAE10 for OEN:\n")
summary(D_OEN$soil_0_20_P_AAE10)
cat("Summary of CO2 for OEN:\n")
summary(D_OEN$soil_0_20_P_CO2)
