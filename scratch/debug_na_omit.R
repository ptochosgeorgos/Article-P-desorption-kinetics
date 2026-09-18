source("scratch/purl_trunc.R")
cat("\nNROW D_Long_Agro:", nrow(D_Long_Agro), "\n")
cat("NROW na.omit(D_Long_Agro):", nrow(na.omit(D_Long_Agro)), "\n")
cat("Number of crop levels in na.omit(D_Long_Agro):", length(levels(droplevels(na.omit(D_Long_Agro)$crop))), "\n")
