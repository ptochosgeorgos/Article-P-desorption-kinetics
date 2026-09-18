suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readxl))
D2 <- read_excel("data/STYCS_data_2023_260511.xlsx")
cat("rollMean_soil_0_20_Ca_AAE10 not NA:", sum(!is.na(D2$rollMean_soil_0_20_Ca_AAE10)), "\n")
