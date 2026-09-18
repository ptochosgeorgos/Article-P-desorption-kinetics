suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readxl))

RES <- readRDS("data/RES.rds")
D <- RES$D
cat("RES$D treatment_ID:", paste(unique(D$treatment_ID), collapse=", "), "\n")

D2 <- read_excel("data/STYCS_data_2023_260511.xlsx")
cat("STYCS treatment_ID:", paste(unique(D2$treatment_ID), collapse=", "), "\n")
