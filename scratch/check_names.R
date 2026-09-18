library(tidyverse)
library(readxl)
D2 <- read_excel("data/STYCS_data_2023_260511.xlsx")
print(grep("pH|K|Mg", colnames(D2), value=TRUE))
