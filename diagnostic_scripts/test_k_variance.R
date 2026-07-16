library(dplyr)
library(lme4)
library(ggplot2)

D <- readxl::read_excel("data/STYCS_data_2023_260511.xlsx")
kin <- D |> filter(!is.na(k)) |> select(site, treatment_ID, rep, k)

# ANOVA
aov_model <- aov(k ~ site + treatment_ID, data = kin)
print(summary(aov_model))

# Site means and variance
kin_summary <- kin |> group_by(site) |> summarise(mean_k = mean(k), sd_k = sd(k), cv_k = sd(k)/mean(k))
print(kin_summary)

