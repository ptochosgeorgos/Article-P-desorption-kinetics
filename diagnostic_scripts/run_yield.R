library(dplyr)
library(lme4)
library(nlme)
library(ggplot2)

source("R/02_kinetics.R")

load("data/STYCS_P_Clean.RData")
# replicate the data pipeline for yield quickly...
# wait, actually the R workspace isn't saved, but the html is saved.
