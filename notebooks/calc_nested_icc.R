library(lme4)
library(dplyr)
library(readxl)
library(tidyr)

climate_data <- readRDS("../data/all_P.rds") |>
    dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec) |>
    dplyr::distinct()

D_main <- read_excel("../data/STYCS_data_2023_260511.xlsx", guess_max = 20000) |>
    rename(rep = replicate) |>
    mutate(site = gsub("STYCS_", "", LtE_name)) |>
    left_join(climate_data, by = c("site", "year")) |>
    filter(year >= 1990)

num_cols <- names(D_main)[sapply(D_main, is.numeric)]
covariates <- setdiff(num_cols, c("year", "rep", "soil_0_20_P_test", "plot_nr"))

get_nested_icc <- function(var_name, data) {
    res <- tryCatch({
        d_clean <- data |> filter(!is.na(.data[[var_name]]))
        d_clean$year_f <- as.factor(d_clean$year)
        fmla <- as.formula(paste0("scale(`", var_name, "`) ~ 1 + (1 | site/year_f)"))
        mod <- lmer(fmla, data = d_clean, control = lmerControl(calc.derivs = FALSE))
        
        vc <- as.data.frame(VarCorr(mod))
        var_site <- vc$vcov[vc$grp == "site"]
        var_site_year <- vc$vcov[vc$grp == "year_f:site"]
        var_resid <- vc$vcov[vc$grp == "Residual"]
        
        total_var <- var_site + var_site_year + var_resid
        icc_site <- var_site / total_var
        icc_year <- var_site_year / total_var
        
        return(c(icc_site, icc_year))
    }, error = function(e) return(c(NA, NA)))
    return(res)
}

results <- lapply(covariates, function(v) {
    iccs <- get_nested_icc(v, D_main)
    data.frame(Variable = v, ICC_Site = iccs[1], ICC_Site_Year = iccs[2])
})
icc_df <- bind_rows(results) |> filter(!is.na(ICC_Site)) 

cat("--- Top Spatial (Site) Variables ---\n")
print(icc_df |> arrange(desc(ICC_Site)) |> head(15))

cat("\n--- Top Temporal (Site:Year) Variables ---\n")
print(icc_df |> arrange(desc(ICC_Site_Year)) |> head(15))
