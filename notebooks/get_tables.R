library(dplyr)
library(tidyr)
library(readxl)
library(lme4)
library(performance)

# 1. Load legacy kinetics and climate data
RES <- readRDS("../data/RES.rds")
D_legacy <- RES$D
climate_data <- readRDS("../data/all_P.rds") |>
    dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec) |>
    dplyr::distinct()

site_geochemistry <- D_legacy |>
    group_by(site) |>
    summarise(feox_mean = mean(Feox, na.rm = TRUE), alox_mean = mean(Alox, na.rm = TRUE)) |>
    ungroup()

kinetics_stable <- D_legacy |>
    dplyr::select(site, treatment_ID, rep, k, v0_kPS = kPS, Pmax_PS = PS) |>
    distinct(site, treatment_ID, rep, .keep_all = TRUE)

# 2. Load accurate raw STYCS dataset and construct D_main
D_main <- read_excel("../data/STYCS_data_2023_260511.xlsx", guess_max = 20000) |>
    rename(rep = replicate) |>
    mutate(site = gsub("STYCS_", "", LtE_name)) |>
    left_join(climate_data, by = c("site", "year")) |>
    mutate(
        soil_0_20_P_CO2 = soil_0_20_P_test * 0.155,
        crop = crop_abr,
        fert_P_tot = fert_P2O5_tot / 2.291,
        annual_P_uptake = rowSums(across(starts_with("P_harv")), na.rm = TRUE),
        annual_yield_mp_DM = rowSums(across(matches("^harv.*mp_yield_DM$")), na.rm = TRUE),
        annual_yield_bp_DM = rowSums(across(matches("^harv.*bp[1-2]_yield_DM$")), na.rm = TRUE),
        annual_P_balance = fert_P_tot - annual_P_uptake
    ) |>
    filter(year >= 1990) |>
    left_join(site_geochemistry, by = "site") |>
    left_join(kinetics_stable, by = c("site", "treatment_ID", "rep")) |>
    filter(!is.na(Pmax_PS)) |>
    mutate(Treatment = factor(treatment_ID, levels = c("P0", "P100", "P166")))

cova_ann <- c("ansum_prec", "anavg_temp", "soil_0_20_pH_H2O", "soil_0_20_Corg", "soil_0_20_clay", "soil_0_20_presample_lime")
inds_ann <- c("soil_0_20_P_CO2", "soil_0_20_P_AAE10", "Pmax_PS", "v0_kPS")

run_perms <- function(target, inds, covs, dat, nested = FALSE) {
    d_clean <- dat |> drop_na(all_of(c(target, "site", "year", covs, inds)))
    d_clean$year_f <- as.factor(d_clean$year)
    combos <- combn(covs, 2, simplify = FALSE)
    results <- list()
    for(ind in inds) {
        if (!nested) {
            f_base <- as.formula(paste0("scale(", target, ") ~ scale(`", ind, "`) + (1 | site)"))
        } else {
            f_base <- as.formula(paste0("scale(", target, ") ~ scale(`", ind, "`) + (1 | site) + (1 | site:year_f)"))
        }
        mod_base <- lmer(f_base, data = d_clean, control = lmerControl(calc.derivs = FALSE))
        base_vc <- as.data.frame(VarCorr(mod_base))
        base_site_var <- base_vc$vcov[base_vc$grp == "site"]
        
        for(cb in combos) {
            v1 <- cb[1]; v2 <- cb[2]
            if (!nested) {
                f_mod <- as.formula(paste0("scale(", target, ") ~ scale(`", ind, "`) + scale(`", v1, "`) + scale(`", v2, "`) + (1 | site)"))
            } else {
                f_mod <- as.formula(paste0("scale(", target, ") ~ scale(`", ind, "`) + scale(`", v1, "`) + scale(`", v2, "`) + (1 | site) + (1 | site:year_f)"))
            }
            mod <- suppressWarnings(tryCatch({
                lmer(f_mod, data = d_clean, control = lmerControl(calc.derivs = FALSE))
            }, error = function(e) NULL))
            
            if(is.null(mod)) next
            
            vc <- as.data.frame(VarCorr(mod))
            site_var <- vc$vcov[vc$grp == "site"]
            
            r2m <- suppressWarnings(tryCatch(as.numeric(performance::r2(mod)$R2_marginal), error=function(e) NA))
            
            results[[length(results)+1]] <- data.frame(
                Soil_Indicator = ind,
                Pedo_Var_1 = v1,
                Pedo_Var_2 = v2,
                R2_Marginal = r2m,
                AIC = AIC(mod),
                Site_Var_Reduction_Pct = (base_site_var - site_var) / base_site_var * 100
            )
        }
    }
    
    bind_rows(results) |> 
        mutate(Stability = ifelse(Site_Var_Reduction_Pct < 0, "Unstable (Collinear)", "Stable")) |>
        filter(Stability == "Stable") |>
        mutate(
            R2_Marginal = round(R2_Marginal, 3),
            Site_Var_Reduction_Pct = round(Site_Var_Reduction_Pct, 1),
            AIC = round(AIC, 1)
        ) |>
        arrange(desc(R2_Marginal)) |>
        select(-Stability)
}

cat("--- DEFAULT ANNUAL ---\n")
res_ann <- run_perms("annual_P_balance", inds_ann, cova_ann, D_main, nested=FALSE)
print(head(res_ann, 15))

cat("\n--- NESTED ANNUAL (1 | site) + (1 | site:year_f) ---\n")
res_ann_nested <- run_perms("annual_P_balance", inds_ann, cova_ann, D_main, nested=TRUE)
print(head(res_ann_nested, 15))
