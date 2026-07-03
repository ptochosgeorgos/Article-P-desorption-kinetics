lines <- readLines("notebooks/qi_modelling1.R")
insert_idx <- grep("m_yield_nlme <- nlme\\(", lines)
lines_to_run <- lines[1:(insert_idx - 1)]
writeLines(lines_to_run, "scratch_temp.R")

source("scratch_temp.R")
library(dplyr)
library(nlme)

test_model_matrix <- function(filter_rare = FALSE) {
    D_Test <- D_Yield
    if (filter_rare) {
        D_Test <- D_Test |>
            group_by(crop) |>
            filter(n() > 200) |>
            ungroup() |>
            mutate(crop = droplevels(crop))
    }
        
    cat("n_crops:", length(levels(D_Test$crop)), "\n")
    
    # Check rank of fixed effects
    form <- ~ crop + z_inv_b + z_pH + z_ln_K + z_ln_Mg + z_fert_N + z_Temp_Mean + z_Prec_Anom
    X <- model.matrix(form, data = D_Test)
    qr_X <- qr(X)
    cat("Matrix columns:", ncol(X), "\n")
    cat("Matrix rank:", qr_X$rank, "\n")
    if (qr_X$rank < ncol(X)) {
        cat("RANK DEFICIENT! Collinear columns:\n")
        pivot <- qr_X$pivot
        cat(paste(colnames(X)[pivot[(qr_X$rank + 1):ncol(X)]], collapse = ", "), "\n")
    } else {
        cat("Full rank!\n")
    }
}

cat("--- WITHOUT FILTER ---\n")
test_model_matrix(FALSE)

cat("\n--- WITH FILTER ---\n")
test_model_matrix(TRUE)
