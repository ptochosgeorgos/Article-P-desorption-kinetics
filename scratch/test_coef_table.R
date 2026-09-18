suppressPackageStartupMessages({
  library(nlme)
  library(lmerTest)
  library(MuMIn)
})

m_yield <- readRDS("models/m_yield_thm_co2.rds")
m_ptf <- readRDS("models/ptf_agro_thm.rds")

create_coef_table <- function(models) {
  extract_coef_info <- function(model) {
    if (inherits(model, "nlme")) {
      coef_matrix <- summary(model)$tTable
    } else {
      coef_matrix <- summary(model)$coefficients
    }
    estimates <- coef_matrix[, 1]
    p_values <- coef_matrix[, ncol(coef_matrix)]
    formatted_coef <- sapply(seq_along(estimates), function(i) {
      est_str <- sprintf("%.3f", estimates[i])
      stars <- if (p_values[i] < 0.001) "***" else
               if (p_values[i] < 0.01) "** " else
               if (p_values[i] < 0.05) "* " else  ""
      paste0(est_str,stars)
    })
    names(formatted_coef) <- rownames(coef_matrix)
    return(formatted_coef)
  }
  
  lapply(models, extract_coef_info)
}

print(create_coef_table(list(Yield_THM = m_yield, PTF_THM = m_ptf)))
