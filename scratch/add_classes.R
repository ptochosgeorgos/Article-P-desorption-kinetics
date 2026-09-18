library(dplyr)

grud_co2_matrix <- matrix(c(
  1.5, 1.5, 1.5, 1.4, 1.2, 1.4, 1.4, 1.3, 1.2, 1.1, 1.2, 1.2, 1.1, 1.0, 1.0,
  1.0, 1.0, 1.0, 1.0, 0.8, 1.0, 1.0, 1.0, 0.8, 0.6, 1.0, 1.0, 0.8, 0.6, 0.0,
  1.0, 0.8, 0.6, 0.0, 0.0, 0.8, 0.8, 0.4, 0.0, 0.0, 0.8, 0.6, 0.0, 0.0, 0.0,
  0.6, 0.4, 0.0, 0.0, 0.0, 0.6, 0.4, 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0.0, 0.0,
  0.4, 0.0, 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0), nrow=16, byrow=TRUE)

grud_aae_matrix <- matrix(c(
  1.5, 1.5, 1.5, 1.4, 1.4, 1.5, 1.5, 1.4, 1.4, 1.2, 1.5, 1.4, 1.4, 1.2, 1.2,
  1.4, 1.4, 1.2, 1.2, 1.0, 1.4, 1.2, 1.2, 1.0, 1.0, 1.2, 1.2, 1.2, 1.0, 1.0,
  1.2, 1.0, 1.0, 1.0, 1.0, 1.2, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8, 0.8, 1.0, 1.0, 0.8, 0.8, 0.6,
  1.0, 1.0, 0.8, 0.8, 0.6, 1.0, 0.8, 0.8, 0.6, 0.6, 0.8, 0.8, 0.8, 0.6, 0.6,
  0.8, 0.8, 0.6, 0.6, 0.4, 0.8, 0.6, 0.6, 0.4, 0.4, 0.6, 0.6, 0.6, 0.4, 0.4,
  0.6, 0.6, 0.4, 0.4, 0.0, 0.6, 0.4, 0.4, 0.0, 0.0, 0.4, 0.4, 0.4, 0.0, 0.0,
  0.4, 0.4, 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), nrow=26, byrow=TRUE)

get_grud_class <- function(type, p_val, clay) {
  if (is.na(p_val) || is.na(clay)) return(NA)
  clay_idx <- min(5, max(1, floor(clay / 10) + 1))
  if (type == "CO2") {
    row_idx <- which(p_val <= c(0.15, 0.46, 0.77, 1.08, 1.39, 1.70, 2.01, 2.32, 2.63, 2.94, 3.25, 3.56, 3.88, 4.19, 4.50))[1]
    if (is.na(row_idx)) row_idx <- 16
    fac <- grud_co2_matrix[row_idx, clay_idx]
  } else {
    row_idx <- which(p_val <= seq(4.9, 124.9, by=5))[1]
    if (is.na(row_idx)) row_idx <- 26
    fac <- grud_aae_matrix[row_idx, clay_idx]
  }
  if (fac >= 1.5) return("A")
  if (fac >= 1.2) return("B")
  if (fac >= 1.0) return("C")
  if (fac >= 0.4) return("D")
  return("E")
}

clay_map <- c("ALT" = 22, "CAD" = 8, "ELL" = 33, "GRA" = 17, "OEN" = 37, "REC" = 39, "REH" = 39)

add_classes <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      temp_clay = ifelse(!is.null(df$soil_0_20_clay) && !is.na(soil_0_20_clay), soil_0_20_clay, clay_map[as.character(Site)]),
      Class_CO2 = factor(get_grud_class("CO2", soil_0_20_P_CO2, temp_clay), levels=c("A","B","C","D","E")),
      Class_AAE = factor(get_grud_class("AAE", soil_0_20_P_AAE10, temp_clay), levels=c("A","B","C","D","E"))
    ) %>%
    select(-temp_clay) %>%
    ungroup()
}

# Update models/D_Yield.rds
d_yield <- readRDS("models/D_Yield.rds")
d_yield <- add_classes(d_yield)
saveRDS(d_yield, "models/D_Yield.rds")
cat("Updated models/D_Yield.rds\n")

# Update data/D_Yield_clean.rds
d_clean <- readRDS("data/D_Yield_clean.rds")
d_clean <- add_classes(d_clean)
saveRDS(d_clean, "data/D_Yield_clean.rds")
cat("Updated data/D_Yield_clean.rds\n")

# Update data/RES.rds (which contains D and data)
if (file.exists("data/RES.rds")) {
  res <- readRDS("data/RES.rds")
  if (!is.null(res$D)) res$D <- add_classes(res$D)
  if (!is.null(res$data)) res$data <- add_classes(res$data)
  saveRDS(res, "data/RES.rds")
  cat("Updated data/RES.rds\n")
}

# Update models/D_ready.rds
if (file.exists("models/D_ready.rds")) {
  d_ready <- readRDS("models/D_ready.rds")
  d_ready <- add_classes(d_ready)
  saveRDS(d_ready, "models/D_ready.rds")
  cat("Updated models/D_ready.rds\n")
}
