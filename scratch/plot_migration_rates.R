library(tidyverse)
library(ggplot2)
library(broom)
library(patchwork)

# 1. Load Data
artifacts <- readRDS("data/Final_Models_Data.rds")
D <- artifacts$data$D_Yield
clay_map <- c("ALT" = 22, "CAD" = 8, "ELL" = 33, "GRA" = 17, "OEN" = 37, "REC" = 39, "REH" = 39)
D$soil_clay <- clay_map[as.character(D$site)]
D$site <- ifelse(as.character(D$site) == "REH", "REC", as.character(D$site))

# 2. GRUD Matrices (from Javascript)
grud_co2_matrix <- matrix(c(
  1.5, 1.5, 1.5, 1.4, 1.2, 1.4, 1.4, 1.3, 1.2, 1.1, 1.2, 1.2, 1.1, 1.0, 1.0,
  1.0, 1.0, 1.0, 1.0, 0.8, 1.0, 1.0, 1.0, 0.8, 0.6, 1.0, 1.0, 0.8, 0.6, 0.0,
  1.0, 0.8, 0.6, 0.0, 0.0, 0.8, 0.8, 0.4, 0.0, 0.0, 0.8, 0.6, 0.0, 0.0, 0.0,
  0.6, 0.4, 0.0, 0.0, 0.0, 0.6, 0.4, 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0.0, 0.0,
  0.4, 0.0, 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0
), nrow=16, byrow=TRUE)

grud_aae_matrix <- matrix(c(
  1.5, 1.5, 1.5, 1.4, 1.4, 1.5, 1.5, 1.4, 1.4, 1.2, 1.5, 1.4, 1.4, 1.2, 1.2,
  1.4, 1.4, 1.2, 1.2, 1.0, 1.4, 1.2, 1.2, 1.0, 1.0, 1.2, 1.2, 1.2, 1.0, 1.0,
  1.2, 1.0, 1.0, 1.0, 1.0, 1.2, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8, 0.8, 1.0, 1.0, 0.8, 0.8, 0.6,
  1.0, 1.0, 0.8, 0.8, 0.6, 1.0, 0.8, 0.8, 0.6, 0.6, 0.8, 0.8, 0.8, 0.6, 0.6,
  0.8, 0.8, 0.6, 0.6, 0.4, 0.8, 0.6, 0.6, 0.4, 0.4, 0.6, 0.6, 0.6, 0.4, 0.4,
  0.6, 0.6, 0.4, 0.4, 0.0, 0.6, 0.4, 0.4, 0.0, 0.0, 0.4, 0.4, 0.4, 0.0, 0.0,
  0.4, 0.4, 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
), nrow=26, byrow=TRUE)

get_grud_class_num <- function(type, p_val, clay) {
  if (is.na(p_val) || is.na(clay)) return(NA)
  clay_idx <- min(5, max(1, floor(clay / 10) + 1))
  if (type == "CO2") {
    thresholds <- c(0.15, 0.46, 0.77, 1.08, 1.39, 1.70, 2.01, 2.32, 2.63, 2.94, 3.25, 3.56, 3.88, 4.19, 4.50)
    row_idx <- which(p_val <= thresholds)[1]
    if (is.na(row_idx)) row_idx <- 16
    fac <- grud_co2_matrix[row_idx, clay_idx]
  } else {
    thresholds <- seq(4.9, 124.9, by=5)
    row_idx <- which(p_val <= thresholds)[1]
    if (is.na(row_idx)) row_idx <- 26
    fac <- grud_aae_matrix[row_idx, clay_idx]
  }
  if (fac >= 1.5) return(1)
  if (fac >= 1.2) return(2)
  if (fac >= 1.0) return(3)
  if (fac >= 0.4) return(4)
  return(5)
}

# 3. Calculate Classes
aae_col <- if ("soil_0_20_P_AAE10" %in% names(D)) "soil_0_20_P_AAE10" else "rollMean_soil_0_20_P_AAE10"
p_treats <- c("P0", "P33", "P66", "P100", "P133", "P166")

D_res <- D %>%
  filter(treatment_ID %in% p_treats) %>%
  filter(!is.na(soil_0_20_P_CO2) & !is.na(!!sym(aae_col))) %>%
  rowwise() %>%
  mutate(
    Num_CO2 = get_grud_class_num("CO2", soil_0_20_P_CO2, soil_clay),
    Num_AAE = get_grud_class_num("AAE", !!sym(aae_col), soil_clay),
    Bias = Num_CO2 - Num_AAE
  ) %>%
  ungroup() %>%
  mutate(
    year_num = as.numeric(as.character(year_sampling)),
    Treatment = factor(treatment_ID, levels=p_treats)
  )

# 4. Calculate Slopes (Migration Rate = classes / decade)
calc_slope <- function(df, y_var) {
  if(nrow(df) < 3) return(NA)
  fit <- lm(df[[y_var]] ~ df$year_num)
  return(coef(fit)[2] * 10) # multiply by 10 for per decade
}

slopes <- D_res %>%
  group_by(site, Treatment) %>%
  summarise(
    Rate_CO2 = calc_slope(cur_data(), "Num_CO2"),
    Rate_AAE = calc_slope(cur_data(), "Num_AAE"),
    Rate_Bias = calc_slope(cur_data(), "Bias"),
    .groups="drop"
  )

slopes_long <- slopes %>%
  pivot_longer(cols=starts_with("Rate_"), names_to="Metric", values_to="Rate") %>%
  mutate(Metric = factor(Metric, levels=c("Rate_CO2", "Rate_AAE", "Rate_Bias"), 
                         labels=c("CO2 Migration", "AAE10 Migration", "Bias Divergence Velocity")))

# 5. Plotting
p <- ggplot(slopes_long, aes(x = Treatment, y = Rate, fill = Metric)) +
  geom_hline(yintercept = 0, linetype="dashed", color="black", linewidth=0.8) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7, color="black", alpha=0.8) +
  facet_wrap(~site, nrow=2) +
  scale_fill_manual(values=c("CO2 Migration"="#D55E00", "AAE10 Migration"="#0072B2", "Bias Divergence Velocity"="#009E73")) +
  theme_minimal(base_size=14) +
  labs(title="GRUD Class Migration Rates (1991 - 2022)",
       subtitle="Velocity of degradation/enrichment in Supply Classes per Decade",
       y="Δ Class / Decade", x="P-Treatment") +
  theme(legend.position="bottom", panel.grid.minor=element_blank(),
        axis.text.x = element_text(angle=45, hjust=1))

ggsave("scratch/grud_migration_rates.png", plot=p, width=12, height=8, dpi=300)
print("Saved Migration Rate Plot")
