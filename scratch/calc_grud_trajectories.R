library(tidyverse)
library(ggplot2)
library(patchwork)

# 1. Load Data (Switching to D_Yield to get all P treatments)
artifacts <- readRDS("data/Final_Models_Data.rds")
D <- artifacts$data$D_Yield

# Site clay percentages
clay_map <- c("ALT" = 22, "CAD" = 8, "ELL" = 33, "GRA" = 17, "OEN" = 37, "REC" = 39, "REH" = 39)
D$soil_clay <- clay_map[as.character(D$site)]
D$site <- ifelse(as.character(D$site) == "REH", "REC", as.character(D$site))

# 2. GRUD Matrices (from Javascript)
grud_co2_matrix <- matrix(c(
  1.5, 1.5, 1.5, 1.4, 1.2,
  1.4, 1.4, 1.3, 1.2, 1.1,
  1.2, 1.2, 1.1, 1.0, 1.0,
  1.0, 1.0, 1.0, 1.0, 0.8,
  1.0, 1.0, 1.0, 0.8, 0.6,
  1.0, 1.0, 0.8, 0.6, 0.0,
  1.0, 0.8, 0.6, 0.0, 0.0,
  0.8, 0.8, 0.4, 0.0, 0.0,
  0.8, 0.6, 0.0, 0.0, 0.0,
  0.6, 0.4, 0.0, 0.0, 0.0,
  0.6, 0.4, 0.0, 0.0, 0.0,
  0.4, 0.0, 0.0, 0.0, 0.0,
  0.4, 0.0, 0.0, 0.0, 0.0,
  0.4, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0
), nrow=16, byrow=TRUE)

grud_aae_matrix <- matrix(c(
  1.5, 1.5, 1.5, 1.4, 1.4,
  1.5, 1.5, 1.4, 1.4, 1.2,
  1.5, 1.4, 1.4, 1.2, 1.2,
  1.4, 1.4, 1.2, 1.2, 1.0,
  1.4, 1.2, 1.2, 1.0, 1.0,
  1.2, 1.2, 1.2, 1.0, 1.0,
  1.2, 1.0, 1.0, 1.0, 1.0,
  1.2, 1.0, 1.0, 1.0, 1.0,
  1.0, 1.0, 1.0, 1.0, 1.0,
  1.0, 1.0, 1.0, 1.0, 1.0,
  1.0, 1.0, 1.0, 0.8, 0.8,
  1.0, 1.0, 0.8, 0.8, 0.6,
  1.0, 1.0, 0.8, 0.8, 0.6,
  1.0, 0.8, 0.8, 0.6, 0.6,
  0.8, 0.8, 0.8, 0.6, 0.6,
  0.8, 0.8, 0.6, 0.6, 0.4,
  0.8, 0.6, 0.6, 0.4, 0.4,
  0.6, 0.6, 0.6, 0.4, 0.4,
  0.6, 0.6, 0.4, 0.4, 0.0,
  0.6, 0.4, 0.4, 0.0, 0.0,
  0.4, 0.4, 0.4, 0.0, 0.0,
  0.4, 0.4, 0.0, 0.0, 0.0,
  0.4, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0
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
  
  # A=1, B=2, C=3, D=4, E=5 (higher is more saturated)
  if (fac >= 1.5) return(1)
  if (fac >= 1.2) return(2)
  if (fac >= 1.0) return(3)
  if (fac >= 0.4) return(4)
  return(5)
}

# 3. Calculate Classes
aae_col <- if ("soil_0_20_P_AAE10" %in% names(D)) "soil_0_20_P_AAE10" else "rollMean_soil_0_20_P_AAE10"

# Filter only P-treatments (P0 to P166)
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
    year_num = as.numeric(as.character(year_sampling)), # D_Yield uses year_sampling
    Treatment = factor(treatment_ID, levels=p_treats)
  )

# 4. Plotting Panels
common_theme <- theme_minimal(base_size=12) +
  theme(legend.position="none", panel.grid.minor=element_blank())

# Panel 1: CO2
p1 <- ggplot(D_res, aes(x = year_num, y = Num_CO2, color = Treatment, group = Treatment)) +
  geom_point(alpha=0.4, size=1.5, position=position_jitter(width=0.5, height=0.1)) +
  geom_smooth(method="loess", se=FALSE, linewidth=1, alpha=0.8) +
  facet_wrap(~site, nrow=1) +
  scale_y_continuous(breaks=1:5, labels=c("A", "B", "C", "D", "E"), limits=c(0.5, 5.5)) +
  scale_color_viridis_d(option="plasma", end=0.9) +
  labs(title="P-CO2 Empirical Supply Classes", y="Supply Class", x=NULL) +
  common_theme

# Panel 2: AAE10
p2 <- ggplot(D_res, aes(x = year_num, y = Num_AAE, color = Treatment, group = Treatment)) +
  geom_point(alpha=0.4, size=1.5, position=position_jitter(width=0.5, height=0.1)) +
  geom_smooth(method="loess", se=FALSE, linewidth=1, alpha=0.8) +
  facet_wrap(~site, nrow=1) +
  scale_y_continuous(breaks=1:5, labels=c("A", "B", "C", "D", "E"), limits=c(0.5, 5.5)) +
  scale_color_viridis_d(option="plasma", end=0.9) +
  labs(title="P-AAE10 Empirical Supply Classes", y="Supply Class", x=NULL) +
  common_theme

# Panel 3: Bias
p3 <- ggplot(D_res, aes(x = year_num, y = Bias, color = Treatment, group = Treatment)) +
  geom_hline(yintercept=0, linetype="dashed", color="black", linewidth=1) +
  geom_point(alpha=0.4, size=1.5, position=position_jitter(width=0.5, height=0.1)) +
  geom_smooth(method="loess", se=FALSE, linewidth=1, alpha=0.8) +
  facet_wrap(~site, nrow=1) +
  scale_y_continuous(breaks=c(-2, -1, 0, 1, 2), limits=c(-2.5, 2.5)) +
  scale_color_viridis_d(option="plasma", end=0.9) +
  labs(title="Bias Map (CO2 Class - AAE10 Class)", y="Class Deviation", x="Year") +
  theme_minimal(base_size=12) +
  theme(legend.position="bottom", panel.grid.minor=element_blank()) +
  guides(color=guide_legend(nrow=1))

# Stitch together
final_plot <- p1 / p2 / p3 + plot_layout(heights = c(1, 1, 1))

ggsave("scratch/grud_trajectory.png", plot=final_plot, width=14, height=10, dpi=300)
print("3-Panel Plot saved to scratch/grud_trajectory.png")
