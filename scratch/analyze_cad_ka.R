library(dplyr)
library(ggplot2)
library(nlme)

cat("Loading artifacts...\n")
artifacts <- readRDS("data/Final_Models_Data.rds")
print(names(artifacts$models))

D_Yield <- artifacts$data$D_Yield
D_Long <- artifacts$data$D_Long_Agro
m_uptake <- artifacts$models$uptake_raw_co2
m_yield <- artifacts$models$yield_raw_co2

d_cad_ka <- D_Long %>% filter(site == "CAD", crop == "KA")
d_cad_ka_y <- D_Yield %>% filter(site == "CAD", crop == "KA")

cat("\n--- CADENAZZO POTATOES (KA) ---\n")
cat("Rows in Uptake data:", nrow(d_cad_ka), "\n")
cat("Rows in Yield data:", nrow(d_cad_ka_y), "\n")

if(nrow(d_cad_ka) > 0) {
  # Level 0 = fixed effects only, Level 1 = site, Level 2 = site/plot
  d_cad_ka$pred_up_fixed <- predict(m_uptake, newdata = d_cad_ka, level = 0)
  
  cat("\nRelative Uptake (Observed):\n")
  print(summary(d_cad_ka$Relative_Uptake))
  cat("\nRelative Uptake (Predicted - Fixed Effects):\n")
  print(summary(d_cad_ka$pred_up_fixed))
  
  p1 <- ggplot(d_cad_ka, aes(x = year, y = Relative_Uptake, color = treatment)) +
    geom_point(size=3) + 
    geom_line(aes(y = pred_up_fixed, group=treatment), linetype="dashed") +
    theme_minimal() + labs(title="Cadenazzo Potatoes: Uptake vs Year")
  ggsave("scratch/cad_ka_uptake_year.png", p1, width=8, height=6)
}

if(nrow(d_cad_ka_y) > 0) {
  d_cad_ka_y$pred_y_fixed <- predict(m_yield, newdata = d_cad_ka_y, level = 0)
  
  cat("\nRelative Yield (Observed):\n")
  print(summary(d_cad_ka_y$Relative_Yield))
  cat("\nRelative Yield (Predicted - Fixed Effects):\n")
  print(summary(d_cad_ka_y$pred_y_fixed))
  
  p2 <- ggplot(d_cad_ka_y, aes(x = year, y = Relative_Yield, color = treatment)) +
    geom_point(size=3) + 
    geom_line(aes(y = pred_y_fixed, group=treatment), linetype="dashed") +
    theme_minimal() + labs(title="Cadenazzo Potatoes: Yield vs Year")
  ggsave("scratch/cad_ka_yield_year.png", p2, width=8, height=6)
}

# General check on sandy soils
d_sand <- D_Long %>% filter(sand > 50, crop == "KA")
cat("\n--- POTATOES ON ALL SANDY SOILS (>50% sand) ---\n")
cat("Rows:", nrow(d_sand), "\n")
if(nrow(d_sand) > 0) {
    d_sand$pred_up_fixed <- predict(m_uptake, newdata = d_sand, level = 0)
    cat("\nRelative Uptake (Predicted):\n")
    print(summary(d_sand$pred_up_fixed))
}
