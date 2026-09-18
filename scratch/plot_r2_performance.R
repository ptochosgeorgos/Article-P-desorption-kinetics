library(ggplot2)
library(dplyr)
library(tidyr)

res <- readRDS("data/cluster_results.rds")

# Extract R2 values dynamically for whatever models are available in the payload
extract_r2 <- function(model_list, type) {
  d_list <- list()
  for (m_name in names(model_list)) {
    if (grepl("r2_", m_name) && !is.null(model_list[[m_name]])) {
      mod <- gsub("r2_", "", m_name)
      
      cond <- as.data.frame(model_list[[m_name]]$conditional)
      marg <- as.data.frame(model_list[[m_name]]$marginal)
      
      # Handle brms version differences (Q2.5 vs l-95% CI)
      lower_col <- ifelse("Q2.5" %in% colnames(cond), "Q2.5", "l-95% CI")
      upper_col <- ifelse("Q97.5" %in% colnames(cond), "Q97.5", "u-95% CI")
      
      d_list[[length(d_list) + 1]] <- data.frame(
        Target = type,
        Model = mod,
        R2_Type = "Conditional (Fixed + Random)",
        Estimate = cond$Estimate,
        Lower = cond[[lower_col]],
        Upper = cond[[upper_col]]
      )
      
      d_list[[length(d_list) + 1]] <- data.frame(
        Target = type,
        Model = mod,
        R2_Type = "Marginal (Fixed Effects Only)",
        Estimate = marg$Estimate,
        Lower = marg[[lower_col]],
        Upper = marg[[upper_col]]
      )
    }
  }
  return(bind_rows(d_list))
}

d_y <- extract_r2(res$yield, "Yield")
d_u <- extract_r2(res$uptake, "Uptake")
d_plot <- bind_rows(d_y, d_u)

# Clean up model names
name_map <- c(
  "mech" = "Mechanistic",
  "heur" = "Heuristic CO2",
  "heur_aae" = "Heuristic AAE10",
  "base" = "Base CO2",
  "base_aae" = "Base AAE10",
  "null" = "Null Model"
)
d_plot$Model <- name_map[d_plot$Model]

p <- ggplot(d_plot, aes(x = Model, y = Estimate, color = Model)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, linewidth = 1) +
  facet_grid(Target ~ R2_Type, scales = "free_y") +
  theme_bw(base_size = 14) +
  labs(
    title = "Model Performance: Marginal vs. Conditional R²",
    subtitle = "Marginal = Fixed Effects (Agronomic Signal) | Conditional = Fixed + Random (Site/Year Noise)",
    x = "Model",
    y = "Bayesian R²"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggsave("scratch/r2_comparison.png", p, width = 9, height = 7, dpi=300)
cat("Plot saved to scratch/r2_comparison.png\n")
