library(ggplot2)
library(dplyr)
library(tibble)

# Load cluster results payload
res <- readRDS("data/cluster_results.rds")

# Extract the LOO comparison object
comp <- res$yield$comparison

# Convert to data frame
comp_df <- as.data.frame(comp)
comp_df$Model <- rownames(comp_df)

# Rename models for nicer labels
name_map <- c(
  "mod_mech_Y" = "Mechanistic",
  "mod_heur_Y" = "Heuristic CO2",
  "mod_heur_Y_aae" = "Heuristic AAE10",
  "mod_base_Y" = "Base CO2",
  "mod_base_Y_aae" = "Base AAE10",
  "mod_null_Y" = "Null Model"
)

comp_df$Model_Clean <- name_map[comp_df$Model]
# If some models are missing (like the AAE10 ones before cluster run), just use what we have
comp_df$Model_Clean <- ifelse(is.na(comp_df$Model_Clean), comp_df$Model, comp_df$Model_Clean)

# The loo_compare function sets the best model to elpd_diff = 0
# We plot the ELPD difference (with standard errors) relative to the best model
p <- ggplot(comp_df, aes(x = reorder(Model_Clean, elpd_diff), y = elpd_diff)) +
  geom_point(size = 4, color = "#0072B2") +
  geom_errorbar(aes(ymin = elpd_diff - 2*se_diff, ymax = elpd_diff + 2*se_diff), width = 0.2, color = "#0072B2", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(
    title = "Out-of-Sample Predictive Performance (LOO)",
    subtitle = "Relative to the best model (higher is better). Error bars are ± 2 SE.",
    x = "Model",
    y = "Difference in Expected Log Predictive Density (ELPD)"
  )

ggsave("scratch/looic_comparison.png", p, width = 8, height = 5, dpi=300)
cat("Plot saved to scratch/looic_comparison.png\n")
