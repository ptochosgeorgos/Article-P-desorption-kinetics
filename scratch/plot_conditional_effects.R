library(ggplot2)
library(dplyr)

res <- readRDS("data/cluster_results.rds")

# Extract the conditional effects data frames
d_null <- res$yield$plot_data$null
d_heur <- res$yield$plot_data$heur
d_mech <- res$yield$plot_data$mech

# Add model labels
if(!is.null(d_null)) d_null$Model <- "Null Model"
if(!is.null(d_heur)) d_heur$Model <- "Heuristic CO2"
if(!is.null(d_mech)) d_mech$Model <- "Mechanistic"

# Combine the datasets we have so far
d_plot <- bind_rows(d_null, d_heur, d_mech)

# The effect1__ column contains the P value, but its name depends on the substrate.
# Since null, heur, mech use P_CO2, let's just use effect1__ for x and estimate__ for y
p <- ggplot(d_plot, aes(x = effect1__, y = estimate__, color = Model, fill = Model)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  facet_wrap(~crop, scales = "free") +
  theme_bw(base_size = 14) +
  labs(
    title = "Conditional Effects of Substrate on Yield",
    subtitle = "Predicted marginal response across crops",
    x = bquote("Substrate " ~ P[CO[2]] ~ "(mg / kg)"),
    y = "Predicted Yield (dt/ha)"
  ) +
  scale_color_manual(values = c("Null Model" = "gray50", "Heuristic CO2" = "#E69F00", "Mechanistic" = "#0072B2")) +
  scale_fill_manual(values = c("Null Model" = "gray50", "Heuristic CO2" = "#E69F00", "Mechanistic" = "#0072B2")) +
  theme(legend.position = "bottom")

ggsave("scratch/conditional_effects_curves.png", p, width = 10, height = 7, dpi=300)
cat("Plot saved to scratch/conditional_effects_curves.png\n")
