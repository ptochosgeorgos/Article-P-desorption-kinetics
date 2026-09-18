library(ggplot2)
library(dplyr)
library(latex2exp)

res <- readRDS("data/cluster_results.rds")

# Helper function to extract covariates
extract_covariates <- function(df, model_name, response_name) {
  # Looking for parameters that start with k_ (which are the modifiers)
  # or b_k_ something
  # Let's print rownames just in case
  rn <- rownames(df)
  # In brms, fixed effects are usually b_k_... or just b_...
  cov_idx <- grep("^b_k_(pH|clay|Corg|Ca|temp_anom|prec_anom|b|k|N_total)", rn)
  if(length(cov_idx) == 0) cov_idx <- grep("^b_(pH|clay|Corg|Ca|temp_anom|prec_anom|b|k|N_total)", rn)
  if(length(cov_idx) == 0) cov_idx <- grep("^(pH|clay|Corg|Ca|temp_anom|prec_anom|b|k|N_total)", rn)
  
  if(length(cov_idx) > 0) {
    sub_df <- df[cov_idx, ]
    sub_df$Parameter <- rn[cov_idx]
    # Clean up parameter names
    sub_df$Parameter <- gsub("^b_k_", "", sub_df$Parameter)
    sub_df$Parameter <- gsub("^b_", "", sub_df$Parameter)
    
    sub_df$Model <- model_name
    sub_df$Response <- response_name
    return(sub_df)
  }
  return(NULL)
}

df_yu_m <- extract_covariates(res$yield$parameters$mech, "Mechanistic", "Yield")
df_yu_h <- extract_covariates(res$yield$parameters$heur, "Heuristic", "Yield")
df_up_m <- extract_covariates(res$uptake$parameters$mech, "Mechanistic", "Uptake")
df_up_h <- extract_covariates(res$uptake$parameters$heur, "Heuristic", "Uptake")

d_plot <- bind_rows(df_yu_m, df_yu_h, df_up_m, df_up_h)

# Check if d_plot is empty
if(nrow(d_plot) == 0) {
  cat("Could not find covariates in the parameters dataframe. Rownames were:\n")
  print(rownames(res$yield$parameters$heur))
  q(status=1)
}

# Rename for nice plotting
d_plot$Parameter[d_plot$Parameter == "temp_anom"] <- "Temp"
d_plot$Parameter[d_plot$Parameter == "prec_anom"] <- "Prec"
d_plot$Parameter[d_plot$Parameter == "Corg"] <- "C_org"
d_plot$Parameter[d_plot$Parameter == "N_total"] <- "N"

# Create plot
p <- ggplot(d_plot, aes(x = Estimate, y = Parameter, color = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(xmin = `l-95% CI`, xmax = `u-95% CI`), 
                position = position_dodge(width = 0.5), width = 0.2, linewidth = 1) +
  facet_wrap(~Response, scales = "free_x") +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("Heuristic" = "#E69F00", "Mechanistic" = "#0072B2")) +
  labs(x = "Effect Size on Base Availability (95% CI)", y = "Covariate Modifier") +
  theme(legend.position = "bottom")

ggsave("scratch/forest_plot_covariates.png", p, width = 8, height = 5, dpi = 300)
cat("Plot saved to scratch/forest_plot_covariates.png\n")
