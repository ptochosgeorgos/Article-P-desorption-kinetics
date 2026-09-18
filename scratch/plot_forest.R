library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Load cluster results payload
res <- readRDS("data/cluster_results.rds")

extract_params <- function(model_name, param_df, type="Yield") {
  if (is.null(param_df)) return(NULL)
  
  df <- as.data.frame(param_df)
  df$Parameter <- rownames(df)
  df$Model <- model_name
  df$Type <- type
  
  # Filter only beta parameters
  df <- df %>% filter(str_detect(Parameter, "beta"))
  
  # Clean parameter names
  df$Covariate <- str_replace(df$Parameter, "beta(.*)_Intercept", "\\1")
  
  # Rename columns for ease of use
  # Some brms versions use Q2.5 and Q97.5, some use l-95% CI and u-95% CI
  if ("l-95% CI" %in% colnames(df)) {
    df <- df %>% rename(Lower = `l-95% CI`, Upper = `u-95% CI`)
  } else if ("Q2.5" %in% colnames(df)) {
    df <- df %>% rename(Lower = Q2.5, Upper = Q97.5)
  }
  
  return(df %>% select(Model, Type, Covariate, Estimate, Lower, Upper))
}

# Collect all parameters
d_list <- list()

if(!is.null(res$yield$parameters$heur)) d_list[[1]] <- extract_params("Heuristic CO2", res$yield$parameters$heur, "Yield")
if(!is.null(res$yield$parameters$heur_aae)) d_list[[2]] <- extract_params("Heuristic AAE10", res$yield$parameters$heur_aae, "Yield")
if(!is.null(res$yield$parameters$mech)) d_list[[3]] <- extract_params("Mechanistic", res$yield$parameters$mech, "Yield")

if(!is.null(res$uptake$parameters$heur)) d_list[[4]] <- extract_params("Heuristic CO2", res$uptake$parameters$heur, "Uptake")
if(!is.null(res$uptake$parameters$heur_aae)) d_list[[5]] <- extract_params("Heuristic AAE10", res$uptake$parameters$heur_aae, "Uptake")
if(!is.null(res$uptake$parameters$mech)) d_list[[6]] <- extract_params("Mechanistic", res$uptake$parameters$mech, "Uptake")

d_plot <- bind_rows(d_list)

# Map covariates to nicer names
cov_names <- c(
  "pH" = "pH", "Clay" = "Clay", "Corg" = "Corg", "Ca" = "Ca", 
  "Temp" = "Temperature", "Prec" = "Precipitation",
  "invb" = "Buffer Capacity (1/b)", "k" = "Desorption Rate (k)", "N" = "N-Fertilizer"
)
d_plot$Covariate_Clean <- cov_names[d_plot$Covariate]

# Create a grouping variable so models with shared covariates are in the same subplot
d_plot$Subplot <- ifelse(str_detect(d_plot$Model, "Heuristic"), "Empirical Substrates & Pedoclimatic Covariates", "Kinetic Substrate & Constraints")

p <- ggplot(d_plot, aes(x = Estimate, y = Covariate_Clean, color = Model, shape = Type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size=1) +
  geom_pointrange(aes(xmin = Lower, xmax = Upper), position = position_dodge(width = 0.5), size = 1) +
  facet_grid(Subplot ~ ., scales = "free_y", space = "free_y") +
  theme_bw() +
  labs(
    title = "Forest Plot of K_base Modulator Effects",
    subtitle = "Posterior Means and 95% Credible Intervals",
    x = "Effect Size (Estimate)",
    y = "Covariate",
    color = "Model",
    shape = "Target"
  ) +
  scale_color_manual(values = c("Heuristic CO2" = "#E69F00", "Heuristic AAE10" = "#D55E00", "Mechanistic" = "#0072B2")) +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0),
    strip.background = element_rect(fill = "gray95"),
    panel.grid.major.y = element_blank()
  )

ggsave("scratch/forest_plot_covariates.png", p, width = 10, height = 6, dpi=300)
cat("Plot saved to scratch/forest_plot_covariates.png\n")
