library(ggplot2)
library(dplyr)

# 1. Define a stylized Yield vs Uptake function that exhibits the Steenbjerg effect
# We use a Hill equation to get the classic sigmoidal response.
# X = Uptake (P_up)
# Y = Yield
f_Y <- function(X) {
    Y_max <- 100
    K <- 10
    n <- 2.5
    Y_max * (X^n) / (K^n + X^n)
}

# 2. Find the tangent point from the origin (max Y/X, which is min X/Y, i.e., min C_P)
# We want f'(X) = f(X)/X
# Derivative of Hill: f'(X) = Y_max * K^n * n * X^(n-1) / (K^n + X^n)^2
f_prime <- function(X) {
    Y_max <- 100
    K <- 10
    n <- 2.5
    (Y_max * (K^n) * n * (X^(n-1))) / ((K^n + X^n)^2)
}

# Find root of f'(X) - f(X)/X = 0
obj_fun <- function(X) { f_prime(X) - f_Y(X)/X }
res <- uniroot(obj_fun, lower = 1, upper = 25)
X_opt <- res$root
Y_opt <- f_Y(X_opt)
slope_opt <- f_Y(X_opt) / X_opt

# 3. Generate Data
X_seq <- seq(0, 35, length.out = 500)
df <- data.frame(
    Uptake = X_seq,
    Yield = f_Y(X_seq)
)

# Tangent line data
df_tangent <- data.frame(
    Uptake = c(0, max(X_seq)),
    Yield = c(0, slope_opt * max(X_seq))
)

# 4. Plot
p <- ggplot() +
    geom_line(data = df, aes(x = Uptake, y = Yield), color = "darkgreen", linewidth = 2) +
    geom_line(data = df_tangent, aes(x = Uptake, y = Yield), color = "red", linetype = "dashed", linewidth = 1) +
    geom_point(aes(x = X_opt, y = Y_opt), color = "red", size = 5) +
    annotate("text", x = X_opt + 1, y = Y_opt - 5, label = "Max Yield / Uptake\n= Min Tissue Concentration (C_P)\n= End of Steenbjerg Effect", hjust = 0, color = "red", fontface = "bold", size = 5) +
    annotate("text", x = 5, y = 80, label = "Luxury Consumption\n(Yield plateaus, Uptake continues)", hjust = 0, color = "black", fontface = "italic", size = 5) +
    annotate("text", x = 2, y = 10, label = "Dilution (Steenbjerg)\n(Yield outpaces Uptake)", hjust = 0, color = "black", fontface = "italic", size = 5) +
    labs(
        title = "The Geometry of Luxury Consumption",
        subtitle = "When plotting Yield vs Uptake, the minimum tissue concentration (C_P) is exactly the tangent from the origin.",
        x = expression("Phosphorus Uptake (" * P[up] * ")"),
        y = "Relative Yield (Y)"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12)
    )

ggsave("presentation/steenbjerg_tangent.png", p, width = 10, height = 7, dpi = 300)
cat("Saved to presentation/steenbjerg_tangent.png\n")
