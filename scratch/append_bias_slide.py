with open("presentation_grud/index.qmd", "a") as f:
    f.write("""

---

## The Illusion of Bijection: GRUD Correction Factors { .smaller }

```{r}
#| echo: false
#| message: false
#| warning: false
library(tidyverse)

# Load data and define D_eval
final_artifacts <- readRDS("../data/Final_Models_Data.rds")
D_Long_Agro <- final_artifacts$data$D_Long_Agro

get_clay_bin <- function(clay) {
  if (is.na(clay)) return(NA)
  if (clay < 10) return(1)
  if (clay < 20) return(2)
  if (clay < 30) return(3)
  if (clay < 40) return(4)
  return(5)
}
get_co2_bin <- function(p) {
  if (is.na(p)) return(NA)
  breaks <- seq(0.310, 4.650, by=0.310)
  idx <- findInterval(p, breaks) + 1
  return(min(idx, 16))
}
get_aae_bin <- function(p) {
  if (is.na(p)) return(NA)
  breaks <- seq(5.0, 125.0, by=5.0)
  idx <- findInterval(p, breaks) + 1
  return(min(idx, 16))
}

D_eval <- D_Long_Agro |> filter(!is.na(soil_0_20_P_CO2) & !is.na(soil_0_20_P_AAE10) & !is.na(rollMean_soil_0_20_clay) & !is.na(rollMean_soil_0_20_pH_H2O))
D_eval <- D_eval |> rowwise() |> mutate(
  clay_bin = get_clay_bin(rollMean_soil_0_20_clay),
  co2_bin = get_co2_bin(soil_0_20_P_CO2),
  aae_bin = get_aae_bin(soil_0_20_P_AAE10),
  v_co2 = z_co2[co2_bin, clay_bin],
  v_aae10 = z_aae10[aae_bin, clay_bin],
  bias = v_co2 - v_aae10
) |> ungroup()

library(plotly)
p <- ggplot(D_eval, aes(x = v_co2, y = v_aae10)) +
    geom_jitter(alpha = 0.05, width=0.03, height=0.03, color="#1f77b4") +
    geom_smooth(method = "lm", color = "red", linewidth = 1.2) +
    labs(
        title = "Bijection Test: GRUD Correction Factors",
        subtitle = "v_AAE10 vs v_CO2",
        x = "Correction Factor v (CO2)",
        y = "Correction Factor v (AAE10)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
ggplotly(p)
```

---

## Interactive Bias Surface (Clay & pH) { .scrollable .smaller }

Here is a 3D visualization of the systematic bias ($\\Delta v = v_{CO2} - v_{AAE10}$) modeled as a smooth surface (GAM) across the entire dataset. You can interact with the surface to see exactly how much the fertilizer recommendations diverge depending on **Clay** and **pH**!

```{r}
#| echo: false
#| message: false
#| warning: false
#| fig-width: 10
#| fig-height: 8
library(plotly)
library(mgcv)

# Fit a GAM to smooth the bias surface dynamically (now using pH instead of Corg)
fit <- gam(bias ~ s(rollMean_soil_0_20_clay, bs="cs") + s(rollMean_soil_0_20_pH_H2O, bs="cs"), data=D_eval)

clay_seq <- seq(min(D_eval$rollMean_soil_0_20_clay), max(D_eval$rollMean_soil_0_20_clay), length.out=50)
ph_seq <- seq(min(D_eval$rollMean_soil_0_20_pH_H2O), max(D_eval$rollMean_soil_0_20_pH_H2O), length.out=50)
grid <- expand.grid(rollMean_soil_0_20_clay = clay_seq, rollMean_soil_0_20_pH_H2O = ph_seq)
grid$pred_bias <- predict(fit, grid)

bias_matrix <- matrix(grid$pred_bias, nrow=50, ncol=50)

# Render Plotly Surface directly
plot_ly(x = clay_seq, y = ph_seq, z = bias_matrix, type = "surface", 
        colorscale = "RdBu", cmin = -1.5, cmax = 1.5) %>%
  layout(title = "Systematic Bias: v_CO2 - v_AAE10",
         scene = list(
           xaxis = list(title = "Clay %"),
           yaxis = list(title = "pH (H2O)"),
           zaxis = list(title = "Bias (CO2 - AAE10)")
         ))
```
""")
