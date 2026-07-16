import re

slides = """
## Annex (Methodological Details) { .center }

## 1. Kinetic Desorption Model ($v_0$) { .scrollable .smaller }

```{r annex-kinetic}
#| echo: false
#| message: false
#| warning: false
library(kableExtra)
library(ggplot2)
library(MuMIn)

summary_models <- readRDS("data/Summary_Models.rds")
m_kin <- summary_models$k_ptf

# Coefficients
cf <- summary(m_kin)$coefficients
kable(cf, digits = 4, caption = "Kinetic Model Fixed Effects") |>
  kable_styling(bootstrap_options = c("striped", "condensed"))

# Metrics
r2 <- summary(m_kin)$r.squared
rmse <- sqrt(mean(residuals(m_kin)^2))
metrics <- data.frame(Metric = c("R-squared", "RMSE"), Value = c(r2, rmse))
kable(metrics, digits = 3, caption = "Performance Metrics") |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width=FALSE)

# Plot
plot_data <- m_kin$model
plot_data$Predicted <- predict(m_kin)

ggplot(plot_data, aes(x = Predicted, y = `log(k)`)) +
  geom_point(alpha = 0.5, size=2, color="blue") +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="red") +
  theme_minimal() +
  labs(title="Predicted vs Observed (Kinetic Model)", x="Predicted log(k)", y="Observed log(k)")
```

## 2. Pedotransfer Function for Capacity ($K$) { .scrollable .smaller }

```{r annex-ptf}
#| echo: false
#| message: false
#| warning: false
m_ptf <- summary_models$ptf_cons_thm

# Coefficients
cf <- summary(m_ptf)$coefficients
kable(cf, digits = 4, caption = "PTF Fixed Effects") |>
  kable_styling(bootstrap_options = c("striped", "condensed"))

# Metrics
r2 <- r.squaredGLMM(m_ptf)
rmse <- sqrt(mean(residuals(m_ptf)^2))
metrics <- data.frame(Metric = c("Marginal R2", "Conditional R2", "RMSE"), Value = c(r2[1,1], r2[1,2], rmse))
kable(metrics, digits = 3, caption = "Performance Metrics") |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width=FALSE)

# Plot
# Extract data from lmer object
plot_data <- m_ptf@frame
plot_data$Predicted <- predict(m_ptf)

ggplot(plot_data, aes(x = Predicted, y = ln_P_AAE)) +
  geom_point(alpha = 0.5, size=2, color="forestgreen") +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="red") +
  theme_minimal() +
  labs(title="Predicted vs Observed (PTF Model)", x="Predicted ln(P_AAE)", y="Observed ln(P_AAE)")
```

## 3. Mechanistic Yield Model { .scrollable .smaller }

```{r annex-yield}
#| echo: false
#| message: false
#| warning: false
m_yield <- summary_models$yield

# Coefficients
cf <- summary(m_yield)$tTable
kable(cf, digits = 4, caption = "Yield Model Fixed Effects") |>
  kable_styling(bootstrap_options = c("striped", "condensed"))

# Metrics
r2 <- r.squaredGLMM(m_yield)
rmse <- sqrt(mean(residuals(m_yield)^2))
metrics <- data.frame(Metric = c("Marginal R2", "Conditional R2", "RMSE"), Value = c(r2[1,1], r2[1,2], rmse))
kable(metrics, digits = 3, caption = "Performance Metrics") |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width=FALSE)

# Plot
plot_data <- final_artifacts$data$D_Yield
# filter to rows used in model
plot_data <- plot_data[complete.cases(plot_data[,c("Relative_Yield", "soil_0_20_P_CO2", "z_inv_b_agro")]), ]
plot_data$Predicted <- predict(m_yield, level=0)

ggplot(plot_data, aes(x = Predicted, y = Relative_Yield, color=site)) +
  geom_point(alpha = 0.5, size=2) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="red") +
  theme_minimal() +
  labs(title="Predicted vs Observed (Yield Model)", x="Predicted Relative Yield", y="Observed Relative Yield")
```

## 4. Mechanistic Uptake Model { .scrollable .smaller }

```{r annex-uptake}
#| echo: false
#| message: false
#| warning: false
m_uptake <- summary_models$uptake

# Coefficients
cf <- summary(m_uptake)$tTable
kable(cf, digits = 4, caption = "Uptake Model Fixed Effects") |>
  kable_styling(bootstrap_options = c("striped", "condensed"))

# Metrics
r2 <- r.squaredGLMM(m_uptake)
rmse <- sqrt(mean(residuals(m_uptake)^2))
metrics <- data.frame(Metric = c("Marginal R2", "Conditional R2", "RMSE"), Value = c(r2[1,1], r2[1,2], rmse))
kable(metrics, digits = 3, caption = "Performance Metrics") |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width=FALSE)

# Plot
plot_data <- final_artifacts$data$D_Long_Agro
plot_data$Predicted <- predict(m_uptake, level=0)

ggplot(plot_data, aes(x = Predicted, y = Relative_Uptake, color=site)) +
  geom_point(alpha = 0.5, size=2) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="red") +
  theme_minimal() +
  labs(title="Predicted vs Observed (Uptake Model)", x="Predicted Relative Uptake", y="Observed Relative Uptake")
```
"""

with open("presentation/index.qmd", "r") as f:
    content = f.read()

annex_marker = "## Annex (Methodological Details) { .center .smaller }"
if annex_marker in content:
    content = content.replace(annex_marker, slides)

with open("presentation/index.qmd", "w") as f:
    f.write(content)
print("Added annex slides.")
