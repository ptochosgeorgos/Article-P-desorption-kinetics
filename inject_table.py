import re

with open('notebooks/qi_modelling1.qmd', 'r') as f:
    content = f.read()

# 1. Fix start values for thm and aae
content = re.sub(
    r'(m_yield_thm_co2 <- nlme\(.*?start = ).*?(,\n\s*control = nlmeControl)', 
    r'\1fixef(m_yield_raw_co2)\2', 
    content, flags=re.DOTALL
)

aae_start_code = "c(0.1, unname(fixef(m_yield_raw_co2)[2:length(fixef(m_yield_raw_co2))]))"
content = re.sub(
    r'(m_yield_raw_aae <- nlme\(.*?start = ).*?(,\n\s*control = nlmeControl)', 
    r'\1' + aae_start_code + r'\2', 
    content, flags=re.DOTALL
)

# 2. Append the summary table chunk
new_chunk = """
### Model Performance Summary

```{r yield-model-summary}
library(kableExtra)

rmse_table <- data.frame(
    Model = c("Raw P_CO2", "Thermo a_CO2", "Legacy P_AAE10"),
    Pseudo_R2 = c(
        cor(D_Yield$Relative_Yield, predict(m_yield_raw_co2, level = 2))^2,
        cor(D_Yield$Relative_Yield, predict(m_yield_thm_co2, level = 2))^2,
        cor(D_Yield$Relative_Yield, predict(m_yield_raw_aae, level = 2))^2
    ),
    RMSE_Conditional = c(
        sqrt(mean(residuals(m_yield_raw_co2, level = 2)^2)),
        sqrt(mean(residuals(m_yield_thm_co2, level = 2)^2)),
        sqrt(mean(residuals(m_yield_raw_aae, level = 2)^2))
    ),
    RMSE_Marginal = c(
        sqrt(mean(residuals(m_yield_raw_co2, level = 0)^2)),
        sqrt(mean(residuals(m_yield_thm_co2, level = 0)^2)),
        sqrt(mean(residuals(m_yield_raw_aae, level = 0)^2))
    )
)

rmse_table |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 3))) |>
    kbl(caption = "Performance Metrics of One-Step Mitscherlich NLME Models") |>
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
```
"""

if '```{r yield-model-summary}' not in content:
    content = content + "\n" + new_chunk

with open('notebooks/qi_modelling1.qmd', 'w') as f:
    f.write(content)

print("Injected summary table and fixed starts!")
