with open("presentation_grud/index.qmd", "a") as f:
    f.write("""

---

## The Breakdown of Covariance: Calcareous Soils { .scrollable .smaller }

**The Observable Phenomenon**
We do not measure "True P". We measure the covariance between Intensity ($P_{CO2}$) and Quantity ($P_{AAE10}$). In non-calcareous soils (pH $\\le$ 7.3), these two extractions co-vary predictably. However, when pH exceeds 7.3, this relationship violently breaks down: $P_{CO2}$ continues to extract a wide variance of phosphorus, while $P_{AAE10}$ collapses to near-zero.

**The Chemical Mechanism**
This collapse is not a deviation from some unmeasurable truth; it is a physical failure of the AAE10 method itself. AAE10 uses an ammonium acetate-EDTA buffer calibrated to pH 4.65. In highly calcareous soils, the massive presence of free $CaCO_3$ instantly neutralizes the acid buffer, and the flood of $Ca^{2+}$ ions physically saturates the EDTA chelator. The affine mathematical model forced by the GRUD completely ignores this chemical boundary.

```{r}
#| echo: false
#| message: false
#| warning: false
#| fig-align: center
#| fig-width: 10
#| fig-height: 6

D_plot <- D_Long_Agro |> 
  filter(!is.na(soil_0_20_P_CO2) & !is.na(soil_0_20_P_AAE10) & !is.na(rollMean_soil_0_20_pH_H2O)) |>
  mutate(is_high_ph = ifelse(rollMean_soil_0_20_pH_H2O > 7.3, "pH > 7.3 (Calcareous)", "pH <= 7.3"))

ggplot(D_plot, aes(x = soil_0_20_P_CO2, y = soil_0_20_P_AAE10, color = rollMean_soil_0_20_pH_H2O)) +
  geom_point(alpha = 0.6, size=2) +
  scale_color_viridis_c(option = "magma", name = "Soil pH") +
  geom_smooth(method="gam", color="blue", fill="blue", alpha=0.2) +
  facet_wrap(~is_high_ph, scales="free_y") +
  labs(
    title = "The Breakdown of Covariance in Calcareous Soils",
    subtitle = "Notice how the relationship collapses violently above pH 7.3",
    x = "Phosphorus Intensity (CO2 Extraction, mg/L)",
    y = "Phosphorus Quantity (AAE10 Extraction, mg/kg)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )
```
""")
