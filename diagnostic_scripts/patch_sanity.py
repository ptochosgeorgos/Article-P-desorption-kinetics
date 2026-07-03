with open("notebooks/sanity_checks.qmd", "a") as f:
    f.write("""
## 11. Final Task Sanity Graphs: Yield and Uptake Distributions

To verify the overall distributions and responses of Yield and P Uptake across the sites, treatments, years, and specific fertilizers, we plot the requested sanity checks below.

```{r final-sanity-graphs, fig.width=12, fig.height=8, warning=FALSE}
library(ggplot2)
library(dplyr)
library(tidyr)

# Calculate cumulative P uptake
D_graphs <- D_main |>
    group_by(site, plot_nr) |>
    arrange(year) |>
    mutate(
        cumulative_P_up = cumsum(replace_na(annual_P_uptake, 0))
    ) |>
    ungroup() |>
    filter(!is.na(annual_yield_mp_DM))

# Helper to plot across the 3 metrics
plot_sanity <- function(metric_y, title_y) {
    # 1. Y ~ Treatment faceted by trial
    p1 <- ggplot(D_graphs, aes(x = Treatment, y = .data[[metric_y]], fill = Treatment)) +
        geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
        facet_wrap(~ site, scales = "free_y") +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste(title_y, "~ Treatment by Site"), x = "Treatment", y = title_y)
    
    # 2. Y ~ year faceted by crop and site
    p2 <- ggplot(D_graphs, aes(x = year, y = .data[[metric_y]], color = crop)) +
        geom_point(alpha = 0.5) +
        facet_grid(crop ~ site, scales = "free_y") +
        theme_bw() +
        theme(strip.text.y = element_text(angle = 0), legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste(title_y, "~ Year faceted by Crop and Site"), x = "Year", y = title_y)
    
    # 3. Y ~ fert_X for X in {P, K, Mg, N}
    D_fert <- D_graphs |>
        select(site, Treatment, all_of(metric_y), fert_P_tot, fert_K_tot, fert_Mg_tot, fert_N_tot) |>
        pivot_longer(cols = starts_with("fert_"), names_to = "Fertilizer", values_to = "Amount") |>
        mutate(Fertilizer = factor(Fertilizer, levels = c("fert_P_tot", "fert_K_tot", "fert_Mg_tot", "fert_N_tot"),
                                   labels = c("P Applied", "K Applied", "Mg Applied", "N Applied")))
        
    p3 <- ggplot(D_fert, aes(x = Amount, y = .data[[metric_y]], color = site)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = "lm", se = FALSE) +
        facet_wrap(~ Fertilizer, scales = "free_x") +
        theme_bw() +
        labs(title = paste(title_y, "~ Fertilizer Applied"), x = "Fertilizer Amount (kg/ha)", y = title_y)
    
    print(p1)
    print(p2)
    print(p3)
}

plot_sanity("annual_yield_mp_DM", "Annual Dry Yield (kg/ha)")
plot_sanity("annual_P_uptake", "Annual P Uptake (kg P/ha)")
plot_sanity("cumulative_P_up", "Cumulative P Uptake (kg P/ha)")
```
""")
