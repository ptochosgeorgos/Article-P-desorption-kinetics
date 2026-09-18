#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false
# Calculate the Activity Ratio (Intensity proxy)
# Ensure units are chemically consistent (e.g., mmol/L or equivalent activities)
df$AR_K <- df$K_CO2 / sqrt(df$Ca_CO2 + df$Mg_CO2)
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false
# Calculate remaining permanent capacity (Quantity proxy)
df$CEC_rest <- (df$Ca_AAE10 / 20.04) + (df$Mg_AAE10 / 12.15)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false
library(lme4)

# The Full Mechanistic Model
m_K_final <- lmer(
    log(K_AAE10) ~
        # 1. The Slope Modifier (Buffer Power driven by specific clay sites)
        log(AR_K) * clay +

        # 2. The Intercept Modifiers (Baseline CEC capacity)
        log(CEC_rest) + log(Corg) * pH +

        # 3. The Random Effects (Crossed structure for long-term permanent plots)
        (1 | plot_nr) + (1 | year),
    data = df
)

summary(m_K_final)
#
#
#
#
#
#
#
#| eval: false
library(emmeans)

# Calculate percentiles for clay
clay_q <- quantile(df$clay, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)

# Calculate estimated marginal trends for the slope
k_trends <- emtrends(
    m_K_final,
    ~clay,
    var = "log(AR_K)",
    at = list(clay = clay_q)
)

print(k_trends)
#
#
#
#
