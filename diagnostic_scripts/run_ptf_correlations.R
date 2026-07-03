# Source the Q/I model up to line 453 to define the PTF models and calculate D_Long
lines <- readLines("diagnostic_scripts/temp_qi.R")[1:453]
source(textConnection(lines))

library(dplyr)
library(ggplot2)

iek <- read.csv("data/IEK.csv")
iek_max <- iek %>%
  group_by(uid) %>%
  filter(IEK_time == max(IEK_time, na.rm=TRUE)) %>%
  select(uid, E_exp_max = E_exp, n_iek = n) %>%
  ungroup()
iek_max$uid <- as.character(iek_max$uid)
iek_max$uid <- gsub("Cadenazzo", "CAD", iek_max$uid)
iek_max$uid <- gsub("Ellighausen", "ELL", iek_max$uid)
iek_max$uid <- gsub("Oensingen", "OEN", iek_max$uid)
iek_max$uid <- gsub("Ruemlang", "REH", iek_max$uid)

# D_Long has site, rep, etc. We need to construct uid to match Cadenazzo_P0_1
if ("treatment_ID" %in% names(D_Long)) {
  D_Long$uid <- paste(D_Long$site, D_Long$treatment_ID, D_Long$rep, sep="_")
} else {
  # fallback to treatment_serie if treatment_ID isn't there
  D_Long$uid <- paste(D_Long$site, paste0("P", D_Long$fert_P_tot), D_Long$rep, sep="_")
}


# Deduplicate D_Long for the physical parameters since they are static per plot/year (just take one per uid)
D_plot <- D_Long %>%
  group_by(uid) %>%
  summarise(
    n_freundlich = mean(n_pred_agro, na.rm=TRUE),
    inv_n_freundlich = 1 / mean(n_pred_agro, na.rm=TRUE),
    K_freundlich = mean(exp(ln_K_pred_agro), na.rm=TRUE)
  )

merged <- inner_join(D_plot, iek_max, by="uid")

print(paste("Merged rows:", nrow(merged)))

if(nrow(merged) > 0) {
  p1 <- ggplot(merged, aes(x = K_freundlich, y = E_exp_max)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "red") +
    labs(x = "K (from Agronomic PTF)", y = "E_3m (Max Time IEK)",
         title = paste("Correlation: r =", round(cor(merged$K_freundlich, merged$E_exp_max, use="complete.obs"), 3))) +
    theme_minimal()
  
  p2 <- ggplot(merged, aes(x = n_freundlich, y = n_iek)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "red") +
    labs(x = "n (from Agronomic PTF)", y = "n (IEK Heterogeneity)",
         title = paste("Correlation: r =", round(cor(merged$n_freundlich, merged$n_iek, use="complete.obs"), 3))) +
    theme_minimal()
    
  p3 <- ggplot(merged, aes(x = inv_n_freundlich, y = n_iek)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "red") +
    labs(x = "1/n (from Agronomic PTF)", y = "n (IEK Heterogeneity)",
         title = paste("Correlation: r =", round(cor(merged$inv_n_freundlich, merged$n_iek, use="complete.obs"), 3))) +
    theme_minimal()
  
  ggsave("diagnostic_scripts/iek_vs_ptf_capacity_agro.png", p1, width=6, height=5, bg="white")
  ggsave("diagnostic_scripts/iek_vs_ptf_rate_agro.png", p2, width=6, height=5, bg="white")
  ggsave("diagnostic_scripts/iek_vs_ptf_invrate_agro.png", p3, width=6, height=5, bg="white")
  print("Plots successfully saved to diagnostic_scripts/")
  
  print(paste("Pearson (K vs E_3m):", cor(merged$K_freundlich, merged$E_exp_max, use="complete.obs")))
  print(paste("Pearson (n_PTF vs n_IEK):", cor(merged$n_freundlich, merged$n_iek, use="complete.obs")))
  print(paste("Pearson (1/n_PTF vs n_IEK):", cor(merged$inv_n_freundlich, merged$n_iek, use="complete.obs")))
} else {
  print("Merge failed.")
  print(head(D_plot$uid))
  print(head(iek_max$uid))
}
