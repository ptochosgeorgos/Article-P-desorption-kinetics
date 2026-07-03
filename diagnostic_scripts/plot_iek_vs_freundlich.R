library(dplyr)
library(ggplot2)

res_raw <- readRDS("data/RES.rds")
iek <- read.csv("data/IEK.csv")

# Extract the coefficients from the RES.rds list
if ("nlme.coef.avg" %in% names(res_raw)) {
  res <- res_raw$nlme.coef.avg
  res$uid <- rownames(res)
  
  # The parameters are usually called P_desorb and k or similar.
  # Let's check what they are actually named.
  print("Coefficient names:")
  print(names(res))
  
  # Map names if they are different
  if ("PS" %in% names(res) && !("P_desorb" %in% names(res))) res$P_desorb <- res$PS
  if ("k" %in% names(res) && !("k" %in% names(res))) res$k <- res$k
  
  # Ensure they are numeric
  if ("P_desorb" %in% names(res)) res$P_desorb <- as.numeric(res$P_desorb)
  if ("k" %in% names(res)) res$k <- as.numeric(res$k)
  
} else {
  print("nlme.coef.avg not found in RES.rds")
  res <- data.frame(uid = character(), P_desorb = numeric(), k = numeric())
}

print("Head of res (coefficients):")
print(head(res))

iek_max <- iek %>%
  group_by(uid) %>%
  filter(IEK_time == max(IEK_time, na.rm=TRUE)) %>%
  select(uid, E_exp_max = E_exp, n_iek = n) %>%
  ungroup()

# Some IEK files have space or formatting differences
iek_max$uid <- as.character(iek_max$uid)

if ("uid" %in% names(res)) {
  # RES uses dots (e.g., Cadenazzo.P0.1), IEK uses underscores (Cadenazzo_P0_1)
  res$uid <- gsub("\\.", "_", as.character(res$uid))
  
  merged <- inner_join(res, iek_max, by="uid")
  
  print(paste("Merged rows:", nrow(merged)))
  
  if(nrow(merged) > 0) {
    p1 <- ggplot(merged, aes(x = P_desorb, y = E_exp_max)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", color = "red") +
      labs(x = "P_desorb (Kinetic Capacity K)", y = "E_exp (Long-term IEK)",
           title = paste("Correlation: r =", round(cor(merged$P_desorb, merged$E_exp_max, use="complete.obs"), 3))) +
      theme_minimal()
    
    p2 <- ggplot(merged, aes(x = k, y = n_iek)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", color = "red") +
      labs(x = "k (Kinetic Rate / n_freundlich)", y = "n_iek (IEK Heterogeneity)",
           title = paste("Correlation: r =", round(cor(merged$k, merged$n_iek, use="complete.obs"), 3))) +
      theme_minimal()
    
    ggsave("diagnostic_scripts/iek_vs_kinetic_capacity.png", p1, width=6, height=5, bg="white")
    ggsave("diagnostic_scripts/iek_vs_kinetic_rate.png", p2, width=6, height=5, bg="white")
    print("Plots successfully saved to diagnostic_scripts/iek_vs_kinetic_capacity.png and iek_vs_kinetic_rate.png")
    
    # Print the correlation explicitly
    print(paste("Pearson correlation (P_desorb vs E_exp):", cor(merged$P_desorb, merged$E_exp_max, use="complete.obs")))
    print(paste("Pearson correlation (k vs n_iek):", cor(merged$k, merged$n_iek, use="complete.obs")))
  } else {
    print("Merge failed. UIDs do not match.")
    print("RES uids:")
    print(head(res$uid))
    print("IEK uids:")
    print(head(iek_max$uid))
  }
} else {
  print("RES.rds does not have a 'uid' column. Available columns:")
  print(names(res))
}
