library(dplyr)
library(nlme)

load("gapon_aparK.RData")

biological_data <- modelling_data %>%
  filter(!is.na(crop), !is.na(annual_yield_mp_DM), annual_yield_mp_DM > 0, !is.na(z_AR_K), !is.na(z_b), !is.na(site), !is.na(year)) %>%
  mutate(crop = droplevels(factor(crop)))

yield_data <- biological_data 

proxy_A_df <- yield_data %>% group_by(crop) %>% summarize(proxy_A = quantile(annual_yield_mp_DM, 0.90, na.rm=TRUE), .groups='drop')
proxy_Y0_df <- yield_data %>% group_by(crop) %>% filter(z_AR_K <= quantile(z_AR_K, 0.10, na.rm=TRUE)) %>% summarize(proxy_Y0 = mean(annual_yield_mp_DM, na.rm=TRUE), .groups='drop')

data_for_c <- yield_data %>% left_join(proxy_A_df, by="crop") %>% left_join(proxy_Y0_df, by="crop") %>% filter(annual_yield_mp_DM < proxy_A, annual_yield_mp_DM > proxy_Y0) %>% mutate(log_term = log(1 - (annual_yield_mp_DM - proxy_Y0)/(proxy_A - proxy_Y0)))
proxy_c_df <- data_for_c %>% group_by(crop) %>% summarize(proxy_c_I = -coef(lm(log_term ~ z_AR_K + z_b))[2], proxy_c_b = -coef(lm(log_term ~ z_AR_K + z_b))[3], .groups='drop')

start_A <- coef(lm(proxy_A ~ crop, data=proxy_A_df))
start_Y0 <- coef(lm(proxy_Y0 ~ crop, data=proxy_Y0_df))
start_c_I <- coef(lm(proxy_c_I ~ crop, data=proxy_c_df))
start_c_I[is.na(start_c_I)] <- 0.1
start_c_b <- rep(0.1, length(start_c_I)) 

my_starts <- c(start_A, start_Y0, start_c_I, start_c_b)

print(length(my_starts))
print(length(levels(yield_data$crop)))

# Try model.matrix
tryCatch({
    mat <- model.matrix(~ crop, data=yield_data)
    print("model.matrix ~ crop successful")
}, error = function(e) print(e))

