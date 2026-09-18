import re

with open("code_gapon_parallel.qmd", "r") as f:
    content = f.read()

# Replace the complicated dplyr start with a safe zero-contrast start
old_block = """# 1. Automated Starting Values via dplyr
# Proxy A (90th percentile)
proxy_A_df <- yield_data %>% group_by(crop) %>% summarize(proxy_A = quantile(annual_yield_mp_DM, 0.90, na.rm=TRUE), .groups='drop')
# Proxy Y0 (mean of lowest 10% AR_K)
proxy_Y0_df <- yield_data %>% group_by(crop) %>% filter(z_AR_K <= quantile(z_AR_K, 0.10, na.rm=TRUE)) %>% summarize(proxy_Y0 = mean(annual_yield_mp_DM, na.rm=TRUE), .groups='drop')

# Slopes
data_for_c <- yield_data %>% left_join(proxy_A_df, by="crop") %>% left_join(proxy_Y0_df, by="crop") %>% filter(annual_yield_mp_DM < proxy_A, annual_yield_mp_DM > proxy_Y0) %>% mutate(log_term = log(1 - (annual_yield_mp_DM - proxy_Y0)/(proxy_A - proxy_Y0)))
proxy_c_df <- data_for_c %>% group_by(crop) %>% summarize(proxy_c_I = -coef(lm(log_term ~ z_AR_K + z_b))[2], proxy_c_b = -coef(lm(log_term ~ z_AR_K + z_b))[3], .groups='drop')

# Get perfect contrast starting vectors
start_A <- coef(lm(proxy_A ~ crop, data=proxy_A_df))
start_Y0 <- coef(lm(proxy_Y0 ~ crop, data=proxy_Y0_df))
start_c_I <- coef(lm(proxy_c_I ~ crop, data=proxy_c_df))
# Handle NA if regression failed for small subsets (use a generic small value)
start_c_I[is.na(start_c_I)] <- 0.1
start_c_b <- rep(0.1, length(start_c_I)) 

my_starts <- c(start_A, start_Y0, start_c_I, start_c_b)"""

new_block = """# 1. Safe Automated Starting Values
# We initialize the baseline crop with a proxy and assume all other crops have 0 contrast initially.
# nlme will automatically solve the exact contrast differences during optimization.
num_crops <- length(levels(yield_data$crop))

base_A <- quantile(yield_data$annual_yield_mp_DM, 0.90, na.rm=TRUE)
base_Y0 <- quantile(yield_data$annual_yield_mp_DM, 0.10, na.rm=TRUE)

start_A <- c(base_A, rep(0, num_crops - 1))
start_Y0 <- c(base_Y0, rep(0, num_crops - 1))
start_c_I <- c(0.1, rep(0, num_crops - 1))
start_c_b <- c(0.1, rep(0, num_crops - 1))

my_starts <- c(start_A, start_Y0, start_c_I, start_c_b)"""

if old_block in content:
    with open("code_gapon_parallel.qmd", "w") as f:
        f.write(content.replace(old_block, new_block))
    print("Replaced starting values!")
else:
    print("Could not find the block to replace!")
