with open("code_gapon_parallel.qmd", "r") as f:
    content = f.read()

# Fix the starts to be absolute values for all crops (no contrasts)
content = content.replace("start_A <- c(base_A, rep(0, num_crops - 1))", "start_A <- rep(base_A, num_crops)")
content = content.replace("start_Y0 <- c(base_Y0, rep(0, num_crops - 1))", "start_Y0 <- rep(base_Y0, num_crops)")
content = content.replace("start_c_I <- c(0.1, rep(0, num_crops - 1))", "start_c_I <- rep(0.1, num_crops)")
content = content.replace("start_c_b <- c(0.1, rep(0, num_crops - 1))", "start_c_b <- rep(0.1, num_crops)")

# Fix the fixed formula to use ~ 0 + crop (no intercept, avoids contrast evaluation on single-crop groups)
content = content.replace("fixed = list(A ~ crop, Y_0 ~ crop, c_I ~ crop, c_b ~ crop),", "fixed = list(A ~ 0 + crop, Y_0 ~ 0 + crop, c_I ~ 0 + crop, c_b ~ 0 + crop),")

# Fix the agronomic application back-transformation (it now extracts the absolute values directly, no intercept logic needed)
# Previously: A_ref <- coefs_y["A.(Intercept)"]
# Now: A_ref <- coefs_y[1] # first crop is fine for reference, or just average them. We'll use the mean of all crop baselines for the plot.
new_extract = """# Extract fixed coefficients for the baseline (reference) crop
# We use the mean across all crops as a robust global reference
coefs_y <- fixef(m_nlme_yield)
A_ref <- mean(coefs_y[grepl("A.crop", names(coefs_y))])
Y_0_ref <- mean(coefs_y[grepl("Y_0.crop", names(coefs_y))])
c_I_ref <- mean(coefs_y[grepl("c_I.crop", names(coefs_y))])
c_b_ref <- mean(coefs_y[grepl("c_b.crop", names(coefs_y))])"""

import re
content = re.sub(r"# Extract fixed coefficients for the baseline \(reference\) crop.*?c_b_ref <- coefs_y\[\"c_b\.\(Intercept\)\"\]", new_extract, content, flags=re.DOTALL)


with open("code_gapon_parallel.qmd", "w") as f:
    f.write(content)
