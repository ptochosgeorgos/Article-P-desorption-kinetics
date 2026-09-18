with open("code_gapon_parallel.qmd", "r") as f:
    content = f.read()

import re

# Replace the block that creates dummy variables and my_fixed
old_block = """dummy_mat <- model.matrix(~ 0 + crop, yield_data)
yield_data <- cbind(yield_data, as.data.frame(dummy_mat))
crop_cols <- colnames(dummy_mat)
fixed_f <- paste(crop_cols, collapse = ' + ')
my_fixed <- list(
  as.formula(paste('A ~', fixed_f, '- 1')),
  as.formula(paste('Y_0 ~', fixed_f, '- 1')),
  as.formula(paste('c_I ~', fixed_f, '- 1')),
  as.formula(paste('c_b ~', fixed_f, '- 1'))
)"""

new_block = """dummy_mat <- model.matrix(~ 0 + crop, yield_data)
colnames(dummy_mat) <- paste0("c", 1:ncol(dummy_mat))
yield_data <- cbind(yield_data, as.data.frame(dummy_mat))
crop_cols <- colnames(dummy_mat)
fixed_f <- paste(crop_cols, collapse = ' + ')
my_fixed <- list(
  as.formula(paste('A ~', fixed_f, '- 1')),
  as.formula(paste('Y_0 ~', fixed_f, '- 1')),
  as.formula(paste('c_I ~', fixed_f, '- 1')),
  as.formula(paste('c_b ~', fixed_f, '- 1'))
)"""

content = content.replace(old_block, new_block)

# Also we need to make sure we remove the explicitly named my_starts from previous fix
content = re.sub(r'names\(my_starts\) <- c\([^)]+\)\n', '', content)

with open("code_gapon_parallel.qmd", "w") as f:
    f.write(content)
