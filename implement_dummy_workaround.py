with open("code_gapon_parallel.qmd", "r") as f:
    lines = f.readlines()

new_lines = []
in_chunk_12 = False
for line in lines:
    if "num_crops <- length(levels(yield_data$crop))" in line:
        new_lines.append(line)
        new_lines.append("dummy_mat <- model.matrix(~ 0 + crop, yield_data)\n")
        new_lines.append("yield_data <- cbind(yield_data, as.data.frame(dummy_mat))\n")
        new_lines.append("crop_cols <- colnames(dummy_mat)\n")
        new_lines.append("fixed_f <- paste(crop_cols, collapse = ' + ')\n")
        new_lines.append("my_fixed <- list(\n")
        new_lines.append("  as.formula(paste('A ~', fixed_f, '- 1')),\n")
        new_lines.append("  as.formula(paste('Y_0 ~', fixed_f, '- 1')),\n")
        new_lines.append("  as.formula(paste('c_I ~', fixed_f, '- 1')),\n")
        new_lines.append("  as.formula(paste('c_b ~', fixed_f, '- 1'))\n")
        new_lines.append(")\n")
        continue

    if "fixed = list(A ~ 0 + crop, Y_0 ~ 0 + crop, c_I ~ 0 + crop, c_b ~ 0 + crop)" in line:
        new_lines.append("  fixed = my_fixed,\n")
        continue

    # Fix coefficient extraction
    if "A_ref <- mean(coefs_y[grepl(\"A.crop\", names(coefs_y))])" in line:
        new_lines.append("A_ref <- mean(coefs_y[grepl(\"^A\\\\.crop\", names(coefs_y))])\n")
        continue
    if "Y_0_ref <- mean(coefs_y[grepl(\"Y_0.crop\", names(coefs_y))])" in line:
        new_lines.append("Y_0_ref <- mean(coefs_y[grepl(\"^Y_0\\\\.crop\", names(coefs_y))])\n")
        continue
    if "c_I_ref <- mean(coefs_y[grepl(\"c_I.crop\", names(coefs_y))])" in line:
        new_lines.append("c_I_ref <- mean(coefs_y[grepl(\"^c_I\\\\.crop\", names(coefs_y))])\n")
        continue
    if "c_b_ref <- mean(coefs_y[grepl(\"c_b.crop\", names(coefs_y))])" in line:
        new_lines.append("c_b_ref <- mean(coefs_y[grepl(\"^c_b\\\\.crop\", names(coefs_y))])\n")
        continue

    new_lines.append(line)

with open("code_gapon_parallel.qmd", "w") as f:
    f.writelines(new_lines)
