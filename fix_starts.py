with open("code_gapon_parallel.qmd", "r") as f:
    lines = f.readlines()

new_lines = []
for line in lines:
    if "my_starts <- c(start_A, start_Y0, start_c_I, start_c_b)" in line:
        new_lines.append(line)
        new_lines.append("names(my_starts) <- c(\n")
        new_lines.append("  paste0('A.', crop_cols),\n")
        new_lines.append("  paste0('Y_0.', crop_cols),\n")
        new_lines.append("  paste0('c_I.', crop_cols),\n")
        new_lines.append("  paste0('c_b.', crop_cols)\n")
        new_lines.append(")\n")
    else:
        new_lines.append(line)

with open("code_gapon_parallel.qmd", "w") as f:
    f.writelines(new_lines)
