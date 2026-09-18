with open("code_gapon_parallel.qmd", "r") as f:
    lines = f.readlines()

new_lines = []
skip = False
for line in lines:
    if "paste0('Y_0.', crop_cols)," in line or "'c_I'," in line or "'c_b'" in line or (line.strip() == ")" and skip):
        continue
    new_lines.append(line)

with open("code_gapon_parallel.qmd", "w") as f:
    f.writelines(new_lines)
