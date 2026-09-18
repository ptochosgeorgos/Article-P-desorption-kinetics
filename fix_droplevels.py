with open("code_gapon_parallel.qmd", "r") as f:
    content = f.read()

# Add droplevels(yield_data) right before num_crops
content = content.replace("num_crops <- length(levels(yield_data$crop))", "yield_data <- droplevels(yield_data)\nnum_crops <- length(levels(yield_data$crop))")

with open("code_gapon_parallel.qmd", "w") as f:
    f.write(content)
