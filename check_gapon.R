lines_gapon <- readLines("code_gapon.qmd")
start_modelling <- grep("modelling_data <- ", lines_gapon)[1]
cat(lines_gapon[start_modelling:(start_modelling+20)], sep="\n")
