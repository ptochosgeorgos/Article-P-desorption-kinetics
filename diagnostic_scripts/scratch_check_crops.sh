cat << 'EOF' > scratch_check_crops.R
source("notebooks/qi_modelling1.R")

cat("\nCrops per site in D_Yield:\n")
print(table(D_Yield$site, D_Yield$crop))

EOF
Rscript scratch_check_crops.R
