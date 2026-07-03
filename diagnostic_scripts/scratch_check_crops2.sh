cat << 'EOF' > scratch_check_crops2.R
D2 <- readxl::read_excel("data/STYCS_data_2023_260511.xlsx")
D2$site <- gsub("STYCS_", "", D2$LtE_name)
cat("Crops per site:\n")
print(table(D2$site, D2$crop_abr))
EOF
Rscript scratch_check_crops2.R
