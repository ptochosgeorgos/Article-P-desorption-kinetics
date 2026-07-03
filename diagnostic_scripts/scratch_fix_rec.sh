cat << 'EOF' > scratch_fix_rec.R
# Load original all_P that has both REC and REH
all_p <- readRDS("data/all_P.rds")

# We want to replace our newly built "REH" with the original "REC" data renamed to "REH"
# because REC spans 1990-2024!
all_p_clean <- all_p |> dplyr::filter(site != "REH")
all_p_clean$site[all_p_clean$site == "REC"] <- "REH"

saveRDS(all_p_clean, "data/all_P.rds")
cat("Restored Reckenholz climate data to 1990-2024 using the 'REC' block!\n")
EOF
Rscript scratch_fix_rec.R
